(**************************************************************************)
(*                                                                        *)
(*  This file is part of the why3find.                                    *)
(*                                                                        *)
(*  Copyright (C) 2022-2024                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the enclosed LICENSE.md for details.                              *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Proof Client                                                       --- *)
(* -------------------------------------------------------------------------- *)

let host = ref ""
let port = ref 5555
let server = ref ""
let polling = ref 1.0
let trace = ref false

let options = [
  "--host",Arg.Set_string host,
  "HOST Proof Server host (default: none)" ;
  "--port",Arg.Set_int port,
  "PORT Proof Server port (default: 5555)" ;
  "--server",Arg.Set_string server,
  "URL Proof Server address (default: \"tcp://HOST:PORT\")" ;
  "--polling",Arg.Set_float polling,
  "TIME server polling interval (default 1.0s)";
  "--trace",Arg.Set trace,"Trace server protocol";
]

type goal = { prover : Prover.prover_desc ; digest : string }
type task = {
  goal : goal ;
  prv : Prover.prover ;
  tsk : Runner.prooftask ;
  mutable timeout : float ;
  channel : Runner.result Fibers.signal ;
}

type client = {
  env : Wenv.env ;
  context : Zmq.Context.t ;
  socket : [`Dealer] Zmq.Socket.t ;
  profile : Calibration.profile ;
  pending : (goal,task) Hashtbl.t ;
  mutable terminated : bool ;
}

(* -------------------------------------------------------------------------- *)
(* --- Connection                                                         --- *)
(* -------------------------------------------------------------------------- *)

let resolve () =
  if !server <> "" then Some !server else
  if !host <> "" then Some (Printf.sprintf "tcp://%s:%d" !host !port) else
    None

let connect env =
  Option.map
    begin fun address ->
      let context = Zmq.Context.create () in
      let socket = Zmq.Socket.(create context dealer) in
      let profile = Calibration.create () in
      let pending = Hashtbl.create 0 in
      Zmq.Socket.connect socket address ;
      Log.emit "Server %s" address ;
      { env ; context ; socket ; profile ; pending ; terminated = false }
    end @@ resolve ()

(* -------------------------------------------------------------------------- *)
(* --- Socket                                                             --- *)
(* -------------------------------------------------------------------------- *)

let send client msg =
  if !trace then
    Log.emit "SEND -> %a" Utils.pp_args msg ;
  Zmq.Socket.send_all client.socket msg

let recv client fn =
  try
    let msg = Zmq.Socket.recv_all ~block:false client.socket in
    if !trace then
      Log.emit "<- RECV %a" Utils.pp_args msg ;
    fn msg
  with Unix.Unix_error(EAGAIN,_,_) -> ()

(* -------------------------------------------------------------------------- *)
(* --- Messages Sent                                                      --- *)
(* -------------------------------------------------------------------------- *)

let send_profile client prover (size,time) =
  let prover = Prover.desc_to_string prover.Prover.desc in
  send client
    ["PROFILE";prover;string_of_int size;string_of_float time]

let send_get client goal timeout =
  let prover = Prover.desc_to_string goal.prover in
  send client ["GET";prover;goal.digest;string_of_float timeout]

let send_upload client goal data =
  let prover = Prover.desc_to_string goal.prover in
  send client ["UPLOAD";prover;goal.digest;data]

let send_kill client goal =
  let prover = Prover.desc_to_string goal.prover in
  send client ["KILL";prover;goal.digest]
[@@ warning "-32"]

(* -------------------------------------------------------------------------- *)
(* --- PROVE                                                              --- *)
(* -------------------------------------------------------------------------- *)

let get_task client prv tsk =
  let goal = { prover = prv.Prover.desc ; digest = Runner.digest tsk } in
  try Hashtbl.find client.pending goal with Not_found ->
    let channel = Fibers.signal () in
    let task = { goal ; prv ; tsk ; timeout = 0.0 ; channel } in
    Hashtbl.add client.pending goal task ; task

let request client profile prover task timeout =
  let open Fibers.Monad in
  let env = client.env in
  let task = get_task client prover task in
  Fibers.background
    begin
      (* project profile *)
      let* project = Calibration.profile env profile prover in
      if Calibration.lock client.profile prover.desc then
        send_profile client prover project ;
      (* server profile *)
      let* server = Calibration.profile env client.profile prover in
      (* t-project / t-server *)
      let* gamma =
        let (np,tp) = project in
        let (ns,ts) = server in
        if np = ns then Fibers.return @@ tp /. ts else
          (* vp = tp / tr ; vs = tr / ts *)
          let+ vp = Calibration.velocity env profile prover
          and* vs = Calibration.velocity env client.profile prover
          in vs /. vp in
      (* server timeout *)
      let time = timeout /. gamma in
      if task.timeout < time then
        begin
          task.timeout <- time ;
          send_get client task.goal time ;
        end ;
      Fibers.return ()
    end ;
  task.channel

(* -------------------------------------------------------------------------- *)
(* --- CALIBRATION                                                        --- *)
(* -------------------------------------------------------------------------- *)

let do_profile client prover size time =
  Calibration.set client.profile prover size time

(* -------------------------------------------------------------------------- *)
(* --- RESULT                                                             --- *)
(* -------------------------------------------------------------------------- *)

let do_result client goal status time =
  let task = Hashtbl.find client.pending goal in
  let result =
    match status with
    | "NoResult" -> Runner.NoResult
    | "Valid" -> Runner.Valid time
    | "Unknown" -> Runner.Unknown time
    | "Timeout" -> Runner.Timeout time
    | _ -> Runner.Failed
  in
  Fibers.background
    ~callback:(fun alpha ->
        Runner.update task.prv task.tsk @@
        Runner.map (fun t -> t *. alpha) result
      ) (Calibration.velocity client.env client.profile task.prv) ;
  if Runner.definitive ~timeout:task.timeout result then
    Hashtbl.remove client.pending task.goal ;
  Fibers.emit task.channel result

(* -------------------------------------------------------------------------- *)
(* --- DOWNLOAD                                                           --- *)
(* -------------------------------------------------------------------------- *)

let do_download client goal =
  let task = Hashtbl.find client.pending goal in
  send_upload client goal (Runner.data task.prv task.tsk)

(* -------------------------------------------------------------------------- *)
(* --- SERVER Handler                                                     --- *)
(* -------------------------------------------------------------------------- *)

let handler client msg =
  try match msg with
    | ["PROFILE";prover;size;time] ->
      let prover = Prover.desc_of_string prover in
      do_profile client prover (int_of_string size) (float_of_string time)
    | ["RESULT";prover;digest;status;time] ->
      let prover = Prover.desc_of_string prover in
      do_result client { prover ; digest } status (float_of_string time)
    | ["DOWNLOAD";prover;digest] ->
      let prover = Prover.desc_of_string prover in
      do_download client { prover ; digest }
    | _ -> ()
  with Not_found | Invalid_argument _ | Prover.InvalidProverDescription _ -> ()

(* -------------------------------------------------------------------------- *)
(* --- Running                                                            --- *)
(* -------------------------------------------------------------------------- *)

let yield client =
  try recv client (handler client)
  with exn ->
    Log.emit "Client Error (%s)" @@ Printexc.to_string exn

let terminate client =
  try
    send client ["HANGUP"] ;
    Zmq.Socket.close client.socket ;
    Zmq.Context.terminate client.context ;
  with exn ->
    Log.emit "Client Killed (%s)" @@ Printexc.to_string exn

(* -------------------------------------------------------------------------- *)
