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

type goal = { prover : string ; digest : string }
type task = {
  goal : goal ;
  prv : Runner.prover ;
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
      Utils.flush () ;
      Format.printf "Server %s@." address ;
      { env ; context ; socket ; profile ; pending ; terminated = false }
    end @@ resolve ()

(* -------------------------------------------------------------------------- *)
(* --- Socket                                                             --- *)
(* -------------------------------------------------------------------------- *)

let trace = ref false

let send client msg =
  if !trace then
    begin
      Utils.flush () ;
      Format.printf "SEND %a@." Utils.pp_args msg ;
    end ;
  Zmq.Socket.send_all client.socket msg

let recv client fn =
  try
    let msg = Zmq.Socket.recv_all ~block:false client.socket in
    if !trace then
      begin
        Utils.flush () ;
        Format.printf "RECV %a@." Utils.pp_args msg ;
      end ;
    fn msg
  with Unix.Unix_error(EAGAIN,_,_) -> ()

(* -------------------------------------------------------------------------- *)
(* --- Messages Sent                                                      --- *)
(* -------------------------------------------------------------------------- *)

let send_profile client prover (size,time) =
  send client
    ["PROFILE";Runner.id prover;string_of_int size;string_of_float time]

let send_get client goal timeout =
  send client ["GET";goal.prover;goal.digest;string_of_float timeout]

let send_upload client goal data =
  send client ["UPLOAD";goal.prover;goal.digest;data]

let send_kill client goal =
  send client ["KILL";goal.prover;goal.digest]

(* -------------------------------------------------------------------------- *)
(* --- PROVE                                                              --- *)
(* -------------------------------------------------------------------------- *)

let get_task client prv tsk =
  let goal = { prover = Runner.id prv ; digest = Runner.digest tsk } in
  try Hashtbl.find client.pending goal with Not_found ->
    let channel = Fibers.signal () in
    let task = { goal ; prv ; tsk ; timeout = 0.0 ; channel } in
    Hashtbl.add client.pending goal task ; task

let prove client profile ?cancel prover task timeout =
  let open Fibers.Monad in
  let env = client.env in
  (* project profile *)
  let* alpha = Calibration.profile env profile prover in
  if Calibration.lock client.profile (Runner.id prover) then
    send_profile client prover alpha ;
  (* server profile *)
  let* beta = Calibration.profile env client.profile prover in
  (* t-project / t-server *)
  let* gamma =
    let (np,tp) = alpha in
    let (ns,ts) = beta in
    if np = ns then Fibers.return @@ tp /. ts else
      (* vp = tp / tr ; vs = tr / ts *)
      let+ vp = Calibration.velocity env profile prover
      and* vs = Calibration.velocity env client.profile prover
      in vs /. vp in
  (* server timeout *)
  let time = timeout /. gamma in
  let task = get_task client prover task in
  if 0.0 < task.timeout && task.timeout < time then
    begin
      task.timeout <- time ;
      send_get client task.goal time ;
    end ;
  let result = Fibers.var () in
  let resolve rs =
    let rp = Runner.map (fun t -> t *. gamma) rs in
    match Runner.crop ~timeout rp with
    | None -> send_get client task.goal task.timeout
    | Some rp -> Fibers.set result rp
  in
  let interrupt () =
    send_kill client task.goal ;
    Fibers.set result NoResult
  in
  Fibers.hook ~signal:task.channel ~handler:resolve @@
  Fibers.hook ?signal:cancel ~handler:interrupt @@
  Fibers.get result

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
  Fibers.emit task.channel @@
  match status with
  | "NoResult" -> Runner.NoResult
  | "Valid" -> Runner.Valid time
  | "Unknown" -> Runner.Unknown time
  | "Timeout" -> Runner.Timeout time
  | _ -> Runner.Failed

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
      do_profile client prover (int_of_string size) (float_of_string time)
    | ["RESULT";prover;digest;status;time] ->
      do_result client { prover ; digest } status (float_of_string time)
    | ["DOWNLOAD";prover;digest] ->
      do_download client { prover ; digest }
    | _ -> ()
  with Not_found | Invalid_argument _ -> ()

(* -------------------------------------------------------------------------- *)
(* --- Running                                                            --- *)
(* -------------------------------------------------------------------------- *)

let yield client =
  recv client (handler client)

let terminate client =
  begin
    send client ["HANGUP"] ;
    Zmq.Socket.close client.socket ;
    Zmq.Context.terminate client.context ;
  end

(* -------------------------------------------------------------------------- *)
