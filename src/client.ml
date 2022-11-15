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
  "TCP server host (default none)" ;
  "--port",Arg.Set_int port,
  "TCP server port (default 5555)" ;
  "--server",Arg.Set_string server,
  "URL proof server address (default \"tcp://host:port\", if any)";
  "--polling",Arg.Set_float polling,
  "TIME server polling interval (default 1.0s)";
  "--trace",Arg.Set trace,"Trace server protocol";
]

type goal = { prover : string ; digest : string }
type task = {
  goal : goal ;
  task : Runner.prooftask ;
  mutable timeout : float ;
  result : Runner.result Fibers.var ;
}

type client = {
  env : Wenv.env ;
  socket : [`Dealer] Zmq.Socket.t ;
  profile : Calibration.profile ;
  pending : (goal,task) Hashtbl.t ;
}

(* -------------------------------------------------------------------------- *)
(* --- Connection                                                         --- *)
(* -------------------------------------------------------------------------- *)

let connect env =
  if !server = "" then None else
  if !host = "" then None else
    let address =
      if !server = "" then Printf.sprintf "tcp://%s:%d" !host !port
      else !server in
    let context = Zmq.Context.create () in
    let socket = Zmq.Socket.(create context dealer) in
    let profile = Calibration.create () in
    let pending = Hashtbl.create 0 in
    Zmq.Socket.connect socket address ;
    Some { env ; socket ; profile ; pending }

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
    fn msg ;
    true
  with Unix.Unix_error(EAGAIN,_,_) -> false
[@@ warning "-32"]

(* -------------------------------------------------------------------------- *)
(* --- Messages Sent                                                      --- *)
(* -------------------------------------------------------------------------- *)

let send_profile client prover (size,time) =
  send client
    ["PROFILE";Runner.id prover;string_of_int size;string_of_float time]

let send_get client goal timeout =
  send client ["GET";goal.prover;goal.digest;string_of_float timeout]

let send_kill client goal =
  send client ["KILL";goal.prover;goal.digest]
[@@ warning "-32"]

(* -------------------------------------------------------------------------- *)
(* --- PROVE                                                              --- *)
(* -------------------------------------------------------------------------- *)

let get_task client prover task =
  let goal = { prover = Runner.id prover ; digest = Runner.digest task } in
  try Hashtbl.find client.pending goal with Not_found ->
    let task = { goal ; task ; timeout = 0.0 ; result = Fibers.var () } in
    Hashtbl.add client.pending goal task ; task

let prove_client client profile prover task timeout =
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
  let timeout = timeout /. gamma in
  let task = get_task client prover task in
  if 0.0 < task.timeout && task.timeout < timeout then
    begin
      task.timeout <- timeout ;
      send_get client task.goal timeout ;
    end ;
  Fibers.map (Runner.map (( *. ) gamma)) @@ Fibers.get task.result

(* -------------------------------------------------------------------------- *)
