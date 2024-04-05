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
(* --- Worker                                                             --- *)
(* -------------------------------------------------------------------------- *)

(* -------------------------------------------------------------------------- *)
(* --- Proof Task                                                         --- *)
(* -------------------------------------------------------------------------- *)

type goal = { prover : Prover.prover_desc ; digest : string }

(* -------------------------------------------------------------------------- *)
(* --- Worker State                                                       --- *)
(* -------------------------------------------------------------------------- *)

type worker = {
  env : Wenv.env ;
  context : Zmq.Context.t ;
  timeout : int ;
  address : string ;
  mutable hangup : int ;
  mutable socket : [ `Dealer ] Zmq.Socket.t ;
  mutable polling : Zmq.Poll.t ;
  maxjobs : int ;
  provers : Prover.prover list ;
  profile : Calibration.profile ;
  pending : (goal,unit Fibers.signal) Hashtbl.t ;
}

(* -------------------------------------------------------------------------- *)
(* --- Socket                                                             --- *)
(* -------------------------------------------------------------------------- *)

let trace = ref false

let send worker msg =
  if !trace then
    Utils.log "SEND -> %a@." Utils.pp_args msg ;
  Zmq.Socket.send_all worker.socket msg

let recv worker fn =
  try
    let msg = Zmq.Socket.recv_all ~block:false worker.socket in
    if !trace then
      Utils.log "<- RECV %a@." Utils.pp_args msg ;
    fn msg ;
    true
  with Unix.Unix_error(EAGAIN,_,_) -> false

(* -------------------------------------------------------------------------- *)
(* --- READY                                                              --- *)
(* -------------------------------------------------------------------------- *)

let send_ready worker =
  let provers =
    List.map (fun p -> Prover.desc_to_string p.Prover.desc) worker.provers in
  send worker ("READY"::string_of_int worker.maxjobs::provers)

(* -------------------------------------------------------------------------- *)
(* --- CALIBRATION                                                        --- *)
(* -------------------------------------------------------------------------- *)

let send_profile worker prv =
  send worker ["PROFILE";Prover.desc_to_string prv]

let recv_profile worker prv size time =
  Calibration.set worker.profile prv size time

(* -------------------------------------------------------------------------- *)
(* --- KILL                                                               --- *)
(* -------------------------------------------------------------------------- *)

let do_kill worker goal =
  try
    let cancel = Hashtbl.find worker.pending goal in
    Hashtbl.remove worker.pending goal ;
    Fibers.emit cancel ()
  with Not_found -> ()

(* -------------------------------------------------------------------------- *)
(* --- RESULT                                                             --- *)
(* -------------------------------------------------------------------------- *)

let send_result worker goal (alpha,result) =
  let status,time = match result with
    | Runner.NoResult -> "NoResult",0.0
    | Runner.Failed -> "Failed",0.0
    | Runner.Valid t -> "Valid",t
    | Runner.Unknown t -> "Unknown",t
    | Runner.Timeout t -> "Timeout",t
  in
  let prover = Prover.desc_to_string goal.prover in
  send worker
    ["RESULT";prover;goal.digest;status;string_of_float @@ time /. alpha]

(* -------------------------------------------------------------------------- *)
(* --- PROVE                                                              --- *)
(* -------------------------------------------------------------------------- *)

let do_prove worker goal timeout data =
  do_kill worker goal ;
  try
    let env = worker.env in
    let cancel = Fibers.signal () in
    let prover = Prover.prover env goal.prover in
    let buffer = Buffer.create (String.length data) in
    Buffer.add_string buffer data ;
    Hashtbl.add worker.pending goal cancel ;
    if Calibration.lock worker.profile goal.prover then
      send_profile worker goal.prover ;
    Fibers.background ~callback:(send_result worker goal)
      begin
        let open Fibers.Monad in
        let* alpha = Calibration.velocity env worker.profile prover in
        let timeout = alpha *. timeout in
        let* result = Runner.prove_buffer env ~cancel prover buffer timeout in
        Hashtbl.remove worker.pending goal ;
        Fibers.return (alpha,result)
      end
  with Not_found -> ()

(* -------------------------------------------------------------------------- *)
(* --- Message Handler                                                    --- *)
(* -------------------------------------------------------------------------- *)

let handler worker msg =
  try
    match msg with
    | ["RAISE"] ->
      send_ready worker
    | ["PROFILE";prv;size;time] ->
      let prv = Prover.desc_of_string prv in
      recv_profile worker prv (int_of_string size) (float_of_string time)
    | ["KILL";prover;digest] ->
      let prover = Prover.desc_of_string prover in
      do_kill worker { prover ; digest }
    | ["PROVE";prover;digest;timeout;data] ->
      let prover = Prover.desc_of_string prover in
      do_prove worker { prover ; digest } (float_of_string timeout) data
    | _ -> ()
  with Invalid_argument _ | Prover.InvalidProverDescription _ -> ()

(* -------------------------------------------------------------------------- *)
(* --- Worker Lifecycle                                                   --- *)
(* -------------------------------------------------------------------------- *)

let connect context address =
  let socket = Zmq.Socket.(create context dealer) in
  let polling = Zmq.Poll.(mask_of [| socket , In |]) in
  Zmq.Socket.set_linger_period socket 0 ;
  Zmq.Socket.connect socket address ;
  Zmq.Socket.set_reconnect_interval_max socket 20 ;
  socket , polling

let poll worker =
  if Fibers.pending () > 0 then
    (Fibers.yield () ; Unix.sleepf 0.001)
  else
    let mask = Zmq.Poll.poll ~timeout:worker.timeout worker.polling in
    if Array.for_all (fun evt -> evt = None) mask then
      begin
        let hangup = worker.hangup in
        worker.hangup <- succ hangup ;
        if hangup > 5 then
          begin
            Utils.log "Reconnect to %s…@." worker.address ;
            Zmq.Socket.disconnect worker.socket worker.address ;
            Zmq.Socket.close worker.socket ;
            let socket,polling = connect worker.context worker.address in
            worker.socket <- socket ;
            worker.polling <- polling ;
          end ;
        if hangup > 10 then
          begin
            Zmq.Socket.close worker.socket ;
            Zmq.Context.terminate worker.context ;
            Utils.log "Abandon!@." ;
            exit 1
          end
      end

let flush worker handler =
  while recv worker handler do worker.hangup <- 0 ; Fibers.yield () done

(* -------------------------------------------------------------------------- *)
(* --- Worker Main Loop                                                   --- *)
(* -------------------------------------------------------------------------- *)

let connect ~server ~polling =
  let env = Wenv.init () in
  let prvs = Prover.all env in
  let jobs = Runner.maxjobs env in
  Utils.flush () ;
  List.iter (Format.printf "Prover %a@." Prover.pp_prover) prvs ;
  Format.printf "Server %s@." server ;
  let context = Zmq.Context.create () in
  let timeout = int_of_float (polling *. 1e3) in
  let socket,polling = connect context server in
  let profile = Calibration.create () in
  let worker = {
    env ;
    context ;
    timeout ;
    address = server ;
    socket ;
    polling ;
    hangup = 0 ;
    provers = prvs ;
    maxjobs = jobs ;
    profile ;
    pending = Hashtbl.create 0 ;
  } in
  Format.printf "Worker running…@." ;
  send_ready worker ;
  try
    Sys.catch_break true ;
    while true do
      poll worker ;
      flush worker (handler worker) ;
      let busy = Runner.running () in
      let over = Runner.pending () in
      let hangup = worker.hangup in
      Utils.progress "%d/%d%t" busy jobs
        (fun fmt ->
           if hangup > 0 then Format.fprintf fmt " hangup:%d" hangup ;
           if over > 0 then Format.fprintf fmt " overload:%d" over ;
        ) ;
    done
  with Sys.Break ->
    send worker ["HANGUP"] ;
    Zmq.Socket.disconnect worker.socket server ;
    Zmq.Socket.close worker.socket ;
    Zmq.Context.terminate worker.context ;
    Utils.log "Terminated@." ;
    exit 0


(* -------------------------------------------------------------------------- *)
