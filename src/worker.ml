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
(* --- Proof Worker                                                       --- *)
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
  provers : string list ;
  runner : Calibration.profile ;
  server : Calibration.profile ;
}

(* -------------------------------------------------------------------------- *)
(* --- Socket                                                             --- *)
(* -------------------------------------------------------------------------- *)

let trace = ref false
let chrono = ref @@ Unix.time ()

let send worker msg =
  if !trace then
    begin
      Utils.flush () ;
      Format.printf "SEND %a@." Utils.pp_args msg ;
    end ;
  Zmq.Socket.send_all worker.socket msg

let recv worker fn =
  try
    let msg = Zmq.Socket.recv_all ~block:false worker.socket in
    if !trace then
      begin
        Utils.flush () ;
        let time = Unix.time () in
        let delta = time -. !chrono in
        Format.printf "RECV %a (%a)@."
          Utils.pp_args msg
          Utils.pp_time delta ;
        chrono := time ;
      end ;
    fn msg ;
    true
  with Unix.Unix_error(EAGAIN,_,_) -> false

(* -------------------------------------------------------------------------- *)
(* --- READY                                                              --- *)
(* -------------------------------------------------------------------------- *)

let send_ready worker =
  send worker ("READY"::string_of_int worker.maxjobs::worker.provers)

(* -------------------------------------------------------------------------- *)
(* --- CALIBRATION                                                        --- *)
(* -------------------------------------------------------------------------- *)

let send_profile worker prv size time =
  send worker ["PROFILE";prv;string_of_int size;string_of_float time]
[@@ warning "-32"]

let recv_profile worker prv size time =
  begin
    Calibration.set worker.server prv size time ;
    Calibration.set worker.runner prv size time ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Message Handler                                                    --- *)
(* -------------------------------------------------------------------------- *)

let handler worker msg =
  try
    match msg with
    | ["RAISE"] ->
      send_ready worker
    | ["PROFILE";prv;size;time] ->
      recv_profile worker prv (int_of_string size) (float_of_string time)
    | _ -> ()
  with Invalid_argument _ -> ()

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
            Utils.flush () ;
            Format.printf "Reconnect to %s…@." worker.address ;
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
            Utils.flush () ;
            Format.printf "Abandon!@." ;
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
  let prvs = List.map Runner.id @@ Runner.all env in
  let jobs = Runner.maxjobs env in
  Utils.flush () ;
  List.iter (Format.printf "Prover %s@.") prvs ;
  Format.printf "Server %s@." server ;
  let context = Zmq.Context.create () in
  let timeout = int_of_float (polling *. 1e3) in
  let socket,polling = connect context server in
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
    runner = Calibration.create () ;
    server = Calibration.create () ;
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
    Zmq.Socket.disconnect worker.socket server ;
    Zmq.Socket.close worker.socket ;
    Zmq.Context.terminate worker.context ;
    Utils.flush () ;
    Format.printf "Terminated@." ;
    exit 0


(* -------------------------------------------------------------------------- *)
