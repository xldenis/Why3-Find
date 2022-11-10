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
  socket : [ `Dealer ] Zmq.Socket.t ;
  polling : Zmq.Poll.t ;
  maxjobs : int ;
  provers : Runner.prover list ;
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
(* --- Commands                                                           --- *)
(* -------------------------------------------------------------------------- *)

let send_ready worker =
  send worker
    ("READY"::string_of_int worker.maxjobs::List.map Runner.id worker.provers)

(* -------------------------------------------------------------------------- *)
(* --- Message Handler                                                    --- *)
(* -------------------------------------------------------------------------- *)

let handler _worker = function
  | _ -> ()

(* -------------------------------------------------------------------------- *)
(* --- Worker Polling                                                     --- *)
(* -------------------------------------------------------------------------- *)

let poll ~timeout worker =
  if Fibers.pending () > 0 then
    (Fibers.yield () ; Unix.sleepf 0.001)
  else
    ignore @@ Zmq.Poll.poll ~timeout worker.polling

let flush worker handler =
  while recv worker handler do Fibers.yield () done

(* -------------------------------------------------------------------------- *)
(* --- Worker Main Loop                                                   --- *)
(* -------------------------------------------------------------------------- *)

let connect ~server ~polling =
  let wenv = Wenv.init () in
  let prvs = Runner.all wenv in
  let jobs = Runner.maxjobs wenv in
  Utils.flush () ;
  List.iter (fun prv -> Format.printf "Prover %s@." (Runner.id prv)) prvs ;
  Format.printf "Server %s@." server ;
  let context = Zmq.Context.create () in
  let socket = Zmq.Socket.(create context dealer) in
  let timeout = int_of_float (polling *. 1e3) in
  let polling = Zmq.Poll.(mask_of [| socket , In |]) in
  Zmq.Socket.connect socket server ;
  let worker = {
    socket ;
    polling ;
    provers = prvs ;
    maxjobs = jobs ;
  } in
  Format.printf "Worker running…@." ;
  while true do
    poll ~timeout worker ;
    send_ready worker ;
    flush worker (handler worker) ;
    let busy = Runner.running () in
    let over = Runner.pending () in
    if over > 0 then
        Utils.progress "%d/%d overload:%d" busy jobs over
    else
      Utils.progress "%d/%d" busy jobs
  done

(* -------------------------------------------------------------------------- *)
