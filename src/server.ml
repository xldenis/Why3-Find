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
(* --- Server Commands                                                    --- *)
(* -------------------------------------------------------------------------- *)

type emitter = { identity : bytes ; time : float }

type task = {
  prover : string ;
  digest : string ;
  mutable sourced : bool ; (* prover task has been downloaded *)
  mutable timeout : float ; (* max of requested timeout *)
  mutable waiting : emitter list ;
  mutable running : emitter list ;
}

type server = {
  context : Zmq.Context.t ;
  polling : Zmq.Poll.t ;
  clients : [ `Router ] Zmq.Socket.t ;
  workers : [ `Router ] Zmq.Socket.t ;
  pending : task Fibers.Queue.t ;
  hangup  : float ;
  mutable pulse : float ;
}

let recv socket ~time fn =
  match Zmq.Socket.recv_all ~block:false socket with
  | identity::msg -> fn { time ; identity = (Bytes.of_string identity) } msg
  | [] -> ()
  | exception Unix.Unix_error(EAGAIN,_,_) -> ()

let send socket emitter msg =
  Zmq.Socket.send_all socket (Bytes.to_string emitter.identity :: msg)

(* -------------------------------------------------------------------------- *)
(* --- Server Heartbeat                                                   --- *)
(* -------------------------------------------------------------------------- *)

let heartbeat server ~time =
  if time > server.pulse then
    begin
      server.pulse <- time +. 10.0 ;
    end

(* -------------------------------------------------------------------------- *)
(* --- Client Handler                                                     --- *)
(* -------------------------------------------------------------------------- *)

let client_handler server emitter msg =
  begin
    ignore server ;
    ignore emitter ;
    ignore msg ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Worker Handler                                                     --- *)
(* -------------------------------------------------------------------------- *)

let worker_handler server emitter msg =
  begin
    ignore server ;
    ignore emitter ;
    ignore msg ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Server Main Program                                                --- *)
(* -------------------------------------------------------------------------- *)

let create ~frontend ~backend ~hangup =
  if frontend = backend then
    Utils.failwith "Server frontend URL and backend URL shall differ" ;
  let context = Zmq.Context.create () in
  let clients = Zmq.Socket.(create context router) in
  let workers = Zmq.Socket.(create context router) in
  let polling = Zmq.Poll.(mask_of [| clients , In ; workers , In |]) in
  let pending = Fibers.Queue.create () in
  Zmq.Socket.bind clients frontend  ;
  Zmq.Socket.bind workers backend ;
  let hangup = float hangup *. 60.0 in
  let pulse = 0.0 in
  let server = {
    context ; polling ; clients ; workers ;
    pending ;
    hangup ;
    pulse
  } in
  while true do
    ignore @@ Zmq.Poll.poll ~timeout:60000 server.polling ;
    let time = Unix.time () in
    recv server.clients ~time (client_handler server) ;
    recv server.workers ~time (worker_handler server) ;
  done

(* -------------------------------------------------------------------------- *)
