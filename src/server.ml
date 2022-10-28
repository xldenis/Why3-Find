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
(* --- Server                                                             --- *)
(* -------------------------------------------------------------------------- *)

type emitter = { identity : string ; time : float }

type task = {
  prover : string ;
  digest : string ;
  mutable sourced : bool ; (* prover task has been downloaded *)
  mutable timeout : float ; (* max of requested timeout *)
  mutable waiting : emitter list ;
  mutable running : emitter list ;
}

(* -------------------------------------------------------------------------- *)
(* --- Tasks                                                              --- *)
(* -------------------------------------------------------------------------- *)

module Task =
struct
  type t = task
  let hash t = Hashtbl.hash (t.prover,t.digest)
  let equal a b = a.prover = b.prover && a.digest = b.digest
end

module TaskIndex = Hashtbl.Make(Task)

type server = {
  context : Zmq.Context.t ;
  polling : Zmq.Poll.t ;
  clients : [ `Router ] Zmq.Socket.t ;
  workers : [ `Router ] Zmq.Socket.t ;
  pending : unit TaskIndex.t ;
  cache : Runner.result TaskIndex.t ;
  database : string ;
  hangup  : float ;
  mutable pulse : float ;
}

let recv socket ~time fn =
  match Zmq.Socket.recv_all ~block:false socket with
  | identity::msg -> fn { time ; identity } msg
  | [] -> ()
  | exception Unix.Unix_error(EAGAIN,_,_) -> ()

let send socket emitter msg =
  Zmq.Socket.send_all socket (emitter.identity :: msg)

(* -------------------------------------------------------------------------- *)
(* --- Server Heartbeat                                                   --- *)
(* -------------------------------------------------------------------------- *)

let kill server task =
  List.iter
    (fun w -> send server.clients w ["kill";task.prover;task.digest])
    task.running

let heartbeat server ~time =
  if time > server.pulse then
    begin
      server.pulse <- time +. 10.0 ;
      let active emitter = time < emitter.time +. server.hangup in
      TaskIndex.iter
        begin fun task () ->
           task.waiting <- List.filter active task.waiting ;
           task.running <- List.filter active task.running ;
           if task.waiting = [] then
             begin
               kill server task ;
               TaskIndex.remove server.pending task ;
             end
        end server.pending ;
      let pendings = TaskIndex.length server.pending in
      Utils.progress "pending %4d" pendings
    end

(* -------------------------------------------------------------------------- *)
(* --- Server Cache                                                       --- *)
(* -------------------------------------------------------------------------- *)

let basename database gen task =
  let hh = String.sub task.digest 0 2 in
  Format.sprintf "%s/%d/%s/%s/%s" database gen task.prover hh task.digest

[@@@ warning "-32"]

let get server task =
  let rec lookup n =
    let gen = Format.sprintf "%s/%d" server.database n in
    if Sys.file_exists gen && Sys.is_directory gen then
      let base = basename server.database n task in
      if Sys.file_exists base && Sys.is_directory base then
        let root =
          if n > 0 then
            let root = basename server.database 0 task in
            Sys.rename base root ; root
          else base
        in
        let json = root ^ "/task.json" in
        let data = root ^ "/task.data" in
        let result =
          if Sys.file_exists json then
            try Json.of_file json |> Runner.of_json
            with _err ->
              Utils.flush () ;
              Format.eprintf "Error: incorrect database (removed entry)@." ;
              Sys.remove json ; Runner.NoResult
          else Runner.NoResult in
        let source = if Sys.file_exists data then Some data else None in
        result,source
      else
        lookup (succ n)
    else
      NoResult,None
  in lookup 0

let get_data file =
  let inc = open_in file in
  let buffer = Buffer.create 2048 in
  try
    while true do
      Buffer.add_channel buffer inc 2048
    done ; "" (* unreachable *)
  with End_of_file ->
    close_in inc ; Buffer.contents buffer

let filename server task ext =
  let base = basename server.database 0 task in
  Utils.mkdirs base ;
  Printf.sprintf "%s/task.%s" base ext

let set_result server task result =
  let file = filename server task ".json" in
  Runner.to_json result |> Json.to_file file

let set_data server task data =
  let file = filename server task ".data" in
  let out = open_out file in
  output_string out data ; close_out out

[@@@ warning "+32"]

(* -------------------------------------------------------------------------- *)
(* --- Shifting Database                                                  --- *)
(* -------------------------------------------------------------------------- *)

let prune_database database age =
  let rec shift n =
    let gen = Format.sprintf "%s/%d" database n in
    begin
      if Sys.file_exists gen && Sys.is_directory gen then
        let target = shift (succ n) in
        if n < age then Sys.rename gen target else
        if n > age then
          begin
            Format.printf "Deleting generation %d…@." n ;
            Utils.rmpath gen ;
          end
        else
          Format.printf "Shifting generations 1-%d…@." n
    end ; gen
  in
  begin
    Utils.flush () ;
    ignore @@ shift 0 ;
    Format.printf "Database pruned@." ;
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

let establish ~database ~frontend ~backend ~hangup =
  if frontend = backend then
    Utils.failwith "Server frontend URL and backend URL shall differ" ;
  let context = Zmq.Context.create () in
  let clients = Zmq.Socket.(create context router) in
  let workers = Zmq.Socket.(create context router) in
  let polling = Zmq.Poll.(mask_of [| clients , In ; workers , In |]) in
  Zmq.Socket.bind clients frontend  ;
  Zmq.Socket.bind workers backend ;
  let server = {
    database ;
    context ;
    polling ;
    clients ;
    workers ;
    pending = TaskIndex.create 0 ;
    cache = TaskIndex.create 0 ;
    hangup = float hangup *. 60.0 ;
    pulse = 0.0 ;
  } in
  Format.printf "Server is running…@." ;
  while true do
    let time = Unix.time () in
    recv server.clients ~time (client_handler server) ;
    recv server.workers ~time (worker_handler server) ;
    heartbeat server ~time ;
    ignore @@ Zmq.Poll.poll ~timeout:60000 server.polling ;
  done

(* -------------------------------------------------------------------------- *)
