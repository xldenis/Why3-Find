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

type goal = { prover : string ; digest : string }
type emitter = { identity : string ; time : float }

type task = {
  goal : goal ;
  mutable sourced : bool ; (* prover task has been downloaded *)
  mutable timeout : float ; (* max of requested timeout *)
  mutable waiting : emitter list ;
  mutable loading : emitter list ;
  mutable running : emitter list ;
}

(* -------------------------------------------------------------------------- *)
(* --- Tasks                                                              --- *)
(* -------------------------------------------------------------------------- *)

module Goal =
struct
  type t = goal
  let hash g = Hashtbl.hash (g.prover,g.digest)
  let equal a b = a.prover = b.prover && a.digest = b.digest
end

module TaskIndex = Hashtbl.Make(Goal)

(* -------------------------------------------------------------------------- *)
(* --- Velocity                                                           --- *)
(* -------------------------------------------------------------------------- *)

let load_profile ~database =
  let file = Printf.sprintf "%s/profile.json" database in
  if Sys.file_exists file then
    Json.of_file file |> Calibration.of_json
  else
    Calibration.empty ()

(* -------------------------------------------------------------------------- *)
(* --- Server                                                             --- *)
(* -------------------------------------------------------------------------- *)

type server = {
  context : Zmq.Context.t ;
  socket : [ `Router ] Zmq.Socket.t ;
  polling : Zmq.Poll.t ;
  database : string ;
  profile : Calibration.profile ;
  cache : Runner.result TaskIndex.t ;
  pending : task Fibers.Queue.t ;
  hangup  : float ;
  mutable pulse : float ;
}

let recv server ~time fn =
  match Zmq.Socket.recv_all ~block:false server.socket with
  | identity::msg -> fn { time ; identity } msg
  | [] -> ()
  | exception Unix.Unix_error(EAGAIN,_,_) -> ()

let send server emitter msg =
  Zmq.Socket.send_all server.socket (emitter.identity :: msg)

(* -------------------------------------------------------------------------- *)
(* --- Server Heartbeat                                                   --- *)
(* -------------------------------------------------------------------------- *)

let kill server { goal ; running } =
  List.iter
    (fun id -> send server id ["kill";goal.prover;goal.digest])
    running

let heartbeat server ~time =
  if time > server.pulse then
    begin
      server.pulse <- time +. 10.0 ;
      let active emitter = time < emitter.time +. server.hangup in
      Fibers.Queue.filter server.pending
        begin fun task ->
          task.waiting <- List.filter active task.waiting ;
          task.loading <- List.filter active task.loading ;
          task.running <- List.filter active task.running ;
          if task.waiting = [] then
            begin
              kill server task ;
              false
            end
          else true
        end ;
      let pendings = Fibers.Queue.size server.pending in
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
(* --- Message Handler                                                    --- *)
(* -------------------------------------------------------------------------- *)

let handler server emitter msg =
  begin
    ignore server ;
    ignore emitter ;
    ignore msg ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Server Main Program                                                --- *)
(* -------------------------------------------------------------------------- *)

let establish ~database ~url ~hangup =
  let context = Zmq.Context.create () in
  let socket = Zmq.Socket.(create context router) in
  let polling = Zmq.Poll.(mask_of [| socket , In |]) in
  let profile = load_profile ~database in
  Zmq.Socket.bind socket url  ;
  let server = {
    profile ;
    database ;
    context ;
    polling ;
    socket ;
    pending = Fibers.Queue.create () ;
    cache = TaskIndex.create 0 ;
    hangup = float hangup *. 60.0 ;
    pulse = 0.0 ;
  } in
  Format.printf "Server is running…@." ;
  while true do
    let time = Unix.time () in
    recv server ~time (handler server) ;
    heartbeat server ~time ;
    ignore @@ Zmq.Poll.poll ~timeout:60000 server.polling ;
  done

(* -------------------------------------------------------------------------- *)
