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
  mutable cached : Runner.result ; (* prover result *)
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
(* --- Calibration                                                        --- *)
(* -------------------------------------------------------------------------- *)

let load_profile ~database =
  let file = Printf.sprintf "%s/profile.json" database in
  if Sys.file_exists file then
    Json.of_file file |> Calibration.of_json
  else
    Calibration.empty ()

(* -------------------------------------------------------------------------- *)
(* --- Worker                                                             --- *)
(* -------------------------------------------------------------------------- *)

type worker = {
  cores: int;
  worker: emitter ;
  provers: string list;
}

(* -------------------------------------------------------------------------- *)
(* --- Server                                                             --- *)
(* -------------------------------------------------------------------------- *)

type server = {
  context : Zmq.Context.t ;
  socket : [ `Router ] Zmq.Socket.t ;
  polling : Zmq.Poll.t ;
  database : string ;
  profile : Calibration.profile ;
  index : task TaskIndex.t;
  pending : task Fibers.Queue.t ;
  workers : worker Fibers.Queue.t ;
}

let pp_id fmt id =
  String.iter (fun c -> Format.fprintf fmt "%02x" @@ Char.code c) id

let pp_arg fmt arg =
  let arg = String.escaped arg in
  if String.length arg <= 8 then
    Format.fprintf fmt " %-8s |" arg
  else
    Format.fprintf fmt " %s… |" (String.sub arg 0 7)

let pp_args fmt args = List.iter (pp_arg fmt) args

let trace = ref false
let chrono = ref @@ Unix.time ()

let send server emitter msg =
  if !trace then
    begin
      Utils.flush () ;
      Format.printf "SEND@%a %a@." pp_id emitter.identity pp_args msg ;
    end ;
  Zmq.Socket.send_all server.socket (emitter.identity :: msg)

let recv server ~time fn =
  match Zmq.Socket.recv_all ~block:false server.socket with
  | [] -> true
  | identity::msg ->
    if !trace then
      begin
        Utils.flush () ;
        let delta = time -. !chrono in
        Format.printf "RECV@%a %a (%a)@." pp_id identity pp_args msg
          Utils.pp_time delta ;
      end ;
    fn { time ; identity } msg ; true
  | exception Unix.Unix_error(EAGAIN,_,_) -> false

(* -------------------------------------------------------------------------- *)
(* --- Server Cache                                                       --- *)
(* -------------------------------------------------------------------------- *)

let basename database gen { prover ; digest } =
  let hh = String.sub digest 0 2 in
  Format.sprintf "%s/%d/%s/%s/%s" database gen prover hh digest

let get_cache server goal =
  let rec lookup n =
    let gen = Format.sprintf "%s/%d" server.database n in
    if Sys.file_exists gen && Sys.is_directory gen then
      let base = basename server.database n goal in
      if Sys.file_exists base && Sys.is_directory base then
        let root =
          if n > 0 then
            let root = basename server.database 0 goal in
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
        result,Sys.file_exists data
      else
        lookup (succ n)
    else
      NoResult,false
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
[@@ warning "-32"]

let filename server task ext =
  let base = basename server.database 0 task in
  Utils.mkdirs base ;
  Printf.sprintf "%s/task.%s" base ext

let set_result server task result =
  let file = filename server task ".json" in
  Runner.to_json result |> Json.to_file file
[@@ warning "-32"]

let set_data server task data =
  let file = filename server task ".data" in
  let out = open_out file in
  output_string out data ; close_out out
[@@ warning "-32"]

let get_task server goal =
  try TaskIndex.find server.index goal with Not_found ->
    let cached,sourced = get_cache server goal in
    let task = {
      goal ;
      cached ;
      sourced ;
      waiting = [] ;
      loading = [] ;
      running = [] ;
      timeout = 0.0 ;
    } in
    TaskIndex.add server.index goal task ;
    Fibers.Queue.push server.pending task ;
    task
[@@ warning "-32"]

(* -------------------------------------------------------------------------- *)
(* --- Message Handler                                                    --- *)
(* -------------------------------------------------------------------------- *)

let handler server emitter msg =
  try
    ignore server ;
    ignore emitter ;
    ignore msg ;
  with Invalid_argument _ | Not_found -> ()

let flush ~time server =
  begin
    while recv server ~time (handler server) do () done ;
    chrono := time ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Sent Messages                                                      --- *)
(* -------------------------------------------------------------------------- *)

let send_kill server goal id =
  send server id ["KILL";goal.prover;goal.digest]

let send_download server goal id =
  send server id ["DOWNLOAD";goal.prover;goal.digest]

(* -------------------------------------------------------------------------- *)
(* --- Task Processing                                                    --- *)
(* -------------------------------------------------------------------------- *)

let process server ~time task =
  let timeout = max 2.0 (2.0 *. task.timeout) in
  let active emitter = time < emitter.time +. timeout in
  task.waiting <- List.filter active task.waiting ;
  task.loading <- List.filter active task.loading ;
  task.running <- List.filter active task.running ;
  if task.waiting = [] then
    begin
      List.iter (send_kill server task.goal) task.running ;
      task.running <- [] ;
      task.timeout <- 0.0 ;
    end
  else
  if not task.sourced && task.loading = [] then
    begin
      List.iter (send_download server task.goal) task.waiting ;
      task.loading <- task.waiting ;
    end
  else
  if task.sourced && task.running = [] then
    begin
      (*TODO: schedule *)
    end

(* -------------------------------------------------------------------------- *)
(* --- Server Main Program                                                --- *)
(* -------------------------------------------------------------------------- *)

let poll server =
  let mask = Zmq.Poll.poll ~timeout:1000 server.polling in
  mask.(0) <> None

let establish ~database ~address =
  let context = Zmq.Context.create () in
  let socket = Zmq.Socket.(create context router) in
  let polling = Zmq.Poll.(mask_of [| socket , In |]) in
  let profile = load_profile ~database in
  Zmq.Socket.bind socket address  ;
  let server = {
    profile ;
    database ;
    context ;
    polling ;
    socket ;
    pending = Fibers.Queue.create () ;
    workers = Fibers.Queue.create () ;
    index = TaskIndex.create 0 ;
  } in
  Format.printf "Server is running…@." ;
  while true do
    let _evt = poll server in
    let time = Unix.time () in
    flush ~time server ;
    Fibers.Queue.iter server.pending (process server ~time) ;
    Fibers.Queue.filter server.pending (fun { waiting } -> waiting <> []) ;
    let pendings = Fibers.Queue.size server.pending in
    Utils.progress "pending %4d" pendings ;
  done

(* -------------------------------------------------------------------------- *)
(* --- Shifting Database                                                  --- *)
(* -------------------------------------------------------------------------- *)

let prune ~database ~age =
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
