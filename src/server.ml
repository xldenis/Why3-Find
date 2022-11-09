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

let save_profile ~database profile =
  let file = Printf.sprintf "%s/profile.json" database in
  Json.to_file file @@ Calibration.to_json profile

(* -------------------------------------------------------------------------- *)
(* --- Worker                                                             --- *)
(* -------------------------------------------------------------------------- *)

type worker = {
  mutable cores: int;
  worker: emitter ;
  provers: string list;
}

let remove { identity = a } ids =
  List.filter (fun { identity = b } -> a <> b) ids

let add id ids = id :: remove id ids

let now ~time { identity } = { identity ; time }

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

let filename server goal ext =
  let base = basename server.database 0 goal in
  Utils.mkdirs base ;
  Printf.sprintf "%s/task.%s" base ext

let set_result server task result =
  let file = filename server task ".json" in
  Runner.to_json result |> Json.to_file file
[@@ warning "-32"]

let set_data server goal data =
  Utils.save ~file:(filename server goal ".data") data

let get_data server goal =
  Utils.load ~file:(filename server goal ".data")

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

(* -------------------------------------------------------------------------- *)
(* --- Sent Messages                                                      --- *)
(* -------------------------------------------------------------------------- *)

let send_profile server prv size time id =
  send server id ["PROFILE";prv;string_of_int size;string_of_float time]

let send_kill server goal id =
  send server id ["KILL";goal.prover;goal.digest]

let send_download server goal id =
  send server id ["DOWNLOAD";goal.prover;goal.digest]

let send_prove server goal timeout data id =
  send server id ["PROVE";goal.prover;goal.digest;string_of_float timeout;data]

let send_result server goal status time id =
  send server id ["RESULT";goal.prover;goal.digest;status;string_of_float time]

let send_hiring server id =
  send server id ["HIRING"]

(* -------------------------------------------------------------------------- *)
(* --- PROFILE Command                                                    --- *)
(* -------------------------------------------------------------------------- *)

let do_profile server id prv s t =
  let { profile ; database } = server in
  let size,time =
    match Calibration.get profile prv with
    | Some(s0,t0) -> s0,t0
    | None ->
      Calibration.set profile prv s t ;
      save_profile ~database profile ; s,t
  in send_profile server prv size time id

(* -------------------------------------------------------------------------- *)
(* --- Finalize Task                                                      --- *)
(* -------------------------------------------------------------------------- *)

let finalize server id task status time =
  begin
    List.iter (send_result server task.goal status time) (remove id task.waiting) ;
    List.iter (send_kill server task.goal) (remove id task.running) ;
    task.waiting <- [] ;
    task.running <- [] ;
  end

(* -------------------------------------------------------------------------- *)
(* --- GET Command                                                        --- *)
(* -------------------------------------------------------------------------- *)

let do_get server id goal timeout =
  let task = get_task server goal in
  let status =
    match task.cached with
    | NoResult | Failed -> None
    | Unknown t -> Some ("Unknown",t)
    | Valid t -> if t <= timeout then Some ("Valid",t) else None
    | Timeout t -> if timeout <= t then Some ("Timeout",t) else None
  in
  begin
    match status with
    | Some(status,time) -> finalize server id task status time
    | None ->
      if task.waiting = [] then Fibers.Queue.push server.pending task ;
      task.waiting <- add id task.waiting ;
      task.timeout <- max task.timeout timeout ;
  end

(* -------------------------------------------------------------------------- *)
(* --- UPLOAD Command                                                     --- *)
(* -------------------------------------------------------------------------- *)

let do_upload server id goal data =
  let task = get_task server goal in
  if not task.sourced then
    begin
      set_data server goal data ;
      task.sourced <- true ;
      task.loading <- remove id task.loading ;
    end

(* -------------------------------------------------------------------------- *)
(* --- READY Command                                                      --- *)
(* -------------------------------------------------------------------------- *)

let do_ready server id jobs provers =
  begin
    Fibers.Queue.filter server.workers
      (fun w -> w.worker.identity <> id.identity) ;
    Fibers.Queue.push server.workers {
      worker = id ;
      cores = jobs ;
      provers
    } ;
  end

(* -------------------------------------------------------------------------- *)
(* --- RESULT Command                                                     --- *)
(* -------------------------------------------------------------------------- *)

let do_result server id goal status time =
  Fibers.Queue.iter server.workers
    (fun w ->
       if w.worker.identity = id.identity then
         w.cores <- succ w.cores
    ) ;
  let result =
    match status with
    | "Valid" -> Runner.Valid time
    | "Unknown" -> Runner.Unknown time
    | "Timeout" -> Runner.Timeout time
    | _ -> Runner.NoResult
  in
  if result <> NoResult then
    begin
      let task = get_task server goal in
      set_result server goal result ;
      task.cached <- result ;
      finalize server id task status time ;
    end

(* -------------------------------------------------------------------------- *)
(* --- KILL Command                                                       --- *)
(* -------------------------------------------------------------------------- *)

let do_kill server id goal =
  let task = get_task server goal in
  task.waiting <- remove id task.waiting

(* -------------------------------------------------------------------------- *)
(* --- HANGUP Command                                                     --- *)
(* -------------------------------------------------------------------------- *)

let do_hangup server id =
  begin
    Fibers.Queue.iter server.pending
      (fun task ->
         task.waiting <- remove id task.waiting ;
         task.loading <- remove id task.loading ;
         task.running <- remove id task.running ;
      ) ;
    Fibers.Queue.filter server.workers
      (fun w -> w.worker.identity <> id.identity) ;
  end

(* -------------------------------------------------------------------------- *)
(* --- PROVE Command                                                      --- *)
(* -------------------------------------------------------------------------- *)

let schedule server ~time task =
  try
    let n = Fibers.Queue.size server.workers in
    for _i = 1 to n do
      let w = Fibers.Queue.pop server.workers in
      Fibers.Queue.push server.workers w ;
      if w.cores > 0 && List.mem task.goal.prover w.provers then
        begin
          w.cores <- pred w.cores ;
          let data = get_data server task.goal in
          send_prove server task.goal task.timeout data w.worker ;
          task.running <- now ~time w.worker :: task.running ;
          raise Exit ;
        end
    done
  with Not_found | Exit -> ()

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
      let id = List.hd task.waiting in
      send_download server task.goal id ;
      task.loading <- [now ~time id] ;
    end
  else
  if task.sourced && task.running = [] then
    schedule server ~time task

(* -------------------------------------------------------------------------- *)
(* --- Message Handler                                                    --- *)
(* -------------------------------------------------------------------------- *)

let handler server id msg =
  try
    match msg with
    | ["PROFILE";prover;size;time] ->
      do_profile server id prover (int_of_string size) (float_of_string time)
    | ["GET";prover;digest;timeout] ->
      do_get server id { prover ; digest } (float_of_string timeout)
    | ["UPLOAD";prover;digest;data] ->
      do_upload server id { prover ; digest } data
    | "READY"::jobs::provers ->
      do_ready server id (int_of_string jobs) provers
    | ["RESULT";prover;digest;status;time] ->
      do_result server id { prover ; digest } status (float_of_string time)
    | ["KILL";prover;digest] ->
      do_kill server id { prover ; digest }
    | ["HANGUP"] ->
      do_hangup server id
    | _ -> ()
  with Invalid_argument _ | Not_found -> ()

let flush ~time server =
  begin
    while recv server ~time (handler server) do () done ;
    chrono := time ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Server Statistics                                                  --- *)
(* -------------------------------------------------------------------------- *)

let print_stats server =
  begin
    let waiting = ref 0 in
    let loading = ref 0 in
    let running = ref 0 in
    let cores = ref 0 in
    Fibers.Queue.iter server.pending
      (fun task ->
         incr @@
           if task.running <> [] then running else
           if task.loading <> [] then loading else
             waiting) ;
    Fibers.Queue.iter server.workers
      (fun w -> cores := w.cores + !cores) ;
    let workers = Fibers.Queue.size server.workers in
    Utils.progress "Tasks:%d/%d/%d  Workers:%d(%d)"
      !waiting !loading !running workers (!cores + !running)
  end

(* -------------------------------------------------------------------------- *)
(* --- Server Main Program                                                --- *)
(* -------------------------------------------------------------------------- *)

let poll server =
  let mask = Zmq.Poll.poll ~timeout:1000 server.polling in
  mask.(0) <> None

let stuck server =
  try
    Fibers.Queue.iter server.pending
      (fun task -> if task.loading = [] && task.running = [] then raise Exit) ;
    false
  with Exit -> true

let hire server =
  begin
    Fibers.Queue.iter server.workers (fun w -> send_hiring server w.worker) ;
    Fibers.Queue.clear server.workers ;
  end

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
    let activity = poll server in
    let time = Unix.time () in
    flush ~time server ;
    Fibers.Queue.iter server.pending (process server ~time) ;
    Fibers.Queue.filter server.pending (fun { waiting } -> waiting <> []) ;
    if not activity && stuck server then hire server ;
    print_stats server ;
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
