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
(* --- Why3 Runner                                                        --- *)
(* -------------------------------------------------------------------------- *)

open Why3

(* -------------------------------------------------------------------------- *)
(* --- Provers                                                            --- *)
(* -------------------------------------------------------------------------- *)

type prover = {
  config : Whyconf.config_prover ;
  driver : Driver.driver ;
}

type callback =
  Why3.Whyconf.prover ->
  Why3.Call_provers.resource_limit ->
  Why3.Call_provers.prover_result ->
  unit

let id prv = Whyconf.prover_parseable_format prv.config.prover
let name prv = String.lowercase_ascii @@ prv.config.prover.prover_name
let title ?(strict=false) p = if strict then id p else name p

let pp_prover fmt prv = Format.pp_print_string fmt @@ id prv

let load (env : Wenv.env) (config : Whyconf.config_prover) =
  try Whyconf.load_driver (Whyconf.get_main env.wconfig) env.wenv config
  with _ ->
    Format.eprintf "Failed to load driver for %s@."
      (Whyconf.prover_parseable_format config.prover) ;
    exit 2

let find_exact (env : Wenv.env) s =
  try
    let filter = Whyconf.parse_filter_prover s in
    let config = Whyconf.filter_one_prover env.wconfig filter in
    let driver = load env config in
    Some { config ; driver }
  with _ -> None

let find_default env name =
  match find_exact env name with
  | Some prv -> [prv]
  | None -> []

let relax name =
  String.lowercase_ascii @@
  match String.split_on_char ',' name with
  | shortname :: _ :: _ -> shortname
  | _ -> name

let relaxed p = (String.lowercase_ascii p = p)

let find env name =
  match find_exact env name with
  | Some prv -> prv
  | None ->
    match find_exact env (String.lowercase_ascii name) with
    | Some prv -> prv
    | None ->
      match String.split_on_char ',' name with
      | shortname :: _ :: _ ->
        begin
          match find_exact env (String.lowercase_ascii shortname) with
          | Some prv ->
            Format.eprintf "Warning: prover %S not found, fallback to %a.@."
              name pp_prover prv ;
            prv
          | None -> raise Not_found
        end
      | _ -> raise Not_found

let prover env name =
  try find env name
  with Not_found ->
    Format.eprintf "Error: prover %S not found." name ;
    exit 2

let all env =
  let byid p q = String.compare (id p) (id q) in
  List.sort byid @@
  Whyconf.Mprover.fold
    (fun _id config prvs ->
       if config.Whyconf.prover.prover_altern = "" then
         let driver = load env config in
         { config ; driver } :: prvs
       else prvs
    ) (Whyconf.get_provers env.Wenv.wconfig) []

let default env =
  find_default env "alt-ergo" @
  find_default env "z3" @
  find_default env "cvc4"

let select env provers =
  if provers = []
  then default env
  else List.map (prover env) provers

(* -------------------------------------------------------------------------- *)
(* --- Prover Result                                                      --- *)
(* -------------------------------------------------------------------------- *)

type result =
  | NoResult | Failed | Unknown of float | Timeout of float | Valid of float

let merge a b =
  match a,b with
  | NoResult,c | c,NoResult -> c
  | Failed,c | c,Failed -> c
  | Valid ta , Valid tb -> Valid (min ta tb)
  | Valid _ , (Unknown _ | Timeout _) -> a
  | (Unknown _ | Timeout _) , Valid _ -> b
  | Unknown ta , Unknown tb -> Unknown (min ta tb)
  | Unknown _ , Timeout _ -> a
  | Timeout _ , Unknown _ -> b
  | Timeout ta , Timeout tb -> Timeout (max ta tb)

let map f = function
  | NoResult -> NoResult
  | Failed -> Failed
  | Valid t -> Valid (f t)
  | Unknown t -> Unknown (f t)
  | Timeout t -> Timeout (f t)

let pp_result fmt = function
  | NoResult -> Format.pp_print_string fmt "No Result"
  | Failed -> Format.pp_print_string fmt "Failed"
  | Unknown t -> Format.fprintf fmt "Unknown (%a)" Utils.pp_time t
  | Timeout t -> Format.fprintf fmt "Timeout (%a)" Utils.pp_time t
  | Valid t -> Format.fprintf fmt "Valid (%a)" Utils.pp_time t

let of_json (js : Json.t) =
  let open Json in
  let status = jfield "status" js |> jstring in
  let time = jfield "time" js |> jfloat in
  match status with
  | "Failed" -> Failed
  | "Unknown" -> Unknown time
  | "Timeout" -> Timeout time
  | "Valid" -> Valid time
  | _ -> NoResult

let to_json (r : result) : Json.t =
  let status st = `Assoc [ "status", `String st ] in
  let timed st t = `Assoc [ "status", `String st ; "time", `Float t ] in
  match r with
  | NoResult -> status "NoResult"
  | Failed -> status "Failed"
  | Unknown t -> timed "Unknown" t
  | Timeout t -> timed "Timeout" t
  | Valid t -> timed "Valid" t

(* -------------------------------------------------------------------------- *)
(* --- Prover Cache                                                       --- *)
(* -------------------------------------------------------------------------- *)

let cachedir = ".why3find"
let version = ".why3find/v1"

let destroycache () =
  begin
    if Utils.tty then
      Utils.log "Upgrading cache (%s)@." (Filename.basename version) ;
    Utils.rmpath cachedir ;
  end

let preparecache () =
  begin
    Utils.mkdirs cachedir ;
    let out = open_out version in
    output_string out "why3find cache " ;
    output_string out (Filename.basename version) ;
    output_string out "\n" ;
    close_out out ;
  end

let checkcache = lazy
  begin
    let cd = Sys.file_exists cachedir in
    let cv = Sys.file_exists version in
    Utils.flush () ;
    if cd && not cv then destroycache () ;
    if not cd || not cv then preparecache () ;
  end

module Cache = Hashtbl.Make
    (struct
      type t = Task.task * prover
      let hash (t,p) = Hashtbl.hash (Task.task_hash t , id p)
      let equal (t1,p1) (t2,p2) = (p1 == p2) && (Task.task_equal t1 t2)
    end)

let digest task = Why3.Termcode.(task_checksum task |> string_of_checksum)

let data prv task =
  let buffer = Buffer.create 2048 in
  let fmt = Format.formatter_of_buffer buffer in
  ignore @@ Driver.print_task_prepared prv.driver fmt task ;
  Format.pp_print_flush fmt () ;
  Buffer.contents buffer

let file (t,p) =
  Lazy.force checkcache ;
  let hash = digest t in
  let h2 = String.sub hash 0 2 in
  Printf.sprintf ".why3find/%s/%s/%s.json" (id p) h2 hash

let read e =
  let f = file e in
  if Sys.file_exists f then
    try Json.of_file f |> of_json with _ -> NoResult
  else NoResult

let write e r =
  let f = file e in
  Utils.mkdirs (Filename.dirname f) ;
  Json.to_file f (to_json r)

let hash = Cache.create 0
let cache = ref true

let get e =
  try Cache.find hash e with Not_found ->
    let r = if !cache then read e else NoResult in
    Cache.add hash e r ; r

let set e r1 =
  let r0 = try Cache.find hash e with Not_found -> NoResult in
  let r = merge r0 r1 in
  if r <> r0 then
    match r with
    | NoResult -> ()
    | Failed ->
      Cache.replace hash e Failed
    | Valid _ | Unknown _ | Timeout _ ->
      Cache.replace hash e r ; write e r

(* -------------------------------------------------------------------------- *)
(* --- Running Prover                                                     --- *)
(* -------------------------------------------------------------------------- *)

let jobs = ref 0
let clients = ref 0
let pending = ref 0
let running = ref 0
let goals = ref []

let rec ginc a = function
  | [] -> [a,1]
  | ((a0,n) as hd)::tl ->
    if a = a0 then (a0,n+1)::tl else hd::ginc a tl

let rec gdec a = function
  | [] -> []
  | ((a0,n) as hd)::tl ->
    if a = a0 then
      if n > 1 then (a0,n-1)::tl else tl
    else hd :: gdec a tl

let schedule () = incr pending
let unschedule () = decr pending

let start ?name () =
  decr pending ;
  incr running ;
  match name with
  | None -> ()
  | Some a -> goals := ginc a !goals

let stop ?name () =
  decr running ;
  match name with
  | None -> ()
  | Some a -> goals := gdec a !goals

let pending () = !pending
let running () = !running

let pp_goals fmt =
  List.iter
    (fun (a,n) ->
       if n > 1 then
         Format.fprintf fmt " %s(%d)" a n
       else
         Format.fprintf fmt " %s" a
    ) !goals

let is_modified () = !jobs > 0

let save_config (env : Wenv.env) =
  let j = !jobs in
  if j > 0 then
    let open Why3 in
    let file = Whyconf.get_conf_file env.wconfig in
    if file <> "" then
      let main = Whyconf.get_main env.wconfig in
      let time = Whyconf.timelimit main in
      let mem = Whyconf.memlimit main in
      let config = Whyconf.User.set_limits ~time ~mem ~j env.wconfig in
      Whyconf.save_config config ;
      Format.printf "Why3 config. saved to %s@." file

let maxjobs (env : Wenv.env) =
  if !jobs > 0 then !jobs else
    let main = Whyconf.get_main env.wconfig in
    Whyconf.running_provers_max main

let update_client env =
  let njobs = maxjobs env in
  if njobs <> !clients then
    begin
      clients := njobs ;
      Why3.Prove_client.set_max_running_provers njobs ;
    end

let limit env t =
  Call_provers.{
    limit_time = int_of_float (t +. 0.5) ;
    limit_mem = Whyconf.memlimit (Whyconf.get_main env.Wenv.wconfig) ;
    limit_steps = (-1) ;
  }

let notify env prover result cb =
  let fire cb p l r = cb p.config.prover l r in
  let pr ~s ?(t = 0.0) ans =
    Why3.Call_provers.{
      pr_answer = ans ;
      pr_status = Unix.WEXITED s ;
      pr_time = t ;
      pr_steps = 0 ;
      pr_output = "Cached" ;
      pr_models = [] ;
    }
  in
  match result with
  | NoResult -> ()
  | Valid t -> fire cb prover (limit env t) (pr ~s:0 ~t Valid)
  | Timeout t -> fire cb prover (limit env t) (pr ~s:1 ~t Timeout)
  | Unknown t -> fire cb prover (limit env t) (pr ~s:1 ~t (Unknown "why3find"))
  | Failed -> fire cb prover (limit env 1.0) (pr ~s:2 (Failure "why3find"))

let notify_pr prv limit pr cb =
  cb prv.config.prover limit pr

type prepared_task = Prepared of Task.task | Buffered of Buffer.t

let call_prover (env : Wenv.env)
    ?(name : string option)
    ?(cancel : unit Fibers.signal option)
    ?(callback : callback option)
    ~(prepared : prepared_task)
    ~(prover : prover)
    ~(timeout : float) () =
  let main = Whyconf.get_main env.wconfig in
  let limit = limit env timeout in
  let clockwall = ref 0.0 in
  let started = ref false in
  let killed = ref false in
  let canceled = ref false in
  let timedout = ref false in
  update_client env ;
  schedule () ;
  let libdir = Whyconf.libdir main in
  let datadir = Whyconf.datadir main in
  let call =
    match prepared with
    | Prepared task ->
      Driver.prove_task_prepared
        ~command:(Whyconf.get_complete_command prover.config ~with_steps:false)
        ~libdir ~datadir ~limit prover.driver task
    | Buffered buffer ->
      Driver.prove_buffer_prepared
        ~command:(Whyconf.get_complete_command prover.config ~with_steps:false)
        ~libdir ~datadir ~limit prover.driver buffer
  in
  let kill () =
    if not !killed then
      begin
        try
          Call_provers.interrupt_call ~libdir call ;
          killed := true ;
        with _ -> ()
      end in
  let interrupt () =
    begin
      canceled := true ;
      if !started then kill () ;
    end in
  Fibers.monitor ?signal:cancel ~handler:interrupt @@
  Fibers.async @@
  begin fun () ->
    let t0 = Unix.gettimeofday () in
    if !started then
      begin
        if !canceled then kill ()
        else if not !timedout && !clockwall < t0 then
          (timedout := true ; kill ()) ;
      end ;
    let query =
      try Call_provers.query_call call with e -> InternalFailure e in
    match query with
    | NoUpdates
    | ProverInterrupted ->
      None
    | ProverStarted ->
      if not !started then
        begin
          start ?name () ;
          clockwall := Unix.gettimeofday () +. timeout *. 1.5 ;
          started := true ;
        end ;
      None
    | InternalFailure _ ->
      begin
        if !started then stop ?name () ;
        Some Failed
      end
    | ProverFinished pr ->
      let result, precise =
        match pr.pr_answer with
        | Valid ->
          Valid pr.pr_time, true
        | Timeout ->
          Timeout timeout, false
        | Invalid | Unknown _ | OutOfMemory | StepLimitExceeded ->
          Unknown pr.pr_time, true
        | Failure _ | HighFailure ->
          (if !canceled then NoResult else
           if !timedout then Timeout timeout else
             Failed), false
      in
      if !started then stop ?name () else unschedule () ;
      begin match callback with
        | Some cb ->
          if precise then
            notify_pr prover limit pr cb
          else
            notify env prover result cb
        | None -> ()
      end ;
      Some result
  end

(* -------------------------------------------------------------------------- *)
(* --- Running Prover with Cache                                          --- *)
(* -------------------------------------------------------------------------- *)

let hits = ref 0
let miss = ref 0

let crop ~timeout result =
  match result with
  | NoResult -> None
  | Failed | Unknown _ -> Some result
  | Timeout t -> if timeout <= t then Some result else None
  | Valid t ->
    if t <= timeout *. 1.25 then Some result else Some (Timeout timeout)

let definitive ~timeout result =
  match result with
  | NoResult -> false
  | Timeout t -> timeout <= t
  | Failed | Unknown _ | Valid _ -> true

let prove env ?name ?cancel ?callback prover task timeout =
  let task = Driver.prepare_task prover.driver task in
  let entry = task,prover in
  let cached = get entry in
  match crop ~timeout cached with
  | Some cached ->
    incr hits ;
    Option.iter (notify env prover cached) callback ;
    Fibers.return cached
  | None ->
    incr miss ;
    Fibers.map
      (fun result -> set entry result ; result)
      (call_prover env ?name ?cancel ?callback
         ~prepared:(Prepared task) ~prover ~timeout ())

type prooftask = Why3.Task.task

let update prover task result =
  set (task,prover) result

let prove_cached prover task timeout =
  let task = Driver.prepare_task prover.driver task in
  let cached = get (task,prover) in
  match crop ~timeout cached with
  | Some cached ->
    incr hits ;
    `Cached cached
  | None ->
    incr miss ;
    `Prepared task

let prove_prepared env ?name ?cancel ?callback prover task timeout =
  call_prover env ?name ?cancel ?callback
    ~prepared:(Prepared task) ~prover ~timeout ()

let prove_buffer env ?cancel prover buffer timeout =
  call_prover env ?cancel ~prepared:(Buffered buffer) ~prover ~timeout ()

(* -------------------------------------------------------------------------- *)
(* --- Options                                                            --- *)
(* -------------------------------------------------------------------------- *)

let options = [
  "-c", Arg.Clear cache, "force cache update";
  "-j", Arg.Set_int jobs, "JOBS max parallel provers";
]

let print_stats () =
  let h = !hits in
  let m = !miss in
  Format.printf "Cache %d/%d@." h (h+m)

(* -------------------------------------------------------------------------- *)
