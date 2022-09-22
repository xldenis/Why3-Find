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

open Why3.Whyconf
open Why3.Driver
open Why3.Task
open Why3.Call_provers

(* -------------------------------------------------------------------------- *)
(* --- Provers                                                            --- *)
(* -------------------------------------------------------------------------- *)

type prover = {
  config : config_prover ;
  driver : driver ;
}

type callback =
  Why3.Whyconf.prover ->
  Why3.Call_provers.resource_limit ->
  Why3.Call_provers.prover_result ->
  unit

let id prv = prover_parseable_format prv.config.prover
let name prv = String.lowercase_ascii @@ prv.config.prover.prover_name

let pp_prover fmt prv = Format.pp_print_string fmt @@ id prv

let find_exact (env : Wenv.env) s =
  try
    let filter = parse_filter_prover s in
    let config = filter_one_prover env.wconfig filter in
    let driver =
      try load_driver (get_main env.wconfig) env.wenv config
      with _ ->
        Format.eprintf "Failed to load driver for %s@."
          (prover_parseable_format config.prover) ;
        exit 2
    in
    Some { config ; driver }
  with _ -> None

let find_default env name =
  match find_exact env name with
  | Some prv -> [prv]
  | None -> []

let prover env name =
  try
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
  with Not_found ->
    Format.eprintf "Error: prover %S not found." name ;
    exit 2

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

module Cache = Hashtbl.Make
    (struct
      type t = task * prover
      let hash (t,p) = Hashtbl.hash (task_hash t , id p)
      let equal (t1,p1) (t2,p2) = (p1 == p2) && (task_equal t1 t2)
    end)

let file (t,p) =
  let hash = Why3.Termcode.(task_checksum t |> string_of_checksum) in
  let h2 = String.sub hash 0 2 in
  Printf.sprintf ".why3find/%s/%s/%s.json" h2 hash (id p)

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

let set e r =
  match r with
  | NoResult -> ()
  | Failed -> Cache.replace hash e r
  | Valid _ | Unknown _ | Timeout _ -> Cache.replace hash e r ; write e r

(* -------------------------------------------------------------------------- *)
(* --- Running Prover                                                     --- *)
(* -------------------------------------------------------------------------- *)

let jobs = ref 0
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

let p prover = prover.config.prover

let limit env t =
  {
    limit_time = int_of_float (t +. 0.5) ;
    limit_mem = memlimit (get_main env.Wenv.wconfig) ;
    limit_steps = (-1) ;
  }

let call_prover (env : Wenv.env)
    ?(name : string option)
    ?(cancel : unit Fibers.signal option)
    ?(callback : callback option)
    (task : task) (prover : prover) (time : float) =
  let main = get_main env.wconfig in
  let limit = limit env time in
  let timeout = ref 0.0 in
  let started = ref false in
  let killed = ref false in
  let canceled = ref false in
  let np = !jobs in
  if np > 0 then
    (jobs := 0 ; Why3.Prove_client.set_max_running_provers np) ;
  schedule () ;
  let cancel = match cancel with None -> Fibers.signal () | Some s -> s in
  let call = prove_task
      ~command:(get_complete_command prover.config ~with_steps:false)
      ~libdir:(libdir main)
      ~datadir:(datadir main)
      ~limit prover.driver task in
  let kill () =
    if not !killed then
      begin
        try
          interrupt_call ~libdir:(libdir main) call ;
          killed := true ;
        with _ -> ()
      end in
  let interrupt () =
    begin
      canceled := true ;
      if !started then kill () ;
    end in
  Fibers.hook cancel interrupt Fibers.async
    begin fun () ->
      let t0 = Unix.gettimeofday () in
      if !started && (!canceled || !timeout < t0) then kill () ;
      let query = try query_call call with e -> InternalFailure e in
      match query with
      | NoUpdates
      | ProverInterrupted ->
        None
      | ProverStarted ->
        if not !started then
          begin
            start ?name () ;
            timeout := Unix.gettimeofday () +. time ;
            started := true ;
         end ;
        None
      | InternalFailure _ ->
        begin
          if !started then stop ?name () ;
          Some Failed
        end
      | ProverFinished pr ->
        let result =
          match pr.pr_answer with
          | Valid -> Valid pr.pr_time
          | Timeout -> Timeout pr.pr_time
          | Invalid | Unknown _ | OutOfMemory | StepLimitExceeded ->
            Unknown pr.pr_time
          | Failure _ ->
            if !killed then NoResult else Failed
          | HighFailure ->
            if !killed then Timeout pr.pr_time else Failed
        in
        if !started then stop ?name () else unschedule () ;
        (match callback with None -> () | Some f -> f (p prover) limit pr) ;
        Some result
    end

(* -------------------------------------------------------------------------- *)
(* --- Running Prover with Cache                                          --- *)
(* -------------------------------------------------------------------------- *)

let hits = ref 0
let miss = ref 0

let pr ~s ?(t = 0.0) ans =
  Why3.Call_provers.{
    pr_answer = ans ;
    pr_status = Unix.WEXITED s ;
    pr_time = t ;
    pr_steps = 0 ;
    pr_output = "Cached" ;
    pr_models = [] ;
  }

let notify env prover (callback : callback option) cached =
  match callback with
  | None -> ()
  | Some f ->
    match cached with
    | NoResult -> ()
    | Valid t -> f (p prover) (limit env t) (pr ~s:0  ~t Valid)
    | Timeout t -> f (p prover) (limit env t) (pr ~s:1  ~t Timeout)
    | Unknown t -> f (p prover) (limit env t) (pr ~s:1  ~t (Unknown "cached"))
    | Failed -> f (p prover) (limit env 1.0) (pr ~s:2 (Failure "cached"))

let prove env ?name ?cancel ?callback task prover time =
  let entry = task,prover in
  let cached = get entry in
  let promote = match cached with
    | NoResult -> false
    | Failed -> true
    | Unknown _ -> true
    | Valid t -> t <= time
    | Timeout t -> time <= t
  in
  if promote then
    (incr hits ;
     notify env prover callback cached ;
     Fibers.return cached)
  else
    (incr miss ;
     Fibers.map
       (fun result -> set entry result ; result)
       (call_prover env ?name ?cancel ?callback task prover time))

let report_stats () =
  let h = !hits in
  let m = !miss in
  Format.printf "Cache %d/%d@." h (h+m)

(* -------------------------------------------------------------------------- *)
