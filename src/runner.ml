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

let id prv = prover_parseable_format prv.config.prover
let name prv = String.lowercase_ascii @@ prv.config.prover.prover_name

let pp_prover fmt prv = Format.pp_print_string fmt @@ id prv

let find_exact (env : Wenv.env) s =
  try
    let filter = parse_filter_prover s in
    let config = filter_one_prover env.config filter in
    let driver =
      try load_driver (get_main env.config) env.env config
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

let pp_time fmt t =
  if t < 1e-3 then Format.fprintf fmt "%dns" (int_of_float @@ t *. 1e6) else
  if t < 1.0 then Format.fprintf fmt "%dms" (int_of_float @@ t *. 1e3) else
    Format.fprintf fmt "%ds" (int_of_float @@ t)

let pp_result fmt = function
  | NoResult -> Format.pp_print_string fmt "No Result"
  | Failed -> Format.pp_print_string fmt "Failed"
  | Unknown t -> Format.fprintf fmt "Unknown (%a)" pp_time t
  | Timeout t -> Format.fprintf fmt "Timeout (%a)" pp_time t
  | Valid t -> Format.fprintf fmt "Valid (%a)" pp_time t

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
  Printf.sprintf ".why3find/%s/%s.json"
    (id p) Why3.Termcode.(task_checksum t |> string_of_checksum)

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

let get e =
  try Cache.find hash e with Not_found ->
    let r = read e in Cache.add hash e r ; r

let set e r =
  match r with
  | NoResult -> ()
  | Failed -> Cache.replace hash e r
  | Valid _ | Unknown _ | Timeout _ -> Cache.replace hash e r ; write e r

(* -------------------------------------------------------------------------- *)
(* --- Running Prover                                                     --- *)
(* -------------------------------------------------------------------------- *)

let jobs = ref 0

let call_prover (env : Wenv.env) (cancel : unit Fibers.signal)
    (task : task) (prover : prover) (time : float) =
  let main = get_main env.config in
  let limit = { empty_limit with limit_time = int_of_float (time +. 0.5) } in
  let timeout = ref 0.0 in
  let np = !jobs in
  if np > 0 then
    (jobs := 0 ; Why3.Prove_client.set_max_running_provers np) ;
  let call = prove_task
      ~command:prover.config.command
      ~libdir:(libdir main)
      ~datadir:(datadir main)
      ~limit prover.driver task in
  let kill () = timeout := 0.0 ; interrupt_call ~libdir:(libdir main) call in
  Fibers.hook cancel kill Fibers.async
    begin fun () ->
      let timer = !timeout in
      if 0.0 < timer && timer < Unix.gettimeofday () then kill () ;
      match query_call call with
      | NoUpdates | ProverInterrupted -> None
      | ProverStarted ->
        if timer = 0.0 then timeout := Unix.gettimeofday () +. time ;
        None
      | InternalFailure _ -> Some Failed
      | ProverFinished pr ->
        Some begin
          match pr.pr_answer with
          | Valid -> Valid pr.pr_time
          | Timeout -> Timeout time
          | Invalid | Unknown _ | OutOfMemory | StepLimitExceeded ->
            Unknown pr.pr_time
          | HighFailure | Failure _ -> Failed
        end
    end

(* -------------------------------------------------------------------------- *)
(* --- Running Prover with Cache                                          --- *)
(* -------------------------------------------------------------------------- *)

let hits = ref 0
let miss = ref 0

let prove env cancel task prover time =
  let e = task,prover in
  let r = get e in
  let cached = match r with
    | NoResult -> false
    | Failed -> true
    | Unknown _ -> true
    | Valid t -> t <= time
    | Timeout t -> time <= t
  in
  if cached then
    (incr hits ; Fibers.return r)
  else
    (incr miss ;
     Fibers.map
       (fun r -> set e r ; r)
       (call_prover env cancel task prover time))

let report_stats () =
  let h = !hits in
  let m = !miss in
  Format.printf "%d/%d cached@." h (h+m)

(* -------------------------------------------------------------------------- *)
