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
(* --- Why3 Provers                                                       --- *)
(* -------------------------------------------------------------------------- *)

open Why3.Whyconf
open Why3.Driver
open Why3.Task
open Why3.Call_provers

type prover = {
  config : config_prover ;
  driver : driver ;
}

let find_exact (env : Env.env) s =
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

let find_default config name =
  match find_exact config name with
  | Some prv -> [prv]
  | None -> []

let prover config name =
  try
    match find_exact config name with
    | Some prv -> prv
    | None ->
      match find_exact config (String.lowercase_ascii name) with
      | Some prv -> prv
      | None ->
        match String.split_on_char ',' name with
        | shortname :: _ :: _ ->
          begin
            match find_exact config (String.lowercase_ascii shortname) with
            | Some prv ->
              Format.eprintf "Warning: prover %S not found, fallback to %s.@."
                name (prover_parseable_format prv.config.prover) ;
              prv
            | None -> raise Not_found
          end
        | _ -> raise Not_found
  with Not_found ->
    Format.eprintf "Error: prover %S not found." name ;
    exit 2

let default config =
  find_default config "Alt-Ergo" @
  find_default config "Z3" @
  find_default config "CVC4"

(* -------------------------------------------------------------------------- *)
(* --- Prover Result                                                      --- *)
(* -------------------------------------------------------------------------- *)

type result =
  | NoResult | Failed | Unknown of float | Timeout of float | Valid of float

let of_json (js : Json.t) =
  let open Json in
  let status = try jfield "status" js |> jstring with Not_found -> "" in
  let time = try jfield "status" js |> jfloat with Not_found -> 0.0 in
  match status with
  | "NoResult" -> NoResult
  | "Failed" -> Failed
  | "Unknown" -> Unknown time
  | "Timeout" -> Timeout time
  | "Valid" -> Valid time
  | _ -> raise Not_found

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
(* --- Running Prover                                                     --- *)
(* -------------------------------------------------------------------------- *)

type process = {
  cancel : unit -> unit ;
  status : unit -> result option ;
}

let run (env : Env.env) (task : task) (prover : prover) (time : float) =
  let main = get_main env.config in
  let limit = { empty_limit with limit_time = int_of_float (time +. 0.5) } in
  let timeout = Unix.gettimeofday () +. time in
  let call = prove_task
      ~command:prover.config.command
      ~libdir:(libdir main)
      ~datadir:(datadir main)
      ~limit prover.driver task in
  let cancel () = interrupt_call ~libdir:(libdir main) call in
  let status () =
    match query_call call with
    | ProverStarted -> None
    | InternalFailure _ -> Some Failed
    | ProverInterrupted ->
      Some (if Unix.gettimeofday () > timeout then Timeout time else NoResult)
    | NoUpdates ->
      if Unix.gettimeofday () > timeout then cancel () ; None
    | ProverFinished pr ->
      Some begin
        match pr.pr_answer with
        | Valid -> Valid pr.pr_time
        | Timeout -> Timeout time
        | Invalid | Unknown _ | OutOfMemory | StepLimitExceeded ->
          Unknown pr.pr_time
        | HighFailure | Failure _ -> Failed
      end
  in { cancel ; status }

(* -------------------------------------------------------------------------- *)
