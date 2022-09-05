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

let id prv = prover_parseable_format prv.config.prover
let pretty fmt prv = Format.pp_print_string fmt @@ id prv

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
                name pretty prv ;
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

let prove (env : Wenv.env) (cancel : unit Fibers.signal)
    (task : task) (prover : prover) (time : float) =
  let main = get_main env.config in
  let limit = { empty_limit with limit_time = int_of_float (time +. 0.5) } in
  let timeout = ref 0.0 in
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
