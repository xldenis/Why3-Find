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

let find_exact config s =
  try
    let filter = parse_filter_prover s in
    Some ((filter_one_prover config filter).prover)
  with
  | ProverNotFound _
  | ParseFilterProver _
  | ProverAmbiguity _  ->
    None

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
                name (prover_parseable_format prv) ;
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
(* ---                                                                    --- *)
(* -------------------------------------------------------------------------- *)

(* -------------------------------------------------------------------------- *)
