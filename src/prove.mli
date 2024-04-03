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
(* --- Proof Manager                                                      --- *)
(* -------------------------------------------------------------------------- *)

module M = Why3.Wstdlib.Mstr

type profile = Calibration.profile
type theories = Crc.crc M.t M.t

val proofs_file : string -> string
(** Returns the proofs file associated with the given mlw file *)

val load_proofs : ?defaultprofile:profile -> string -> profile * theories
(** Load the content of the proofs file associated with the given mlw file *)

type mode = [ `Force | `Update | `Minimize | `Replay ]
type log = [
    `Default | `Modules | `Theories | `Goals | `Proofs | `Context of int
]

val stdlib : bool ref
val externals : bool ref
val builtins : bool ref

type outcome = {
  provers : Prover.prover list ;
  tactics : string list ;
  time : int ;
  mem : int ;
  unfixed : string list ;
}

val prove_files :
  mode:mode ->
  session:bool ->
  log:log ->
  axioms:bool ->
  files:string list ->
  outcome
