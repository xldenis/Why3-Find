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

open Wenv
open Why3.Task

type prover
val id : prover -> string
val name : prover -> string
val pp_prover : Format.formatter -> prover -> unit

val default : env -> prover list
val prover : env -> string -> prover
val select : env -> string list -> prover list

type result =
  | NoResult | Failed | Unknown of float | Timeout of float | Valid of float

val pp_time : Format.formatter -> float -> unit
val pp_result : Format.formatter -> result -> unit
val of_json : Json.t -> result
val to_json : result -> Json.t

val prove : env -> unit Fibers.signal ->
  task -> prover -> float -> result Fibers.t



(* -------------------------------------------------------------------------- *)
