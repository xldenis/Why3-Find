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
(* --- Compute Axioms                                                     --- *)
(* -------------------------------------------------------------------------- *)

type henv
val init : Wenv.env -> henv

open Why3

type signature
val signature : henv -> Theory.theory -> signature

type kind =
  | Type (* abstract type *)
  | Logic (* constant, function of predicate *)
  | Axiom (* hypothesis *)
  | Param (* non-constrained val *)
  | Value (* constrained val *)
  | Unsafe

type parameter = {
  kind : kind ;
  name : Ident.ident ;
  builtin : (Prover.prover * string) list ;
  extern : string option ;
}

val is_free : parameter -> bool
val is_unsafe : parameter -> bool
val is_external : parameter -> bool

val parameter : signature -> Ident.ident -> parameter option
val parameters : signature -> parameter list
val iter : henv -> ?self:bool -> (parameter -> unit) -> Theory.theory list -> unit

(* -------------------------------------------------------------------------- *)
