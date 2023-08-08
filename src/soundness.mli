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
(** Compute Parameterized Module Soundness *)
(* -------------------------------------------------------------------------- *)

type env

val init : unit -> env
val register : env -> Docref.source -> unit

type soundness =
  | Unsound
  | Sound of Docref.instance list
  | Unknown of Docref.instance list

val clone : soundness
val unknown : soundness
val merge : soundness -> soundness -> soundness

val is_clone : soundness -> bool
val is_sound : soundness -> bool
val is_unsound : soundness -> bool
val compute : env -> Docref.theory -> soundness

(* -------------------------------------------------------------------------- *)
