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
(* --- Session Management                                                 --- *)
(* -------------------------------------------------------------------------- *)

open Why3

type session

val create :
  session:bool ->
  dir:string ->
  file:string ->
  format:string ->
  Why3.Theory.theory list ->
  session

val save : session -> unit

type theory

val name : theory -> string
val theories : session -> theory list

type goal

val split : theory -> goal list
val goal_name : goal -> string
val goal_loc : goal -> Loc.position option
val goal_task : goal -> Task.task

val thy_name : Theory.theory -> string
val task_name : Task.task -> string

val result : goal ->
  Whyconf.prover ->
  Call_provers.resource_limit ->
  Call_provers.prover_result ->
  unit

val apply : Env.env -> string -> goal -> goal list option

(* -------------------------------------------------------------------------- *)
