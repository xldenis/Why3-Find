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

type prover = {
  config : Why3.Whyconf.config_prover ;
  driver : Why3.Driver.driver ;
}

type result =
  | NoResult | Failed | Unknown of float | Timeout of float | Valid of float

val id : prover -> string
val name : prover -> string
val title : ?strict:bool -> prover -> string
val relax : string -> string
val relaxed : string -> bool

val all : env -> prover list
val default : env -> prover list
val prover : env -> string -> prover
val select : env -> string list -> prover list
val fits : timeout:float -> result -> bool
val merge : result -> result -> result

val pp_prover : Format.formatter -> prover -> unit
val pp_result : Format.formatter -> result -> unit

val of_json : Json.t -> result
val to_json : result -> Json.t

type callback =
  Why3.Whyconf.prover ->
  Why3.Call_provers.resource_limit ->
  Why3.Call_provers.prover_result ->
  unit

val prove : env ->
  ?name:string ->
  ?cancel:unit Fibers.signal ->
  ?callback:callback ->
  task -> prover -> float -> result Fibers.t

val options : (string * Arg.spec * string) list
val pending : unit -> int
val running : unit -> int
val maxjobs : Wenv.env -> int
val pp_goals : Format.formatter -> unit

val is_modified : unit -> bool
val save_config : env -> unit
val print_stats : unit -> unit

(* -------------------------------------------------------------------------- *)
