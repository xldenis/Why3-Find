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

type prover = {
  config : Why3.Whyconf.config_prover ;
  driver : Why3.Driver.driver ;
}

type result =
  | NoResult | Failed | Unknown of float | Timeout of float | Valid of float

val id : prover -> string
val name : prover -> string
val version : prover -> string
val fullname : prover -> string
val infoname : prover -> string

val all : env -> prover list
val find : env -> pattern:string -> prover option
val prover : env -> id:string -> prover
val select : env -> patterns:string list -> prover list
val default : env -> prover list

val map : (float -> float) -> result -> result
val crop : timeout:float -> result -> result option
val definitive : timeout:float -> result -> bool
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
  prover -> Why3.Task.task -> float -> result Fibers.t

type prooftask

val digest : prooftask -> string
val data : prover -> prooftask -> string

val prove_cached : prover -> Why3.Task.task -> float ->
  [ `Cached of result | `Prepared of prooftask ]

val prove_prepared : env ->
  ?name:string ->
  ?cancel:unit Fibers.signal ->
  ?callback:callback ->
  prover -> prooftask -> float -> result Fibers.t

val prove_buffer : env ->
  ?cancel:unit Fibers.signal ->
  prover -> Buffer.t -> float -> result Fibers.t

val notify : env -> prover -> result -> callback -> unit
val update : prover -> prooftask -> result -> unit

val options : (string * Arg.spec * string) list
val pending : unit -> int
val running : unit -> int
val maxjobs : Wenv.env -> int
val pp_goals : Format.formatter -> unit

val is_modified : unit -> bool
val memlimit : env -> int
val save_config : env -> unit
val print_stats : unit -> unit

(* -------------------------------------------------------------------------- *)
