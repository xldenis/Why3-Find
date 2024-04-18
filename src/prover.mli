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

(** Types representing provers and associated operations *)

(** {1 Prover descriptions} *)

type prover_desc
(** The type of prover descriptions, identifying a unique prover version. *)

exception InvalidPattern of string
exception InvalidProverDescription of string
(** Raised on invalid pattern or prover description, see below. The argument
    is the erroneous string. *)

val desc_to_string : prover_desc -> string
val desc_of_string : string -> prover_desc
(** Converts prover descriptions to and from string. The syntax is the same as
    patterns below, but the version part is mandatory.
    @raise InvalidProverDescription when the string description is invalid. *)

val desc_name : prover_desc -> string
val desc_version : prover_desc -> string

val pp_desc : Format.formatter -> prover_desc -> unit

val compare_desc : prover_desc -> prover_desc -> int
(** Prover descriptions comparison function. Different provers are ordered
    alphabetically by names, and different version of a same prover by
    {b reverse} version order. Thus, when sorting, the latest version come
    {b before} older ones. *)


(** {1 Provers} *)

type prover = private {
  desc : prover_desc ;
  config : Why3.Whyconf.config_prover ;
  driver : Why3.Driver.driver ;
}
(** The type for provers  *)

val why3_desc : prover -> string
(** The prover descriptions in Why3 syntax. *)

val name : prover -> string
val version : prover -> string
val fullname : prover -> string
val infoname : prover -> string

val pp_prover : Format.formatter -> prover -> unit
(** Pretty print the given prover *)

(** {1 Patterns} *)

(** Patterns identify provers. The syntax is [prover-name@prover-version].
    The [prover-name] match the prover name case-insensitively. (eg [alt-ergo]
    match the prover of Why3 name [Alt-Ergo])
    The version part, starting from the '@', is optional. If omitted, it
    generally means the last available version. *)

val pattern_name : string -> string
val pattern_version : string -> string option

val pmatch : pattern:string -> prover_desc -> bool
(** Whether the given pattern match the prover description.
    @raise InvalidPattern when the pattern is invalid. *)

(** {1 Retrieving available provers} *)

val all : Wenv.env -> prover list
(** Returns the set of all available provers from the Why3 environment. *)

val prover : Wenv.env -> prover_desc -> prover
(** Gets a prover from its description. Raises [Not_found] if not available. *)

val select : Wenv.env -> patterns:string list -> prover list
(** Returns the set of available provers that match the [patterns].
    For each [pattern] in order, if the pattern is invalid or the prover
    unavailable, a warning is emitted, otherwise one prover is appended to the
    return list, either the one specified or the latest available version in
    case of version-less pattern. *)

val default : Wenv.env -> prover list
(** Returns the set of available "default" provers *)

val check_and_get_prover : Wenv.env -> patterns:string list -> prover_desc
  -> prover option
(** Returns the prover if available and selected by the [patterns]. Warn if
    the prover is not found. We assume the [patterns] have been [select]ed so
    we don't warn if an unavailable prover match the [patterns] *)
