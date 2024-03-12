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

type prover_desc

val desc_to_string : prover_desc -> string
val desc_of_string : string -> prover_desc

val desc_name : prover_desc -> string

val pp_desc : Format.formatter -> prover_desc -> unit

val compare_desc : prover_desc -> prover_desc -> int

type prover = private {
  desc : prover_desc ;
  config : Why3.Whyconf.config_prover ;
  driver : Why3.Driver.driver ;
}

val why3_desc : prover -> string
val name : prover -> string
val version : prover -> string
val fullname : prover -> string
val infoname : prover -> string

val all : Wenv.env -> prover list
val find : Wenv.env -> pattern:string -> prover option
val prover : Wenv.env -> prover_desc -> prover
val select : Wenv.env -> patterns:string list -> prover list
val default : Wenv.env -> prover list

val pp_prover : Format.formatter -> prover -> unit
