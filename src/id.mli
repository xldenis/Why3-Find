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
(* --- Why3 Identifiers                                                   --- *)
(* -------------------------------------------------------------------------- *)

type t = Why3.Ident.ident

val hash : t -> int
val equal : t -> t -> bool
val compare : t -> t -> int
val pp : Format.formatter -> t -> unit

val loc : t -> Why3.Loc.position
val file : t -> string
val line : t -> int
val path : ?lib:string list -> t -> string list * string * string list
val cat : string list -> string

type package = [ `Local | `Stdlib | `Package of Meta.pkg ]

type id = {
  id : t ;
  id_pkg : package ;
  id_lib : string list ;
  id_mod : string ;
  id_qid : string list ;
}

val resolve : lib:string list -> t -> id

val of_infix : string -> string
val to_infix : string -> string

val pp_title : Format.formatter -> id -> unit
val pp_aname : Format.formatter -> id -> unit
val pp_ahref : scope:string option -> Format.formatter -> id -> unit

val pp_proof_aname : Format.formatter -> id -> unit
val pp_proof_ahref : Format.formatter -> id -> unit

(* -------------------------------------------------------------------------- *)
