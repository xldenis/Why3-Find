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
(* --- Global References                                                  --- *)
(* -------------------------------------------------------------------------- *)

val init : pkgs:string list -> Wenv.env

module Mstr = Why3.Wstdlib.Mstr

type ident = Why3.Ident.ident
val pp_ident : Format.formatter -> ident -> unit

type section = {
  cloned_path : string ;
  cloned_order : int ;
}

type clone = {
  id_section : section ;
  id_source : Why3.Ident.ident ;
  id_target : Why3.Ident.ident ;
}

type theory = {
  theory: Why3.Theory.theory;
  clones: clone list ;
  proofs: Crc.crc Mstr.t ;
}

type source = {
  name: string;
  url: string;
  profile: Calibration.profile;
  theories: theory Mstr.t;
}

val parse : why3env:Why3.Env.env -> string -> source
val derived : source -> string -> string (* URL name *)

val is_keyword : string -> bool

val id_line : ident -> int
val id_name : ident -> string
val id_anchor : ident -> string
val id_path : src:source -> scope:string option -> ident -> string
val id_href : src:source -> scope:string option -> ident -> string

type href =
  | NoRef
  | Def of { name: string ; id: Why3.Ident.ident ; proof: Crc.crc option }
  | Ref of { kind: string ; path: string ; href: string }

type position = Lexing.position * Lexing.position

val resolve :
  src:source -> scope:string option -> theory:theory option -> infix:bool ->
  position -> href

val reference :
  why3env:Why3.Env.env ->
  src:source -> scope:string option ->
  string -> string * ident

(* -------------------------------------------------------------------------- *)
