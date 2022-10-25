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

val init : unit -> Why3.Env.env * Axioms.henv

module Mstr = Why3.Wstdlib.Mstr

type ident = Why3.Ident.ident

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
  signature: Axioms.signature;
  clones: clone list ;
  proofs: Crc.crc Mstr.t ;
}

type source = {
  lib: string list;
  urlbase: string;
  profile: Calibration.profile;
  theories: theory Mstr.t;
}

val empty : unit -> source
val parse : wenv:Why3.Env.env -> henv:Axioms.henv -> string -> source
val derived : source -> string -> string (* URL name *)

val is_keyword : string -> bool

type href =
  | NoRef
  | Ref of Id.id
  | Def of Id.id * Crc.crc option

type position = Lexing.position * Lexing.position

val find_proof : ident -> theory option -> Crc.crc option

val resolve :
  src:source -> theory:theory option -> infix:bool ->
  position -> href

val reference :
  wenv:Why3.Env.env ->
  src:source -> scope:string option ->
  string -> string * ident

(* -------------------------------------------------------------------------- *)
