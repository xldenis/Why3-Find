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

module Thy = Why3.Theory
module Pmod = Why3.Pmodule
module Mstr = Why3.Wstdlib.Mstr

type ident = Why3.Ident.ident

type instance = {
  inst_path : string ; (** container of the clone instance *)
  inst_order : int ; (** clone declaration offset *)
  inst_cloned : Thy.theory ; (** cloned theory *)
}

type clone = {
  id_instance : instance ;
  id_source : ident ; (** from cloned theory *)
  id_target : ident ; (** to clone instance *)
}

type theory = {
  path: string ;
  theory: Thy.theory ;
  depends: Thy.theory list ;
  signature: Axioms.signature ;
  clones: clone list ;
  proofs: Crc.crc Mstr.t ;
}

type source = {
  lib: string list;
  urlbase: string;
  profile: Calibration.profile;
  theories: theory Mstr.t;
}

type cenv
val init : unit -> cenv
val set_container : cenv -> path:string -> id:string -> unit
val set_instance : cenv -> ident -> int
val current_instance : cenv -> instance -> bool
val find_clone : cenv ->
  ?source:ident -> ?target:ident ->
  theory -> clone option

val parse : wenv:Why3.Env.env -> cenv:cenv -> henv:Axioms.henv -> string -> source
val empty : unit -> source
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
