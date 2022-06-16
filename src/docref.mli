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

val init : pkgs:string list -> Why3.Env.env

type source = {
  pkg: string; (* package name *)
  name: string; (* library path *)
  url: string; (* URL name *)
}

val parse : why3env:Why3.Env.env -> string -> source
val derived : source -> string -> string (* URL name *)

val is_keyword : string -> bool

type href =
  | NoRef
  | Def of string
  | Ref of string * string

type position = Lexing.position * Lexing.position

val resolve : pkg:string -> infix:bool -> position -> href
val reference : why3env:Why3.Env.env -> pkg:string -> string -> string * string

(* -------------------------------------------------------------------------- *)
