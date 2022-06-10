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

val is_keyword : string -> bool

type href =
  | NoRef
  | Thy of string
  | Def of string
  | Ref of string * string

val resolve : pkg:string -> (Lexing.position * Lexing.position) -> href

val init : pkgs:string list -> Why3.Env.env

type source = {
  pkg: string; (* package name *)
  url: string; (* URL basename *)
}

val parse : env:Why3.Env.env -> string -> source

(* -------------------------------------------------------------------------- *)
