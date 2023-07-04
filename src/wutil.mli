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

open Why3

val iter_sm : (Ident.ident -> Ident.ident -> unit) -> Theory.symbol_map -> unit
val iter_mi : (Ident.ident -> Ident.ident -> unit) -> Pmodule.mod_inst -> unit

val pp_thy : Format.formatter -> Theory.theory -> unit
val pp_mod : Format.formatter -> Pmodule.pmodule -> unit
val pp_pdecl : Format.formatter -> Pdecl.pdecl -> unit
val pp_munit : Format.formatter -> Pmodule.mod_unit -> unit
val pp_module : Format.formatter -> Pmodule.pmodule -> unit

(**************************************************************************)
