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
(* --- Dump AST                                                           --- *)
(* -------------------------------------------------------------------------- *)

open Why3
type 'a printer = Format.formatter -> 'a -> unit

val pp_id : Ident.ident printer

val pp_term : Term.term printer

val pp_pvsymbol_use : Ity.pvsymbol printer
val pp_pvsymbol_def : Ity.pvsymbol printer

val pp_ity : Ity.ity printer
val pp_cty : Ity.cty printer

val pp_rs_kind : Expr.rs_kind printer
val pp_rsymbol_use : Expr.rsymbol printer
val pp_rsymbol_def : Expr.rsymbol printer

val pp_expr : Expr.expr printer
val pp_cexp : Expr.cexp printer

(* -------------------------------------------------------------------------- *)
