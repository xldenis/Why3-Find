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

open Ppxlib

type kind = Type | Value

let resolve ~kind id =
  match kind with
  | Type -> "type:" ^ id
  | Value -> "value:" ^ id

(* -------------------------------------------------------------------------- *)
(* --- Type Rule                                                          --- *)
(* -------------------------------------------------------------------------- *)

let expand_type ~ctxt (id: string) =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  match resolve ~kind:Type id with
  | value -> Ast_builder.Default.ptyp_var ~loc value
  | exception Not_found ->
    let ext =
      Location.error_extensionf ~loc "Why3 identifier %S not found" id
    in
    Ast_builder.Default.ptyp_extension ~loc ext

let type_rule =
  Extension.V3.declare "why3"
    Extension.Context.core_type
    Ast_pattern.(single_expr_payload (estring __))
    expand_type

(* -------------------------------------------------------------------------- *)
(* --- Expression Rule                                                    --- *)
(* -------------------------------------------------------------------------- *)

let expand_expr ~ctxt (id: string) =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  match resolve ~kind:Value id with
  | value -> Ast_builder.Default.estring ~loc value
  | exception Not_found ->
    let ext =
      Location.error_extensionf ~loc "Why3 identifier %S not found" id
    in
    Ast_builder.Default.pexp_extension ~loc ext

let expr_rule =
  Extension.V3.declare "why3"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    expand_expr

(* -------------------------------------------------------------------------- *)
(* --- PPX Registration                                                   --- *)
(* -------------------------------------------------------------------------- *)

let () = Driver.register_transformation ~rules:[
    Ppxlib.Context_free.Rule.extension expr_rule ;
    Ppxlib.Context_free.Rule.extension type_rule ;
  ] "why3"

(* -------------------------------------------------------------------------- *)
