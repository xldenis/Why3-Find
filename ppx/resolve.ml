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

(* -------------------------------------------------------------------------- *)
(* --- Symbol Maps                                                        --- *)
(* -------------------------------------------------------------------------- *)

let modules : (string,unit) Hashtbl.t = Hashtbl.create 16

type 'a scope = (string,Location.t -> 'a) Hashtbl.t
let types : core_type scope = Hashtbl.create 256
let values : expression scope = Hashtbl.create 256

(* -------------------------------------------------------------------------- *)
(* --- Symbol Loafind                                                     --- *)
(* -------------------------------------------------------------------------- *)

let load_module _js = ()

(* -------------------------------------------------------------------------- *)
(* --- Symbol Resolution                                                  --- *)
(* -------------------------------------------------------------------------- *)

let is_uident p =
  String.length p > 0 &&
  let c = p.[0] in Char.lowercase_ascii c = c

let mpath qid =
  let rec unwrap rp = function
    | [] -> failwith "module path missing"
    | p::ps -> if is_uident p then unwrap (p::rp) ps else List.rev rp,ps
  in unwrap [] @@ String.split_on_char '.' qid

let resolve (type a) ~(scope: a scope) ~(loc : Location.t) (qid : string) : a =
  try Hashtbl.find scope qid @@ loc
  with Not_found ->
    let p,q = mpath qid in
    try
      let js = String.concat "__" p ^ ".json" in
      if Hashtbl.mem modules js then raise Not_found ;
      load_module js ;
      Hashtbl.find scope qid @@ loc
    with Not_found ->
      Utils.failwith "value '%s' not found in module '%s'"
        (String.concat "." q) (String.concat "." p)

(* -------------------------------------------------------------------------- *)
(* --- Errors                                                             --- *)
(* -------------------------------------------------------------------------- *)

let error ~loc id exn =
  match exn with
  | Not_found ->
    Location.error_extensionf ~loc "Why3 identifier %S not found" id
  | Failure msg ->
    Location.error_extensionf ~loc "Why3 identifier %S is invalid (%s)" id msg
  | exn ->
    Location.error_extensionf ~loc "Resolution error (%S, %s)" id
      (Printexc.to_string exn)

(* -------------------------------------------------------------------------- *)
(* --- Type Rule                                                          --- *)
(* -------------------------------------------------------------------------- *)

let expand_type ~ctxt (id: string) =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  try resolve ~scope:types ~loc id with exn ->
    Ast_builder.Default.ptyp_extension ~loc @@ error ~loc id exn

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
  try resolve ~scope:values ~loc id with exn ->
    Ast_builder.Default.pexp_extension ~loc @@ error ~loc id exn

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
