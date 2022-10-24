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

let modules : (string,unit) Hashtbl.t = Hashtbl.create 0

type 'a scope = (string,loc:Location.t -> lid:Location.t -> 'a) Hashtbl.t
let types : (core_type list -> core_type) scope = Hashtbl.create 0
let constr : (expression option -> expression) scope = Hashtbl.create 0
let values : expression scope = Hashtbl.create 0

(* -------------------------------------------------------------------------- *)
(* --- Symbol Loadind                                                     --- *)
(* -------------------------------------------------------------------------- *)

let mk_val ~lident ~loc ~lid =
  Ast_builder.Default.pexp_ident ~loc { loc = lid ; txt = lident }

let mk_type ~lident ~loc ~lid ts =
  Ast_builder.Default.ptyp_constr ~loc { loc = lid ; txt = lident } ts

let mk_constr ~lident ~loc ~lid prm =
  Ast_builder.Default.pexp_construct ~loc { loc = lid ; txt = lident } prm

let load_module ~package ~basename =
  let js = Json.of_file (basename ^ ".json") in
  let omodule = String.capitalize_ascii basename in
  List.iter
    (fun js ->
       try
         let kind = Json.jfield "kind" js |> Json.jstring in
         let ident = Json.jfield "ident" js |> Json.jstring in
         let oname = Json.jfield "oname" js |> Json.jstring in
         let wident = Printf.sprintf "%s.%s" package ident in
         let oident = Printf.sprintf "%s.%s" omodule oname in
         let lident = Astlib.Longident.parse oident in
         match kind with
         | "val" -> Hashtbl.replace values wident (mk_val ~lident)
         | "type" -> Hashtbl.replace types wident (mk_type ~lident)
         | "constr" -> Hashtbl.replace constr wident (mk_constr ~lident)
         | _ -> ()
       with Not_found -> ()
    ) (Json.jlist js)

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

let resolve (type a) ~(scope: a scope) ~loc ~lid : a =
  let qid = String.concat "." (Astlib.Longident.flatten lid.txt) in
  let lid = lid.loc in
  try Hashtbl.find scope qid ~loc ~lid
  with Not_found ->
    let p,q = mpath qid in
    try
      let package = String.concat "." p in
      let basename = String.concat "__" p in
      if Hashtbl.mem modules package then raise Not_found ;
      Hashtbl.add modules package () ;
      load_module ~package ~basename ;
      Hashtbl.find scope qid ~loc ~lid
    with Not_found ->
      Utils.failwith "value '%s' not found in module '%s'"
        (String.concat "." q) (String.concat "." p)

(* -------------------------------------------------------------------------- *)
(* --- Errors                                                             --- *)
(* -------------------------------------------------------------------------- *)

let error ~loc lid exn =
  let id = String.concat "." @@ Astlib.Longident.flatten lid in
  match exn with
  | Not_found ->
    Location.error_extensionf ~loc "Why3 identifier %S not found" id
  | Failure msg ->
    Location.error_extensionf ~loc "Why3 identifier %S is invalid (%s)" id msg
  | exn ->
    Location.error_extensionf ~loc "Why3 identifier %S error (%s)" id
      (Printexc.to_string exn)

(* -------------------------------------------------------------------------- *)
(* --- Type Rule                                                          --- *)
(* -------------------------------------------------------------------------- *)

let expand_type ~ctxt lid cts =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  try resolve ~scope:types ~loc lid cts with exn ->
    Ast_builder.Default.ptyp_extension ~loc @@ error ~loc lid exn

let type_rule =
  Extension.V3.declare "why3"
    Extension.Context.core_type
    Ast_pattern.(ptyp (ptyp_constr __ __))
    expand_type

(* -------------------------------------------------------------------------- *)
(* --- Expression Rule                                                    --- *)
(* -------------------------------------------------------------------------- *)

let expand_expr ~ctxt lid =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  try resolve ~scope:values ~loc lid with exn ->
    Ast_builder.Default.pexp_extension ~loc @@ error ~loc lid exn

let expr_rule =
  Extension.V3.declare "why3"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload (pexp_ident __))
    expand_expr

(* -------------------------------------------------------------------------- *)
(* --- PPX Registration                                                   --- *)
(* -------------------------------------------------------------------------- *)

let () = Driver.register_transformation ~rules:[
    Ppxlib.Context_free.Rule.extension expr_rule ;
    Ppxlib.Context_free.Rule.extension type_rule ;
  ] "why3"

(* -------------------------------------------------------------------------- *)
