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

type 'a scope = (string,loc:Location.t -> lid:Location.t -> 'a) Hashtbl.t
let types : (core_type list -> core_type) scope = Hashtbl.create 0
let values : expression scope = Hashtbl.create 0
let fields : (expression -> expression) scope = Hashtbl.create 0
let constr : (expression option -> expression) scope = Hashtbl.create 0
let pattern : (pattern option -> pattern) scope = Hashtbl.create 0

(* -------------------------------------------------------------------------- *)
(* --- Using Module                                                       --- *)
(* -------------------------------------------------------------------------- *)

let mk_type ~lident ~loc ~lid ts =
  Ast_builder.Default.ptyp_constr ~loc { loc = lid ; txt = lident } ts

let mk_value ~lident ~loc ~lid =
  Ast_builder.Default.pexp_ident ~loc { loc = lid ; txt = lident }

let mk_field ~lident ~loc ~lid exp =
  Ast_builder.Default.pexp_field ~loc exp { loc = lid ; txt = lident }

let mk_constr ~lident ~loc ~lid prm =
  Ast_builder.Default.pexp_construct ~loc { loc = lid ; txt = lident } prm

let mk_pattern ~lident ~loc ~lid prm =
  Ast_builder.Default.ppat_construct ~loc { loc = lid ; txt = lident } prm

let lookup ~pkg jfile =
  if Sys.file_exists jfile then Some jfile else
    List.find_map
      (fun d ->
         let jfile = Filename.concat d @@ Filename.concat pkg @@ jfile in
         if Sys.file_exists jfile then Some jfile else None)
    @@ Global.Sites.packages

let load_module ~qid ?name () =
  let path = String.split_on_char '.' qid in
  let basename = String.concat "__" path in
  let pkg = List.hd path in
  let jfile = Filename.concat "lib" basename ^ ".json" in
  Format.printf "CWD %s@." @@ Sys.getcwd () ;
  Format.printf "LOOKUP %s / %s / %b@." pkg jfile (Sys.file_exists jfile) ;
  let js = Json.of_file @@
    match lookup ~pkg jfile with
    | None -> raise Not_found
    | Some f -> f in
  let omodule = String.capitalize_ascii basename in
  let emodule = match name with Some e -> e | None ->
    String.capitalize_ascii @@ List.hd @@ List.rev path in
  Format.printf "JLIST@." ;
  List.iter
    (fun js ->
       try
         let kind = Json.jfield_exn "kind" js |> Json.jstring in
         let ident = Json.jfield_exn "ident" js |> Json.jstring in
         let oname = Json.jfield_exn "oname" js |> Json.jstring in
         let oident = Printf.sprintf "%s.%s" omodule oname in
         let lident = Astlib.Longident.parse oident in
         let add scope mk =
           let m = mk ~lident in
           Hashtbl.replace scope ident m ;
           Hashtbl.replace scope (Printf.sprintf "%s.%s" emodule ident) m
         in match kind with
         | "type" -> add types mk_type
         | "val" -> add values mk_value
         | "field" -> add fields mk_field
         | "constr" -> add constr mk_constr ; add pattern mk_pattern
         | _ -> ()
       with Not_found -> ()
    ) (Json.jlist js)

(* -------------------------------------------------------------------------- *)
(* --- Symbol Resolution                                                  --- *)
(* -------------------------------------------------------------------------- *)

let resolve (type a) ~(scope: a scope) ~loc ~lid : a =
  let qid = String.concat "." (Astlib.Longident.flatten lid.txt) in
  Hashtbl.find scope qid ~loc ~lid:lid.loc

(* -------------------------------------------------------------------------- *)
(* --- Errors                                                             --- *)
(* -------------------------------------------------------------------------- *)

let error ~lid exn =
  let loc = lid.loc in
  let qid = String.concat "." @@ Astlib.Longident.flatten lid.txt in
  match exn with
  | Not_found ->
    Location.error_extensionf ~loc "Why3 identifier %S not found" qid
  | Failure msg ->
    Location.error_extensionf ~loc "Why3 identifier %S is invalid (%s)" qid msg
  | exn ->
    Location.error_extensionf ~loc "Why3 identifier %S parse error (%s)" qid
      (Printexc.to_string exn)

(* -------------------------------------------------------------------------- *)
(* --- Expression Resolution                                              --- *)
(* -------------------------------------------------------------------------- *)

let resolve_expression ~loc (exp: expression) : expression =
  match exp.pexp_desc with
  | Pexp_ident lid ->
    begin
      try resolve ~scope:values ~loc ~lid
      with exn -> Ast_builder.Default.pexp_extension ~loc @@ error ~lid exn
    end
  | Pexp_field(exp,lid) ->
    begin
      try resolve ~scope:fields ~loc ~lid exp with exn ->
        Ast_builder.Default.pexp_extension ~loc @@ error ~lid exn
    end
  | Pexp_construct(lid,prm) ->
    begin
      try resolve ~scope:constr ~loc ~lid prm with exn ->
        Ast_builder.Default.pexp_extension ~loc @@ error ~lid exn
    end
  | _ ->
    Ast_builder.Default.pexp_extension ~loc @@
    Location.error_extensionf ~loc "Can not resolve such a Why3 expression"

(* -------------------------------------------------------------------------- *)
(* --- Use Rules                                                          --- *)
(* -------------------------------------------------------------------------- *)

let re = Str.regexp " *as *"

let use import =
  match Str.split_delim re import with
  | [qid] -> load_module ~qid ()
  | [qid;name] -> load_module ~qid ~name ()
  | _ -> ()

let use_rule_sig =
  Extension.V3.declare "why3use"
    Extension.Context.signature_item
    Ast_pattern.(single_expr_payload (estring __'))
    begin fun ~ctxt import ->
      let loc = Expansion_context.Extension.extension_point_loc ctxt in
      try
        use import.txt ;
        Ast_builder.Default.psig_attribute ~loc {
          attr_loc = loc ;
          attr_name = { loc ; txt = "ignore" } ;
          attr_payload = PSig [] ;
        }
      with _exn ->
        Ast_builder.Default.psig_extension ~loc
          (Location.error_extensionf
             ~loc:import.loc "Invalid Why-3 use %S" import.txt) []
    end

let use_rule_struct =
  Extension.V3.declare "why3use"
    Extension.Context.structure_item
    Ast_pattern.(single_expr_payload (estring __))
    begin fun ~ctxt import ->
      let loc = Expansion_context.Extension.extension_point_loc ctxt in
      try
        use import ;
        Ast_builder.Default.pstr_attribute ~loc {
          attr_loc = loc ;
          attr_name = { loc ; txt = "ignore" } ;
          attr_payload = PStr [] ;
        }
      with _exn ->
        Ast_builder.Default.pstr_extension ~loc
          (Location.error_extensionf ~loc "Invalid Why-3 use %S" import) []
    end

(* -------------------------------------------------------------------------- *)
(* --- Type Rule                                                          --- *)
(* -------------------------------------------------------------------------- *)

let type_rule =
  Extension.V3.declare "why3"
    Extension.Context.core_type
    Ast_pattern.(ptyp (ptyp_constr __' __))
    begin fun ~ctxt lid cts ->
      let loc = Expansion_context.Extension.extension_point_loc ctxt in
      try resolve ~scope:types ~loc ~lid cts with exn ->
        Ast_builder.Default.ptyp_extension ~loc @@ error ~lid exn
    end

(* -------------------------------------------------------------------------- *)
(* --- Expression Rule                                                    --- *)
(* -------------------------------------------------------------------------- *)

let expr_rule =
  Extension.V3.declare "why3"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    begin fun ~ctxt exp ->
      let loc = Expansion_context.Extension.extension_point_loc ctxt in
      resolve_expression ~loc exp
    end

(* -------------------------------------------------------------------------- *)
(* --- Pattern Rule                                                       --- *)
(* -------------------------------------------------------------------------- *)

let pattern_rule =
  Extension.V3.declare "why3"
    Extension.Context.pattern
    Ast_pattern.(ppat (ppat_construct __' __) __)
    begin fun ~ctxt lid lprm _when ->
      let loc = Expansion_context.Extension.extension_point_loc ctxt in
      let prm = Option.map snd lprm in
      try resolve ~scope:pattern ~loc ~lid prm with exn ->
        Ast_builder.Default.ppat_extension ~loc @@ error ~lid exn
    end

(* -------------------------------------------------------------------------- *)
(* --- PPX Registration                                                   --- *)
(* -------------------------------------------------------------------------- *)

let () = Driver.register_transformation
    ~rules:(List.map Ppxlib.Context_free.Rule.extension [
        use_rule_sig ;
        use_rule_struct ;
        type_rule ;
        expr_rule ;
        pattern_rule ;
      ]) "why3find"

(* -------------------------------------------------------------------------- *)
