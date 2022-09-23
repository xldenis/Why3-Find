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
(* --- Why3 Identifiers                                                   --- *)
(* -------------------------------------------------------------------------- *)

module I = Why3.Ident

type t = I.ident
let hash = I.id_hash
let equal = I.id_equal
let compare = I.id_compare
let pp fmt (id : t) =
  Format.fprintf fmt "%s<%d>" id.id_string (Why3.Weakhtbl.tag_hash id.id_tag)

(* Location *)

let loc (id : t) =
  match id.id_loc with
  | None -> raise Not_found
  | Some loc -> loc

let line id =
  let _,line,_,_ = Why3.Loc.get (loc id) in line

let file id =
  let file,_,_,_ = Why3.Loc.get (loc id) in file

let path ?lib id =
  let path =
    try Why3.Pmodule.restore_path id
    with Not_found -> Why3.Theory.restore_path id
  in match lib with
  | None -> path
  | Some lib ->
    let lp,md,qid = path in
    if lp = [] then lib,md,qid else path

let cat = String.concat "."

(* Ident Absolute Name *)

type package = [ `Local | `Stdlib | `Package of Meta.pkg ]

type id = {
  id : t ;
  id_pkg : package ;
  id_lib : string list ;
  id_mod : string ;
  id_qid : string list ;
}

let resolve ~lib id =
  let lp,id_mod,id_qid = path id in
  if lp = [] then
    { id ; id_pkg = `Local ; id_lib = lib ; id_mod ; id_qid }
  else
    let id_pkg =
      if Filename.is_relative (file id) then `Local else
        try `Package (Meta.find (List.hd lp))
        with _ -> `Stdlib
    in { id ; id_pkg ; id_lib = lp ; id_mod ; id_qid }

(* List Printing *)

let pp_next fmt p =
  Format.pp_print_char fmt '.' ;
  Format.pp_print_string fmt p

let pp_cat fmt = function
  | [] -> ()
  | p::ps -> Format.pp_print_string fmt p ; List.iter (pp_next fmt) ps

let rec pp_last fmt pp = function
  | [] -> ()
  | [a] -> pp fmt a
  | q::qid ->
      Format.pp_print_string fmt q ;
      Format.pp_print_char fmt '.' ;
      pp_last fmt pp qid

(* Title Resolution *)

let to_infix s =
  let n = String.length s in
  if n > 2 && s.[0] = '(' && s.[n-1] = ')' then
    if String.index_opt s '[' <> None
    then "mixfix " ^ String.sub s 1 (n-2) else
    if s.[n-2] = '_'
    then "prefix " ^ String.sub s 1 (n-3)
    else "infix " ^ String.sub s 1 (n-2)
  else s

let unwrap ~prefix s =
  let n = String.length s in
  let p = String.length prefix in
  Printf.sprintf "(%s)" @@ String.sub s p (n-p)

let rec unwrap_any s = function
  | [] -> s
  | prefix::others ->
    if String.starts_with ~prefix s then
      unwrap ~prefix s
    else unwrap_any s others

let of_infix s = unwrap_any s ["prefix ";"infix ";"mixfix "]

let pp_infix fmt a = Format.pp_print_string fmt (of_infix a)

let pp_title fmt r =
  pp_cat fmt r.id_lib ;
  pp_next fmt r.id_mod ;
  pp_last fmt pp_infix r.id_qid

(* URI Encoding *)

let charset_uri =
  let m = Array.make 256 false in
  let r = " !#$%&'()*+,/:;=?@[]" in
  String.iter (fun c -> m.(Char.code c) <- true) r ; m

let charset_why3 =
  let m = Array.make 256 false in
  let s = "-._~!$'()*+,;=:@/?" in (* authorised *)
  String.iter (fun c -> m.(Char.code c) <- true) s;
  let span m a b = for i = Char.code a to Char.code b do m.(i) <- true done in
  span m 'A' 'Z'; span m 'a' 'z'; span m '0' '9'; m

let encode_char m fmt c =
  let k = int_of_char c in
  if m.(k) then
    Format.fprintf fmt "%%%2X" k
  else
    Format.pp_print_char fmt c

let encode fmt a =
  String.iter (encode_char charset_uri fmt) a

let encode_why3 fmt a =
  String.iter (encode_char charset_why3 fmt) a

(* URL Resolution *)

let pp_selector fmt qid =
  Format.pp_print_char fmt '#' ;
  pp_last fmt encode qid

let pp_aname fmt r = pp_selector fmt r.id_qid

let pp_htmlfile fmt r =
  pp_cat fmt r.id_lib ;
  pp_next fmt r.id_mod ;
  Format.pp_print_string fmt ".html"

let pp_ahref ~scope fmt r =
  match r.id_pkg with
  | `Local ->
    let local = match scope with
      | Some m when m = r.id_mod -> true
      | _ -> false
    in
    if not local then pp_htmlfile fmt r ;
    pp_selector fmt r.id_qid
  | `Package m ->
    Format.fprintf fmt "file://%s/html/" m.path ;
    pp_htmlfile fmt r ;
    pp_selector fmt r.id_qid
  | `Stdlib ->
    Format.pp_print_string fmt "https://why3.lri.fr/stdlib/" ;
    pp_htmlfile fmt r ;
    let id = r.id in
    let name = id.id_string in
    if r.id_qid = [] then
      Format.fprintf fmt "#%a_" encode_why3 name
    else
      Format.fprintf fmt "#%a_%d" encode_why3 name (line id)

let pp_proof_aname fmt r =
  pp_selector fmt (r.id_mod :: r.id_qid)

let pp_proof_ahref fmt r =
  Format.pp_print_char fmt '_' ;
  pp_cat fmt r.id_lib ;
  Format.pp_print_string fmt ".html" ;
  pp_proof_aname fmt r

(* -------------------------------------------------------------------------- *)
