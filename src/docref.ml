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
(* --- Keywords                                                           --- *)
(* -------------------------------------------------------------------------- *)

let keywords = Hashtbl.create 97
let is_keyword = Hashtbl.mem keywords

(* -------------------------------------------------------------------------- *)
(* --- Global References                                                  --- *)
(* -------------------------------------------------------------------------- *)

type position = Lexing.position * Lexing.position

let extract ~infix position =
  let loc = Why3.Loc.extract position in
  if infix then
    let (f,l,s,e) = Why3.Loc.get loc in
    Why3.Loc.user_position f l (succ s) (pred e)
  else loc

let id_loc id =
  match id.Why3.Ident.id_loc with
  | None -> raise Not_found
  | Some loc -> loc

let anchor ~kind id =
  let loc = id_loc id in
  let tag = id.Why3.Ident.id_string in
  let _,line,_,_ = Why3.Loc.get loc in
  if kind = "theory"
  then Printf.sprintf "%s_" tag
  else Printf.sprintf "%s_%d" tag line

let restore_path id =
  try
    Why3.Pmodule.restore_path id
  with Not_found ->
    Why3.Theory.restore_path id

let pathurl ~pkg lp md =
  if lp = [] then "" else
    let lpkg = List.hd lp in
    let path = String.concat "." lp in
    if pkg = lpkg then
      Printf.sprintf "%s.%s.html" path md
    else
      try
        let meta = Meta.find pkg in
        Printf.sprintf "file://%s/html/%s.%s.html" meta.Meta.path path md
      with _ ->
        Printf.sprintf "https://why3.lri.fr/stdlib/%s.html" path

let baseurl ~pkg id =
  let lp,md,_ = restore_path id in
  pathurl ~pkg lp md

type href =
  | NoRef
  | Def of string
  | Ref of string * string

let resolve ~pkg ~infix pos =
  try
    let loc = extract ~infix pos in
    match Why3.Glob.find loc with
    | (id, Why3.Glob.Def, kind) -> Def(anchor ~kind id)
    | (id, Why3.Glob.Use, kind) ->
      let loc = id_loc id in
      let id =
        match Why3.Glob.find loc with
        | exception Not_found -> id
        | ({ Why3.Ident.id_loc = Some _ } as id0, Why3.Glob.Def, _) -> id0
        | _ -> id in
      Ref(baseurl ~pkg id, anchor ~kind id)
  with Not_found -> NoRef

(* -------------------------------------------------------------------------- *)
(* --- Environment                                                        --- *)
(* -------------------------------------------------------------------------- *)

let init ~pkgs =
  let open Why3 in
  begin
    (* Parser config *)
    Debug.set_flag Why3.Glob.flag ;
    List.iter (fun k -> Hashtbl.add keywords k ()) Keywords.keywords ;
    (* Package config *)
    let pkgs = Meta.find_all pkgs in
    let pkg_path = List.map (fun m -> m.Meta.path) pkgs in
    (* Environment config *)
    let config = Whyconf.init_config None in
    let main = Whyconf.get_main config in
    let cfg_path = Whyconf.loadpath main in
    Why3.Env.create_env ("." :: pkg_path @ cfg_path)
  end

(* -------------------------------------------------------------------------- *)
(* --- Parsing                                                            --- *)
(* -------------------------------------------------------------------------- *)

let library_path file =
  let rec scan r p =
    let d = Filename.dirname p in
    if d = "." || d = "" || d = p
    then p::r
    else scan (Filename.basename p :: r) d
  in scan [] (Filename.chop_extension file)

type source = {
  pkg: string;
  name: string;
  url: string;
}

let parse ~why3env file =
  let lib = library_path file in
  let pkg = match lib with [] | [_] -> "" | pkg::_ -> pkg in
  let name = String.concat "." lib in
  let _theories =
    try fst @@ Why3.Env.read_file Why3.Env.base_language why3env file
    with exn ->
      Format.eprintf "%s@." (Printexc.to_string exn) ;
      exit 1
  in
  let url = Printf.sprintf "%s.html" name in
  { pkg ; name ; url }

let derived src id =
  Printf.sprintf "%s.%s.html" src.name id

(* -------------------------------------------------------------------------- *)
(* --- Global References                                                  --- *)
(* -------------------------------------------------------------------------- *)

let is_uppercased s =
  String.length s > 0 &&
  let c = s.[0] in 'A' <= c && c <= 'Z'

let infix s =
  let n = String.length s in
  if n > 2 && s.[0] = '(' && s.[n-1] = ')' then
    if s.[n-2] = '_'
    then "prefix " ^ String.sub s 1 (n-3)
    else "infix " ^ String.sub s 1 (n-2)
  else s

let href ~pkg id =
  Printf.sprintf "%s#%s" (baseurl ~pkg id) (anchor ~kind:"" id)

let reference ~why3env ~pkg r =
  let kind,name =
    let n = String.length r in
    if n >= 2 && r.[1] = ':'
    then r.[0],String.sub r 2 (n-2)
    else '?',r in
  let lp,m,qid =
    let rec split rp = function
      | [] -> [], "", List.rev_map infix rp
      | p::ps ->
        if is_uppercased p then List.rev rp,p,List.map infix ps
        else split (p::rp) ps
    in split [] (String.split_on_char '.' name) in
  let url =
    if kind = '?' && qid = [] then
      pathurl ~pkg lp m
    else
      let open Why3 in
      let thy = Env.read_theory why3env lp m in
      let ns = thy.th_export in
      match kind with
      | 't' -> href ~pkg (Theory.ns_find_ts ns qid).ts_name
      | 'v' -> href ~pkg (Theory.ns_find_ls ns qid).ls_name
      | 'p' -> href ~pkg (Theory.ns_find_pr ns qid).pr_name
      | '?' ->
        begin
          match
            List.concat [
              (try [(Theory.ns_find_ts ns qid).ts_name] with Not_found -> []);
              (try [(Theory.ns_find_ls ns qid).ls_name] with Not_found -> []);
              (try [(Theory.ns_find_pr ns qid).pr_name] with Not_found -> []);
            ]
          with
          | [id] -> href ~pkg id
          | [] -> raise Not_found
          | _ -> failwith "ambiguous reference"
      end
      | _ -> failwith "invalid reference kind (use 't', 'v' or 'p')"
  in url, name

(* -------------------------------------------------------------------------- *)
