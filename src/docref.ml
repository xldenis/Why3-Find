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
(* --- Refs for Documentation                                             --- *)
(* -------------------------------------------------------------------------- *)

module Id = Why3.Ident
module Sid = Why3.Ident.Sid
module Thy = Why3.Theory
module Mstr = Why3.Wstdlib.Mstr

(* -------------------------------------------------------------------------- *)
(* --- Keywords                                                           --- *)
(* -------------------------------------------------------------------------- *)

let keywords = Hashtbl.create 97
let is_keyword = Hashtbl.mem keywords

(* Reference Parsing *)

let is_uppercased s =
  String.length s > 0 &&
  let c = s.[0] in 'A' <= c && c <= 'Z'

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

(* -------------------------------------------------------------------------- *)
(* --- Global References                                                  --- *)
(* -------------------------------------------------------------------------- *)

type ident = Id.ident
type position = Lexing.position * Lexing.position

type section = {
  cloned_path : string ;
  cloned_order : int ;
}

type clone = {
  id_section : section ;
  id_source : ident ;
  id_target : ident ;
}

type theory = {
  theory: Thy.theory;
  clones: clone list ;
  proofs: Crc.crc Mstr.t ;
}

type source = {
  url: string;
  lib: string list;
  profile: Calibration.profile ;
  theories: theory Mstr.t;
}

let extract ~infix position =
  let loc = Why3.Loc.extract position in
  if infix then
    let (f,l,s,e) = Why3.Loc.get loc in
    Why3.Loc.user_position f l (succ s) (pred e)
  else loc

let id_loc id =
  match id.Id.id_loc with
  | None -> raise Not_found
  | Some loc -> loc

let id_line id =
  let _,line,_,_ = Why3.Loc.get (id_loc id) in line

let id_file id =
  let file,_,_,_ = Why3.Loc.get (id_loc id) in file

let restore_path id =
  try
    Why3.Pmodule.restore_path id
  with Not_found ->
    Thy.restore_path id

let id_path ~src ~scope id =
  let lp,md,qid = restore_path id in
  let lp =
    match lp, scope with
    | [], Some m when m <> md -> src.lib
    | _ -> lp
  in String.concat "." (lp @ md :: List.map of_infix qid)

let baseurl ~src ~scope id =
  let lp,md,_ = restore_path id in
  if lp = [] then
    match scope with
    | Some m when m = md -> ""
    | _ -> Printf.sprintf "%s.%s.html" (String.concat "." src.lib) md
  else
    let path = String.concat "." lp in
    if Filename.is_relative (id_file id) then
      Printf.sprintf "%s.%s.html" path md
    else
      try
        let pkg = List.hd lp in
        let meta = Meta.find pkg in
        Printf.sprintf "file://%s/html/%s.%s.html" meta.Meta.path path md
      with _ ->
        Printf.sprintf "https://why3.lri.fr/stdlib/%s.html" path

let anchor ~kind id =
  let name = id.Id.id_string in
  let line = id_line id in
  if kind = "theory"
  then Printf.sprintf "%s_" name
  else Printf.sprintf "%s_%d" name line

type href =
  | NoRef
  | Def of { id: Id.ident ; anchor: string ; proof: Crc.crc option }
  | Ref of { kind: string ; path: string ; href: string }

let pp_ident fmt (id : Id.ident) =
  Format.fprintf fmt "%s<%d>" id.id_string (Why3.Weakhtbl.tag_hash id.id_tag)

let find_proof id = function
  | None -> None
  | Some { proofs } -> Mstr.find_opt id.Id.id_string proofs

let resolve ~src ~scope ~theory ~infix pos =
  try
    let loc = extract ~infix pos in
    match Why3.Glob.find loc with
    | (id, Why3.Glob.Def, kind) ->
      let anchor = anchor ~kind id in
      let proof = find_proof id theory in
      Def { anchor ; id ; proof }
    | (id, Why3.Glob.Use, kind) ->
      let path = id_path ~src ~scope id in
      let base = baseurl ~src ~scope id in
      let name = anchor ~kind id in
      Ref { kind ; path ; href = Printf.sprintf "%s#%s" base name }
  with Not_found -> NoRef

let id_name id = id.Id.id_string
let id_pretty id = of_infix id.Id.id_string
let id_anchor id = anchor ~kind:"" id
let id_href ~src ~scope id =
  Printf.sprintf "%s#%s" (baseurl ~src ~scope id) (anchor ~kind:"" id)

(* -------------------------------------------------------------------------- *)
(* --- Theory Iterators                                                   --- *)
(* -------------------------------------------------------------------------- *)

let iter_mi f (mi : Why3.Pmodule.mod_inst) =
  begin
    let open Why3.Ty in
    let open Why3.Ity in
    let open Why3.Term in
    let open Why3.Decl in
    let open Why3.Expr in
    Mts.iter
      (fun a ity ->
         match ity.ity_node with
         | Ityreg b -> f a.ts_name b.reg_name
         | Ityapp(b,_,_) -> f a.ts_name b.its_ts.ts_name
         | Ityvar b -> f a.ts_name b.tv_name
      ) mi.mi_ty ;
    Mts.iter (fun a b  -> f a.ts_name b.its_ts.ts_name) mi.mi_ts ;
    Mls.iter (fun a b -> f a.ls_name b.ls_name) mi.mi_ls ;
    Mpr.iter (fun a b -> f a.pr_name b.pr_name) mi.mi_pr ;
    Mvs.iter (fun a b -> f a.vs_name b.pv_vs.vs_name) mi.mi_pv ;
    Mrs.iter (fun a b -> f a.rs_name b.rs_name) mi.mi_rs ;
    Mxs.iter (fun a b -> f a.xs_name b.xs_name) mi.mi_xs ;
  end

let iter_sm f (sm : Thy.symbol_map) =
  begin
    let open Why3.Ty in
    let open Why3.Term in
    let open Why3.Decl in
    Mts.iter
      (fun a ty ->
         match ty.ty_node with
         | Tyvar _ -> ()
         | Tyapp(b,_) -> f a.ts_name b.ts_name
      ) sm.sm_ty ;
    Mts.iter (fun a b -> f a.ts_name b.ts_name) sm.sm_ts ;
    Mls.iter (fun a b -> f a.ls_name b.ls_name) sm.sm_ls ;
    Mpr.iter (fun a b -> f a.pr_name b.pr_name) sm.sm_pr ;
  end

let section ~order ~path th =
  let id = th.Thy.th_name in
  let cat = String.concat "." in
  let ld,md,qd = restore_path id in
  let k = incr order ; !order in
  let p =
    if ld = [] && qd = [] then
      cat [path;md]
    else
      cat (ld @ md :: qd)
  in { cloned_path = p ; cloned_order = k }

let iter_cloned_module ~order ~path f (m : Why3.Pmodule.pmodule) =
  let open Why3.Pmodule in
  let rec walk = function
    | Uscope(_,mus) -> List.iter walk mus
    | Uclone mi ->
      let s = section ~order ~path mi.mi_mod.mod_theory in
      iter_mi (fun a b -> if Sid.mem b m.mod_local then f s a b) mi
    | _ -> ()
  in List.iter walk m.mod_units

let iter_cloned_theory ~order ~path f thy =
  try
    let m = Why3.Pmodule.restore_module thy in
    iter_cloned_module ~order ~path f m
  with Not_found ->
    List.iter
      (fun d ->
         match d.Thy.td_node with
         | Clone(th,sm) ->
           let s = section ~order ~path th in
           iter_sm (fun a b -> if Sid.mem b thy.th_local then f s a b) sm
         | _ -> ()
      ) thy.th_decls

(* -------------------------------------------------------------------------- *)
(* --- Environment                                                        --- *)
(* -------------------------------------------------------------------------- *)

let init ~pkgs =
  begin
    (* Parser config *)
    Why3.Debug.set_flag Why3.Glob.flag ;
    List.iter (fun k -> Hashtbl.add keywords k ()) Why3.Keywords.keywords ;
    (* Package config *)
    Wenv.init ~pkgs
  end

(* -------------------------------------------------------------------------- *)
(* --- Proofs                                                             --- *)
(* -------------------------------------------------------------------------- *)

let jmap cc js =
  let m = ref Mstr.empty in
  Json.jiter (fun fd js -> m := Mstr.add fd (cc js) !m) js ; !m

let load_proofs file =
  let js = if Sys.file_exists file then Json.of_file file else `Null in
  let profile = Calibration.of_json @@ Json.jfield "profile" js in
  let strategy = jmap (jmap Crc.of_json) @@ Json.jfield "proofs" js in
  profile , strategy

let zip_goals theory proofs =
  let proofs = Mstr.find_def Mstr.empty (Session.thy_name theory) proofs in
  List.fold_left
    (fun cmap task ->
       let a = Session.task_name task in
       Mstr.add a (Mstr.find_def Crc.Stuck a proofs) cmap
    )
    Mstr.empty
    (Why3.Task.split_theory theory None None)

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

let parse ~why3env file =
  if not @@ String.ends_with ~suffix:".mlw" file then
    begin
      Format.eprintf "Invalid file name: %S@." file ;
      exit 2
    end ;
  let dir = Filename.chop_extension file in
  let lib = library_path file in
  let path = String.concat "." lib in
  let thys =
    try fst @@ Why3.Env.read_file Why3.Env.base_language why3env file
    with exn ->
      Format.eprintf "%s@." (Printexc.to_string exn) ;
      exit 1
  in
  let order = ref 0 in
  let profile, proofs = load_proofs (Filename.concat dir "proof.json") in
  let theories =
    Mstr.map
      (fun (theory : Thy.theory) ->
         let clones = ref [] in
         iter_cloned_theory ~order ~path
           (fun s a b ->
              clones := {
                id_section = s ;
                id_source = a ;
                id_target = b ;
              } :: !clones
           ) theory ;
         let proofs = zip_goals theory proofs in
         { theory ; clones = !clones ; proofs }
      ) thys
  in
  let url = Printf.sprintf "%s.html" path in
  { lib ; url ; profile ; theories }

let derived src id =
  Printf.sprintf "%s.%s.html" (String.concat "." src.lib) id

(* -------------------------------------------------------------------------- *)
(* --- Global References                                                  --- *)
(* -------------------------------------------------------------------------- *)

(* Theory lookup *)

let ns_find_ts ns qid =
  try [(Thy.ns_find_ts ns qid).ts_name] with Not_found -> []

let ns_find_ls ns qid =
  try [(Thy.ns_find_ls ns qid).ls_name] with Not_found -> []

let ns_find_pr ns qid =
  try [(Thy.ns_find_pr ns qid).pr_name] with Not_found -> []

let ns_find ns kind qid =
  match kind with
  | 't' -> ns_find_ts ns qid
  | 'l' -> ns_find_ls ns qid
  | 'p' -> ns_find_pr ns qid
  | 'v' -> []
  | 'e' -> []
  | '?' ->
    List.concat [
      ns_find_ts ns qid ;
      ns_find_ls ns qid ;
      ns_find_pr ns qid ;
    ]
  | _ -> failwith @@ Printf.sprintf
      "invalid reference kind '%c' (use 't', 'l', 'v', 'e' or 'p')" kind

(* Module lookup *)

let pns_find_ts ns tns qid =
  try [(Why3.Pmodule.ns_find_its ns qid).its_ts.ts_name]
  with Not_found -> ns_find_ts tns qid

let pns_find_rs ns tns qid =
  try [(Why3.Pmodule.ns_find_rs ns qid).rs_name]
  with Not_found -> ns_find_ls tns qid

let pns_find_xs ns qid =
  try [(Why3.Pmodule.ns_find_xs ns qid).xs_name] with Not_found -> []

let pns_find pm kind qid =
  let ns = pm.Why3.Pmodule.mod_export in
  let tns = pm.mod_theory.th_export in
  match kind with
  | 't' -> pns_find_ts ns tns qid
  | 'v' -> pns_find_rs ns tns qid
  | 'e' -> pns_find_xs ns qid
  | 'l' -> ns_find_ls tns qid
  | 'p' -> ns_find_pr tns qid
  | '?' ->
    List.concat [
      pns_find_ts ns tns qid ;
      pns_find_rs ns tns qid ;
      pns_find_xs ns qid ;
      ns_find_ls tns qid ;
      ns_find_pr tns qid ;
    ]
  | _ -> failwith @@ Printf.sprintf
      "invalid reference kind '%c' (use 't', 'l', 'v', 'e' or 'p')" kind

(* Reference Lookup *)

let find kind qid thy =
  try pns_find (Why3.Pmodule.restore_module thy) kind qid
  with Not_found -> ns_find thy.Thy.th_export kind qid

let find_theory kind qid { theory } = find kind qid theory

let lookup ~scope ~theories kind m qid =
  match if m <> "" then Some m else scope with
  | Some m0 ->
    (try find_theory kind qid (Mstr.find m0 theories) with Not_found -> [])
  | None ->
    List.concat @@ List.map (find_theory kind qid) (Mstr.values theories)

let select ~name ids =
  let ids = List.sort_uniq Id.id_compare ids in
  match ids with
  | [id] -> id
  | [] -> failwith (Printf.sprintf "reference '%s' not found" name)
  | _ -> failwith (Printf.sprintf "ambiguous reference '%s'" name)

(* Global reference resolution *)

let reference ~why3env ~src ~scope r =
  let kind,name =
    let n = String.length r in
    if n >= 2 && r.[1] = ':'
    then r.[0],String.sub r 2 (n-2)
    else '?',r in
  name,
  let lp,m,qid =
    let rec split rp = function
      | [] -> [], "", List.rev_map to_infix rp
      | p::ps ->
        if is_uppercased p then List.rev rp,p,List.map to_infix ps
        else split (p::rp) ps
    in split [] (String.split_on_char '.' name) in
  if lp = [] then
    select ~name @@ lookup ~scope ~theories:src.theories kind m qid
  else
    let thy =
      try Why3.Env.read_theory why3env lp m
      with Not_found ->
        failwith @@ Printf.sprintf "unknown theory or module '%s.%s'"
          (String.concat "." lp) m
    in
    if kind = '?' && qid = [] then
      thy.th_name
    else
      select ~name @@ find kind qid thy

(* -------------------------------------------------------------------------- *)
