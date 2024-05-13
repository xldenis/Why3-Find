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

module Sid = Why3.Ident.Sid
module Hid = Why3.Ident.Hid
module Thy = Why3.Theory
module Pmod = Why3.Pmodule
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

(* -------------------------------------------------------------------------- *)
(* --- Global References                                                  --- *)
(* -------------------------------------------------------------------------- *)

type ident = Id.t
type position = Lexing.position * Lexing.position

type instance = {
  inst_path : string ; (* container of the clone instance *)
  inst_order : int ; (* clone declaration offset *)
  inst_cloned : Thy.theory ; (* cloned theory *)
}

type clone = {
  id_instance : instance ;
  id_source : ident ; (* from cloned theory *)
  id_target : ident ; (* to clone instance *)
}

type theory = {
  path: string;
  theory: Thy.theory ;
  depends: Thy.theory list ;
  signature: Axioms.signature ;
  clones: clone list ;
  proofs: Crc.crc Mstr.t ;
}

type source = {
  lib: string list;
  urlbase: string;
  profile: Calibration.profile ;
  theories: theory Mstr.t;
}

let extract ~infix position =
  let loc = Why3.Loc.extract position in
  if infix then
    let (f,p,s,q,e) = Why3.Loc.get loc in
    Why3.Loc.user_position f p (succ s) q (pred e)
  else loc

type href =
  | NoRef
  | Ref of Id.id
  | Def of Id.id * Crc.crc option

let find_proof (id : ident) = function
  | None -> None
  | Some { proofs } -> Mstr.find_opt (Session.proof_name id) proofs

let strip_lemma (id : ident) = function
  | None -> id
  | Some { theory } ->
    if not @@ Id.lemma id then id else
      begin
        let exception Found of Id.t in
        try
          List.iter
            (fun (td : Thy.tdecl) ->
               match td.td_node with
               | Use _ | Clone _ | Meta _ -> ()
               | Decl d ->
                 match d.d_node with
                 | Dprop(_, { pr_name = ax },_) ->
                   if ax.id_string ^ "'lemma" = id.id_string then
                     raise (Found ax)
                 | _ -> ()
            ) theory.th_decls ;
          id
        with Found id -> id
      end

let resolve ~src ~theory ~infix pos =
  try
    let loc = extract ~infix pos in
    match Why3.Glob.find loc with
    | (id, Why3.Glob.Def, _) ->
      let id = strip_lemma id theory in
      let proof = find_proof id theory in
      Def (Id.resolve ~lib:src.lib id, proof)
    | (id, Why3.Glob.Use, _) ->
      let id = strip_lemma id theory in
      Ref (Id.resolve ~lib:src.lib id)
  with Not_found -> NoRef

(* -------------------------------------------------------------------------- *)
(* --- Environment                                                        --- *)
(* -------------------------------------------------------------------------- *)

type cenv = {
  cloning : int Hid.t ; (* number of clones per theory/module *)
  mutable path : string ; (* container name *)
  mutable order : int ; (* clone section order during parsing *)
}

let init () =
  begin
    (* Parser config *)
    Why3.Debug.set_flag Why3.Glob.flag ;
    List.iter (fun k -> Hashtbl.add keywords k ()) Why3.Keywords.keywords ;
    (* Cloning env *)
    { cloning = Hid.create 0 ; path = "" ; order = 0 }
  end

let set_container cenv ~path ~id =
  cenv.order <- 0 ; cenv.path <- Printf.sprintf "%s.%s" path id

let set_instance cenv id =
  let order = succ cenv.order + Hid.find_def cenv.cloning 0 id in
  cenv.order <- order ; order

let current_instance cenv s =
  s.inst_path = cenv.path && s.inst_order = cenv.order

let is_ident a = function
  | None -> true
  | Some b -> Why3.Ident.id_equal a b

let is_clone cenv ?source ?target c =
  is_ident c.id_source source &&
  is_ident c.id_target target &&
  current_instance cenv c.id_instance

let find_clone cenv ?source ?target thy =
  List.find_opt (is_clone cenv ?source ?target) thy.clones

(* -------------------------------------------------------------------------- *)
(* --- Theory Iterators                                                   --- *)
(* -------------------------------------------------------------------------- *)

let instance cenv th =
  cenv.order <- succ cenv.order ;
  {
    inst_path = cenv.path ;
    inst_order = cenv.order ;
    inst_cloned = th ;
  }

let iter_module cenv ~depend ~cloned
    (m : Why3.Pmodule.pmodule) =
  let open Why3.Pmodule in
  let rec walk = function
    | Uscope(_,mus) -> List.iter walk mus
    | Uclone mi ->
      let thy = mi.mi_mod.mod_theory in
      depend thy ;
      let s = instance cenv thy in
      Wutil.iter_mi (fun a b ->
          if Sid.mem b m.mod_local && not @@ Id.lemma b then cloned s a b
        ) mi
    | Uuse m -> depend m.mod_theory
    | _ -> ()
  in List.iter walk m.mod_units

let iter_theory cenv ~depend ~cloned thy =
  try
    let m = Why3.Pmodule.restore_module thy in
    iter_module cenv ~depend ~cloned m
  with Not_found ->
    List.iter
      (fun d ->
         match d.Thy.td_node with
         | Clone(th,sm) ->
           let s = instance cenv th in
           Wutil.iter_sm
             (fun a b -> if Sid.mem b thy.th_local then cloned s a b) sm
         | _ -> ()
      ) thy.th_decls

(* -------------------------------------------------------------------------- *)
(* --- Proofs                                                             --- *)
(* -------------------------------------------------------------------------- *)

let zip_goals theory proofs =
  let proofs = Mstr.find_def Mstr.empty (Session.thy_name theory) proofs in
  List.fold_left
    (fun cmap task ->
       let a = Session.task_name task in
       Mstr.add a (Mstr.find_def (Crc.stuck ()) a proofs) cmap
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
  in
  if Filename.is_relative file then
    scan [] (Filename.chop_extension file)
  else Filename.[chop_extension @@ basename file]

let derived src id =
  Printf.sprintf "%s.%s.html" (String.concat "." src.lib) id

let empty () = {
  lib = [] ; urlbase = "" ;
  profile = Calibration.create () ;
  theories = Mstr.empty ;
}

let parse ~wenv ~cenv ~henv file =
  if not @@ String.ends_with ~suffix:".mlw" file then
    begin
      Log.error "invalid file name: %S" file ;
      exit 2
    end ;
  let lib = library_path file in
  let path = String.concat "." lib in
  let thys =
    try fst @@ Why3.Env.read_file Why3.Env.base_language wenv file
    with exn ->
      Log.error "%s" (Printexc.to_string exn) ;
      exit 1
  in
  let profile, proofs = Proofs.load_proofs file in
  let theories =
    Mstr.mapi
      (fun id (theory : Thy.theory) ->
         set_container cenv ~path ~id ;
         let depends = ref [] in
         let depend th = depends := th :: !depends in
         let clones = ref [] in
         let cloned s a b =
           clones := {
             id_instance = s ;
             id_source = a ;
             id_target = b ;
           } :: !clones in
         iter_theory cenv ~depend ~cloned theory ;
         Hid.add cenv.cloning theory.th_name cenv.order ;
         let proofs = zip_goals theory proofs in
         let signature = Axioms.signature henv theory in
         let path = Id.fullname ~lib theory.th_name in
         {
           path ; theory ; signature ; proofs ;
           depends = List.rev !depends ;
           clones = List.rev !clones ;
         }
      ) thys
  in
  { lib ; urlbase = path ; profile ; theories }

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
  | _ ->
    Utils.failwith
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
  | _ -> Utils.failwith
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
  let ids = List.sort_uniq Id.compare ids in
  match ids with
  | [id] -> id
  | [] -> Utils.failwith "reference '%s' not found" name
  | _ -> Utils.failwith "ambiguous reference '%s'" name

(* Global reference resolution *)

let reference ~wenv ~src ~scope r =
  let kind,name =
    let n = String.length r in
    if n >= 2 && r.[1] = ':'
    then r.[0],String.sub r 2 (n-2)
    else '?',r in
  name,
  let lp,m,qid =
    let rec split rp = function
      | [] -> [], "", List.rev_map Id.to_infix rp
      | p::ps ->
        if is_uppercased p then List.rev rp,p,List.map Id.to_infix ps
        else split (p::rp) ps
    in split [] (String.split_on_char '.' name) in
  if lp = [] then
    select ~name @@ lookup ~scope ~theories:src.theories kind m qid
  else
    let thy =
      try Why3.Env.read_theory wenv lp m
      with Not_found ->
        Utils.failwith "unknown theory or module '%s.%s'"
          (String.concat "." lp) m
    in
    if kind = '?' && qid = [] then
      thy.th_name
    else
      select ~name @@ find kind qid thy

(* -------------------------------------------------------------------------- *)
