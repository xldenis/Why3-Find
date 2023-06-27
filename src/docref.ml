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

type section = {
  cloned_path : string ;
  cloned_order : int ;
}

type clone = {
  id_section : section ;
  id_source : ident ;
  id_target : ident ;
}

type instance =
  | Mi of Pmod.mod_inst
  | Ti of Thy.theory * Thy.symbol_map

type theory = {
  theory: Thy.theory ;
  depends: Thy.theory list ;
  signature: Axioms.signature ;
  instances: instance list ;
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
  let ld,md,qd = Id.path id in
  let k = incr order ; !order in
  let p =
    if ld = [] && qd = [] then
      cat [path;md]
    else
      cat (ld @ md :: qd)
  in { cloned_path = p ; cloned_order = k }

let iter_module ~order ~path ~depend ~instance ~cloned
    (m : Why3.Pmodule.pmodule) =
  let open Why3.Pmodule in
  let rec walk = function
    | Uscope(_,mus) -> List.iter walk mus
    | Uclone mi ->
      instance (Mi mi) ;
      let thy = mi.mi_mod.mod_theory in
      depend thy ;
      let s = section ~order ~path thy in
      iter_mi (fun a b ->
          if Sid.mem b m.mod_local && not @@ Id.lemma b then cloned s a b
        ) mi
    | Uuse m -> depend m.mod_theory
    | _ -> ()
  in List.iter walk m.mod_units

let iter_theory ~order ~path ~depend ~instance ~cloned thy =
  try
    let m = Why3.Pmodule.restore_module thy in
    iter_module ~order ~path ~depend ~instance ~cloned m
  with Not_found ->
    List.iter
      (fun d ->
         match d.Thy.td_node with
         | Clone(th,sm) ->
           instance (Ti(th,sm)) ;
           let s = section ~order ~path th in
           iter_sm (fun a b -> if Sid.mem b thy.th_local then cloned s a b) sm
         | _ -> ()
      ) thy.th_decls

(* -------------------------------------------------------------------------- *)
(* --- Environment                                                        --- *)
(* -------------------------------------------------------------------------- *)

let init () =
  begin
    (* Parser config *)
    Why3.Debug.set_flag Why3.Glob.flag ;
    List.iter (fun k -> Hashtbl.add keywords k ()) Why3.Keywords.keywords ;
    (* Package config *)
    let wenv = Wenv.init () in
    (* Axioms config *)
    let henv = Axioms.init wenv in
    wenv.wenv, henv
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
  in
  if Filename.is_relative file then
    scan [] (Filename.chop_extension file)
  else Filename.[chop_extension @@ basename file]

let parse ~wenv ~henv file =
  if not @@ String.ends_with ~suffix:".mlw" file then
    begin
      Format.eprintf "Error: invalid file name: %S@." file ;
      exit 2
    end ;
  let dir = Filename.chop_extension file in
  let lib = library_path file in
  let path = String.concat "." lib in
  let thys =
    try fst @@ Why3.Env.read_file Why3.Env.base_language wenv file
    with exn ->
      Format.eprintf "Error: %s@." (Printexc.to_string exn) ;
      exit 1
  in
  let order = ref 0 in
  let profile, proofs = load_proofs (Filename.concat dir "proof.json") in
  let theories =
    Mstr.map
      (fun (theory : Thy.theory) ->
         let depends = ref [] in
         let depend th = depends := th :: !depends in
         let instances = ref [] in
         let instance c = instances := c :: !instances in
         let clones = ref [] in
         let cloned s a b =
           clones := {
             id_section = s ;
             id_source = a ;
             id_target = b ;
           } :: !clones in
         iter_theory ~order ~path ~instance ~depend ~cloned theory ;
         let proofs = zip_goals theory proofs in
         let signature = Axioms.signature henv theory in
         {
           theory ; signature ;
           depends = List.rev !depends ;
           instances = List.rev !instances ;
           clones = List.rev !clones ;
           proofs }
      ) thys
  in
  { lib ; urlbase = path ; profile ; theories }

let derived src id =
  Printf.sprintf "%s.%s.html" (String.concat "." src.lib) id

let create () = {
  lib = [] ; urlbase = "" ;
  profile = Calibration.create () ;
  theories = Mstr.empty ;
}

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
