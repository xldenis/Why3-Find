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
(* --- Compute Axioms                                                     --- *)
(* -------------------------------------------------------------------------- *)

open Why3
module Sid = Ident.Sid
module Mid = Ident.Mid
module Hid = Ident.Hid

(* -------------------------------------------------------------------------- *)
(* --- Theory & Module Assumed Symbols                                    --- *)
(* -------------------------------------------------------------------------- *)

type kind =
  | Type (* abstract type *)
  | Logic (* constant, function of predicate *)
  | Axiom (* hypothesis *)
  | Param (* non-constrained val *)
  | Value (* constrained val *)
  | Unsafe

type parameter = {
  kind : kind ;
  name : Ident.ident ;
  builtin : (Prover.prover * string) list ;
  extern : string option ;
}

let is_free_kind = function
  | Type | Logic | Param -> true
  | Unsafe | Axiom | Value -> false

let is_unsafe_kind = function
  | Unsafe -> true
  | Type | Logic | Param | Value | Axiom -> false

let is_free { kind } = is_free_kind kind
let is_unsafe { kind } = is_unsafe_kind kind
let is_external { builtin ; extern } = builtin <> [] || extern <> None

type signature = {
  locals : parameter Mid.t ;
  used_theories : Theory.theory list ;
  cloned_theories : Theory.theory list ;
}

type henv = {
  builtins : (Prover.prover * string) list Mid.t ;
  externals : string Mid.t ;
  signatures : signature Hid.t ;
}

(* -------------------------------------------------------------------------- *)
(* --- Builtin & Extracted Environments                                   --- *)
(* -------------------------------------------------------------------------- *)

let ocaml64 =
  Filename.concat Config.datadir @@
  Filename.concat "extraction_drivers" @@
  "ocaml64.drv"

let drivers (pkg : Meta.pkg) =
  List.map (Filename.concat pkg.path) pkg.drivers

let add_builtins bmap p =
  Mid.merge
    (fun _ a b ->
       match a,b with
       | None, None -> None
       | Some _, None -> a
       | None, Some (m,_) -> Some [p,m]
       | Some xs, Some (m,_) -> Some ((p,m) :: xs)
    ) bmap @@ Driver.syntax_map p.Prover.driver

let init (wenv : Wenv.env) =
  let provers = Prover.select wenv ~patterns:(Wenv.provers ()) in
  let builtins = List.fold_left add_builtins Mid.empty provers in
  let drivers = List.concat_map drivers wenv.pkgs @ Wenv.drivers () in
  let main = Whyconf.get_main wenv.wconfig in
  let pdriver = Pdriver.load_driver main wenv.wenv ocaml64 drivers in
  let externals = Mid.map fst pdriver.drv_syntax in
  { builtins ; externals ; signatures = Hid.create 0 }

(* -------------------------------------------------------------------------- *)
(* --- Elementary Operations                                              --- *)
(* -------------------------------------------------------------------------- *)

let empty = {
  locals = Mid.empty ;
  used_theories = [] ;
  cloned_theories = [] ;
}

let add henv hs name kind =
  let builtin = Mid.find_def [] name henv.builtins in
  let extern = Mid.find_opt name henv.externals in
  let prm = { name ; kind ; builtin ; extern } in
  { hs with locals = Mid.add name prm hs.locals }

let add_used hs (thy : Theory.theory) =
  { hs with used_theories = thy :: hs.used_theories }

let add_cloned hs (thy : Theory.theory) =
  { hs with cloned_theories = thy :: hs.cloned_theories }

(* -------------------------------------------------------------------------- *)
(* --- Theory Declarations                                                --- *)
(* -------------------------------------------------------------------------- *)

let nodef (td : _ Ty.type_def) = match td with
  | NoDef -> true
  | Alias _ | Range _ | Float _ -> false

let add_type henv hs (ty : Ty.tysymbol) =
  if nodef ty.ts_def then add henv hs ty.ts_name Type else hs

let add_decl henv (hs : signature) (d : Decl.decl) : signature =
  match d.d_node with
  | Dlogic _ | Dind _ | Ddata _ -> hs
  | Dtype ty -> add_type henv hs ty
  | Dparam ls -> add henv hs ls.ls_name Logic
  | Dprop(Paxiom,pr,_) -> add henv hs pr.pr_name Axiom
  | Dprop((Plemma|Pgoal),_,_) -> hs

let add_tdecl henv (hs : signature) (d : Theory.tdecl) : signature =
  match d.td_node with
  | Decl d -> add_decl henv hs d
  | Use thy -> add_used hs thy
  | Clone(thy,_) -> add_cloned hs thy
  | Meta _ -> hs

let theory_signature henv (thy : Theory.theory) =
  List.fold_left (add_tdecl henv) empty thy.th_decls

(* -------------------------------------------------------------------------- *)
(* --- Definition Kind                                                    --- *)
(* -------------------------------------------------------------------------- *)

type cany =
  | Free (* no post *)
  | Verified (* post with witness of existence *)
  | Unverified (* post without witness of existence *)

(* Checks that contract of Cany is provable, ie. its pre-condition contains a
   proof witnessing the existence of a result value that satisfy all
   post-conditions. This is the only way to distinguish "val" from "let"
   definitions. *)

let verified ~pre ~post =
  begin
    (* compare pre and post *)
    let instance vr (q : Term.term) p =
      let q = (* return variable alpha-conversion *)
        match q.t_node with
        | Tlet( { t_node = Tvar r },q) when Term.vs_equal r vr -> Term.t_eps q
        | _ -> Term.t_eps_close vr q
      in Term.t_equal p q in
    (* compare pre-condition conjunctions wrt list of post-conditions *)
    let rec postcond vr (q : Term.term) (ps : Term.term list) =
      match q.t_node, ps with
      | _ , [p] -> instance vr q p
      | Tbinop(Tand,q,qs) , p::ps -> instance vr q p && postcond vr qs ps
      | _ , _::_ -> false
      | _ , [] -> true in
    (* witness proof is the last pre-condition *)
    let rec witness = function
      | [] -> Term.t_true
      | [pre] -> pre
      | _::ps -> witness ps in
    let w = witness pre in
    (* unfold the witness existential *)
    match w.t_node with
    | Tquant(Texists,tq) ->
      let vs,_trigger,pre = Term.t_open_quant tq in
      begin
        match vs with
        | [vret] -> postcond vret pre post
        | _ -> false
      end
    | _ -> false
  end

let cany (cty: Ity.cty) : cany =
  if cty.cty_post = [] && Ity.Mxs.is_empty cty.cty_xpost
  then Free else
  if verified ~pre:cty.cty_pre ~post:cty.cty_post
  then Verified else Unverified

(* -------------------------------------------------------------------------- *)
(* --- Sound Expression Check                                             --- *)
(* -------------------------------------------------------------------------- *)

let rec safe_let_defn (def : Expr.let_defn) =
  match def with
  | LDvar(_,e) -> safe_expr e
  | LDsym(_,ce) -> safe_cexpr ce
  | LDrec lts -> List.for_all safe_rec_defn lts

and safe_rec_defn (d : Expr.rec_defn) = safe_cexpr d.rec_fun

and safe_expr (e : Expr.expr) =
  match e.e_node with
  | Evar _ | Econst _ | Eassign _  | Epure _ | Eabsurd -> true
  | Eexec (ce, _) -> safe_cexpr ce
  | Elet(def,e) -> safe_let_defn def && safe_expr e
  | Eif (a, b, c) -> safe_expr a && safe_expr b && safe_expr c
  | Ewhile(a,_,_,b) -> safe_expr a && safe_expr b
  | Efor (_, _, _, _, b) -> safe_expr b
  | Eraise (_, e) | Eexn (_, e) | Eghost e -> safe_expr e
  | Eassert ((Assert | Check), _) -> true
  | Eassert (Assume,_) -> false
  | Ematch (a, ps, xs) ->
    safe_expr a &&
    List.for_all safe_branch ps &&
    Ity.Mxs.for_all safe_handler xs

and safe_branch (_,e) = safe_expr e
and safe_handler _ (_,e) = safe_expr e

and safe_cexpr (ce : Expr.cexp) =
  match ce.c_node with
  | Cfun e -> safe_expr e
  | Capp _ | Cpur _ -> true
  | Cany ->
    match cany ce.c_cty with
    | Free | Verified -> true | Unverified -> false

(* -------------------------------------------------------------------------- *)
(* --- Value Definitions                                                  --- *)
(* -------------------------------------------------------------------------- *)

let rec add_def_expr henv hs (decl : Ident.ident) (e : Expr.expr) =
  (* looking for top-level Cany definition *)
  match e.e_node with
  | Eexec(ce,_) -> add_def_cexp henv hs decl ce
  | _ -> if safe_expr e then hs else add henv hs decl Unsafe

and add_def_cexp henv hs (decl : Ident.ident) (ce : Expr.cexp) =
  match ce.c_node with
  | Cpur _ | Capp _ -> hs
  | Cfun e -> add_def_expr henv hs decl e
  | Cany ->
    match cany ce.c_cty with
    | Free -> add henv hs decl Param
    | Unverified -> add henv hs decl Value
    | Verified -> hs

(* -------------------------------------------------------------------------- *)
(* --- Module Declarations                                                --- *)
(* -------------------------------------------------------------------------- *)

let add_mtype henv (hs : signature) (it : Pdecl.its_defn) : signature =
  if it.itd_fields = [] && it.itd_constructors = [] then
    let its = it.itd_its in
    if nodef its.its_def then add henv hs its.its_ts.ts_name Type else hs
  else hs

let add_rsymbol henv hs (rs : Expr.rsymbol) (ce : Expr.cexp) =
  if Id.lemma rs.rs_name then hs else add_def_cexp henv hs rs.rs_name ce

let add_psymbol henv hs (pv : Ity.pvsymbol) (expr : Expr.expr) =
  add_def_expr henv hs pv.pv_vs.vs_name expr

let add_rec_def henv hs (def : Expr.rec_defn) =
  add_def_cexp henv hs def.rec_sym.rs_name def.rec_fun

let add_pdecl henv (hs : signature) (d : Pdecl.pdecl) : signature =
  match d.pd_node with
  | PDexn _ -> hs
  | PDtype tys -> List.fold_left (add_mtype henv) hs tys
  | PDlet (LDsym(r,c)) -> add_rsymbol henv hs r c
  | PDlet (LDvar(v,e)) -> add_psymbol henv hs v e
  | PDlet (LDrec defs) -> List.fold_left (add_rec_def henv) hs defs
  | PDpure -> List.fold_left (add_decl henv) hs d.pd_pure

let rec add_munit henv (hs : signature) (m : Pmodule.mod_unit) : signature =
  match m with
  | Udecl pd -> add_pdecl henv hs pd
  | Uuse pm -> add_used hs pm.mod_theory
  | Uclone mi ->
    add_cloned hs mi.mi_mod.mod_theory
  | Uscope(_,ms) -> List.fold_left (add_munit henv) hs ms
  | Umeta _ -> hs

let module_signature henv (pm : Pmodule.pmodule) : signature =
  List.fold_left (add_munit henv) empty pm.mod_units

(* -------------------------------------------------------------------------- *)
(* --- Signature                                                          --- *)
(* -------------------------------------------------------------------------- *)

let signature henv (thy : Theory.theory) =
  let id = thy.th_name in
  try Hid.find henv.signatures id
  with Not_found ->
    let hs =
      try module_signature henv @@ Pmodule.restore_module thy
      with Not_found -> theory_signature henv thy
    in Hid.add henv.signatures id hs ; hs

let parameter s id = Mid.find_opt id s.locals
let parameters s = Mid.values s.locals

(* -------------------------------------------------------------------------- *)
(* --- Consolidated Hypotheses                                            --- *)
(* -------------------------------------------------------------------------- *)

let dependencies henv ?(self=false) (ths : Theory.theory list) =
  let deps = ref Mid.empty in
  let rec add (thy : Theory.theory) =
    let id = thy.th_name in
    if not @@ Mid.mem id !deps then
      begin
        deps := Mid.add id thy !deps ;
        add_derived thy
      end
  and add_derived thy =
    add_depends (signature henv thy)
  and add_depends s =
    List.iter add s.used_theories ;
    List.iter add_derived s.cloned_theories ;
  in
  let add_init = if self then add else add_derived in
  List.iter add_init ths ; Mid.values !deps

let iter henv ?(self=false) f ths =
  List.iter
    (fun thy ->
       let s = signature henv thy in
       Mid.iter (fun _ p -> f p) s.locals
    ) (dependencies henv ~self ths)

(* -------------------------------------------------------------------------- *)
