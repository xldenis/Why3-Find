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
(* --- Builtin & Extracted Environments                                   --- *)
(* -------------------------------------------------------------------------- *)

type henv = {
  builtins : (Runner.prover * string) list Mid.t ;
  externals : string Mid.t ;
}

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
    ) bmap @@ Driver.syntax_map p.Runner.driver

let init (wenv : Wenv.env) =
  let provers = Runner.select wenv @@ Wenv.provers () in
  let builtins = List.fold_left add_builtins Mid.empty provers in
  let drivers = List.concat_map drivers wenv.pkgs @ Wenv.drivers () in
  let main = Whyconf.get_main wenv.wconfig in
  let pdriver = Pdriver.load_driver main wenv.wenv ocaml64 drivers in
  let externals = Mid.map fst pdriver.drv_syntax in
  { builtins ; externals }

(* -------------------------------------------------------------------------- *)
(* --- Theory & Module Assumed Symbols                                    --- *)
(* -------------------------------------------------------------------------- *)

type kind =
  | Type of Ty.tysymbol
  | Logic of Term.lsymbol
  | Value of Expr.rsymbol
  | Axiom of Decl.prsymbol

let ident = function
  | Type ty -> ty.ts_name
  | Logic ls -> ls.ls_name
  | Value rs -> rs.rs_name
  | Axiom pr -> pr.pr_name

type parameter = {
  kind : kind ;
  builtin : (Runner.prover * string) list ;
  extern : string option ;
}

let is_assumed { builtin ; extern } =
  builtin = [] && extern = None

type signature = {
  assumed : int ;
  locals : parameter Mid.t ;
  used_theories : Theory.theory list ;
  cloned_theories : Theory.theory list ;
}

let empty = {
  assumed = 0 ;
  locals = Mid.empty ;
  used_theories = [] ;
  cloned_theories = [] ;
}

let add henv hs kind =
  let id = ident kind in
  let builtin = Mid.find_def [] id henv.builtins in
  let extern = Mid.find_opt id henv.externals in
  let prm = { kind ; builtin ; extern } in
  let assumed = if is_assumed prm then succ hs.assumed else hs.assumed in
  { hs with assumed ; locals = Mid.add id prm hs.locals }

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
  if nodef ty.ts_def then add henv hs (Type ty) else hs

let add_prop henv hs (pk : Decl.prop_kind) (pr : Decl.prsymbol) =
  match pk with
  | Plemma | Pgoal -> hs
  | Paxiom -> add henv hs (Axiom pr)

let add_decl henv (hs : signature) (d : Decl.decl) : signature =
  match d.d_node with
  | Dtype ty -> add_type henv hs ty
  | Ddata _ -> hs
  | Dparam ls -> add henv hs (Logic ls)
  | Dlogic _ | Dind _ -> hs
  | Dprop(pk,pr,_) -> add_prop henv hs pk pr

let add_tdecl henv (hs : signature) (d : Theory.tdecl) : signature =
  match d.td_node with
  | Decl d -> add_decl henv hs d
  | Use thy -> add_used hs thy
  | Clone(thy,_) -> add_cloned hs thy
  | Meta _ -> hs

let theory_signature henv (thy : Theory.theory) =
  List.fold_left (add_tdecl henv) empty thy.th_decls

(* -------------------------------------------------------------------------- *)
(* --- Module Declarations                                                --- *)
(* -------------------------------------------------------------------------- *)

let add_mtype henv (hs : signature) (it : Pdecl.its_defn) : signature =
  let its = it.itd_its in
  if nodef its.its_def then add henv hs (Type its.its_ts) else hs

let add_rsymbol henv hs (rs : Expr.rsymbol) (cexp : Expr.cexp) =
  match cexp.c_node with
  | Cany when not @@ Id.lemma rs.rs_name -> add henv hs (Value rs)
  | _ -> hs

let add_letrec henv (hs : signature) (def : Expr.rec_defn) : signature =
  add_rsymbol henv hs def.rec_sym def.rec_fun

let add_pdecl henv (hs : signature) (d : Pdecl.pdecl) : signature =
  match d.pd_node with
  | PDtype tys -> List.fold_left (add_mtype henv) hs tys
  | PDlet (LDvar _) -> hs
  | PDlet (LDsym(r,c)) -> add_rsymbol henv hs r c
  | PDlet (LDrec defs) -> List.fold_left (add_letrec henv) hs defs
  | PDexn _ -> hs
  | PDpure -> List.fold_left (add_decl henv) hs d.pd_pure

let rec add_munit henv (hs : signature) (m : Pmodule.mod_unit) : signature =
  match m with
  | Udecl pd -> add_pdecl henv hs pd
  | Uuse pm -> add_used hs pm.mod_theory
  | Uclone mi -> add_cloned hs mi.mi_mod.mod_theory
  | Uscope(_,ms) -> List.fold_left (add_munit henv) hs ms
  | Umeta _ -> hs

let module_signature henv (pm : Pmodule.pmodule) : signature =
  List.fold_left (add_munit henv) empty pm.mod_units

(* -------------------------------------------------------------------------- *)
(* --- Signatute                                                          --- *)
(* -------------------------------------------------------------------------- *)

let sigs = Hid.create 0

let signature henv (thy : Theory.theory) =
  let id = thy.th_name in
  try Hid.find sigs id
  with Not_found ->
    let hs =
      try module_signature henv @@ Pmodule.restore_module thy
      with Not_found -> theory_signature henv thy
    in Hid.add sigs id hs ; hs

let parameter s id = Mid.find_opt id s.locals
let parameters s = Mid.values s.locals
let assumed s =
  List.filter_map
    (fun p -> if is_assumed p then Some p.kind else None)
    (Mid.values s.locals)

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
