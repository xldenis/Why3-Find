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

type param =
  | Type of Ty.tysymbol
  | Logic of Term.lsymbol
  | Param of Expr.rsymbol
  | Value of Expr.rsymbol
  | Axiom of Decl.prsymbol

let ident = function
  | Type ty -> ty.ts_name
  | Logic ls -> ls.ls_name
  | Param rs -> rs.rs_name
  | Value rs -> rs.rs_name
  | Axiom pr -> pr.pr_name

type parameter = {
  param : param ;
  builtin : (Runner.prover * string) list ;
  extern : string option ;
}

let is_abstract { builtin ; extern } = builtin = [] && extern = None

type signature = {
  abstract : int ; (* number of abstract parameters *)
  locals : parameter Mid.t ;
  used_theories : Theory.theory list ;
  cloned_theories : Theory.theory list ;
}

type henv = {
  builtins : (Runner.prover * string) list Mid.t ;
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
    ) bmap @@ Driver.syntax_map p.Runner.driver

let init (wenv : Wenv.env) =
  let provers = Runner.select wenv ~patterns:(Wenv.provers ()) in
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
  abstract = 0 ;
  locals = Mid.empty ;
  used_theories = [] ;
  cloned_theories = [] ;
}

let add henv hs param =
  let id = ident param in
  let builtin = Mid.find_def [] id henv.builtins in
  let extern = Mid.find_opt id henv.externals in
  let prm = { param ; builtin ; extern } in
  let abstract = if is_abstract prm then succ hs.abstract else hs.abstract in
  { hs with abstract ; locals = Mid.add id prm hs.locals }

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

let add_decl henv (hs : signature) (d : Decl.decl) : signature =
  match d.d_node with
  | Dlogic _ | Dind _ | Ddata _ -> hs
  | Dtype ty -> add_type henv hs ty
  | Dparam ls -> add henv hs (Logic ls)
  | Dprop(Paxiom,pr,_) -> add henv hs (Axiom pr)
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
(* --- Constant Detection                                                 --- *)
(* -------------------------------------------------------------------------- *)

let is_var x (a : Term.term) =
  match a.t_node with
  | Tvar y -> Term.vs_equal x y
  | _ -> false

let rec cany (ce : Expr.cexp) =
  match ce.c_node with
  | Cany -> true
  | Cfun { e_node = Eexec(ce,_) } -> cany ce
  | _ -> false

(* Detect (fun result -> result = a) s.t. check a *)
let constrained_post check (p : Ity.post) =
  match p.t_node with
  | Teps rp ->
    let r,post = Term.t_open_bound rp in
    begin
      match post.t_node with
      | Ttrue -> false
      | Tapp(f,[a;b]) ->
        not Term.(ls_equal f ps_equ) ||
        not @@ is_var r a ||
        not @@ check b
      | _ -> true
    end
  | _ -> true

let constrained (rs : Expr.rsymbol) =
  rs.rs_cty.cty_pre <> [] ||
  not @@ Ity.Mxs.is_empty rs.rs_cty.cty_xpost ||
  match rs.rs_logic with
  | RLlemma -> true
  | RLnone -> rs.rs_cty.cty_post <> []
  | RLpv pv ->
    let check = is_var pv.pv_vs in
    List.exists (constrained_post check) rs.rs_cty.cty_post
  | RLls ls ->
    rs.rs_cty.cty_post <> [] &&
    let xs = List.map (fun pv -> pv.Ity.pv_vs) rs.rs_cty.cty_args in
    let check (a : Term.term) =
      match a.t_node with
      | Tapp(f,es) ->
        (try Term.ls_equal f ls && List.for_all2 is_var xs es
         with Invalid_argument _ -> false)
      | _ -> false
    in List.exists (constrained_post check) rs.rs_cty.cty_post

(* -------------------------------------------------------------------------- *)
(* --- Module Declarations                                                --- *)
(* -------------------------------------------------------------------------- *)

let add_mtype henv (hs : signature) (it : Pdecl.its_defn) : signature =
  if it.itd_fields = [] && it.itd_constructors = [] then
    let its = it.itd_its in
    if nodef its.its_def then add henv hs (Type its.its_ts) else hs
  else hs

let add_rsymbol henv hs (rs : Expr.rsymbol) (cexp : Expr.cexp) =
  if Id.lemma rs.rs_name then hs else
    match rs.rs_logic with
    | RLlemma -> hs
    | RLnone | RLpv _ | RLls _ ->
      if cany cexp then
        if constrained rs then
          add henv hs (Value rs)
        else
          add henv hs (Param rs)
      else hs

let add_pdecl henv (hs : signature) (d : Pdecl.pdecl) : signature =
  match d.pd_node with
  | PDtype tys -> List.fold_left (add_mtype henv) hs tys
  | PDlet (LDsym(r,c)) -> add_rsymbol henv hs r c
  | PDlet (LDvar _ | LDrec _) | PDexn _ -> hs
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
let parameters ?(all=false) s =
  if all then Mid.values s.locals else
    List.filter_map
      (fun p -> if is_abstract p then Some p else None)
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
