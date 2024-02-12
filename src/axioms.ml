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
  | Unsafe of Ident.ident

let ident = function
  | Type ty -> ty.ts_name
  | Logic ls -> ls.ls_name
  | Param rs -> rs.rs_name
  | Value rs -> rs.rs_name
  | Axiom pr -> pr.pr_name
  | Unsafe id -> id

type parameter = {
  param : param ;
  builtin : (Runner.prover * string) list ;
  extern : string option ;
}

let is_external { builtin ; extern } = builtin <> [] || extern <> None

let is_unsafe = function
  | { param = (Unsafe _) } -> true
  | { param = (Value _ | Axiom _ | Type _ | Param _ | Logic _) } -> false

let is_hypothesis = function
  | { param = (Value _ | Axiom _ | Unsafe _) } -> true
  | { param = (Type _ | Param _ | Logic _) } -> false

type signature = {
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
  locals = Mid.empty ;
  used_theories = [] ;
  cloned_theories = [] ;
}

let add henv hs param =
  let id = ident param in
  let builtin = Mid.find_def [] id henv.builtins in
  let extern = Mid.find_opt id henv.externals in
  let prm = { param ; builtin ; extern } in
  { hs with locals = Mid.add id prm hs.locals }

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

(* Detect (fun result -> result = a) s.t. check a *)
let constrained_post ?unless (p : Ity.post) =
  match p.t_node with
  | Teps rp ->
    let r,post = Term.t_open_bound rp in
    begin
      match post.t_node with
      | Tapp(f,[a;b]) ->
        not Term.(ls_equal f ps_equ) ||
        not @@ is_var r a ||
        (match unless with None -> true | Some fn -> not @@ fn b)
      | _ -> true
    end
  | _ -> true

let constrained_cty ?unless (cty : Ity.cty) =
  cty.cty_pre <> [] ||
  not @@ Ity.Mxs.is_empty cty.cty_xpost ||
  List.exists (constrained_post ?unless) cty.cty_post

let constrained (rs : Expr.rsymbol) =
  match rs.rs_logic with
  | RLlemma -> true
  | RLnone ->
    constrained_cty rs.rs_cty
  | RLpv pv ->
    let unless = is_var pv.pv_vs in
    constrained_cty ~unless rs.rs_cty
  | RLls ls ->
    if rs.rs_cty.cty_post <> [] then
      let xs = List.map (fun pv -> pv.Ity.pv_vs) rs.rs_cty.cty_args in
      let unless (a : Term.term) =
        match a.t_node with
        | Tapp(f,es) ->
          (try Term.ls_equal f ls && List.for_all2 is_var xs es
           with Invalid_argument _ -> false)
         | _ -> false
      in
      constrained_cty ~unless rs.rs_cty
    else
      constrained_cty rs.rs_cty

[@@@ warning "-32"]

let rec print_cexp fmt (ce : Expr.cexp) =
  match ce.c_node with
  | Cany -> Format.fprintf fmt "Cany"
  | Cfun({ e_node = Eexec(ce,cty) }) ->
    Format.fprintf fmt "@[<hov 2>Exec(%a%a)@]" print_cexp ce print_cty cty
  | Cfun _ -> Format.fprintf fmt "Cfun _"
  | Capp _ -> Format.fprintf fmt "Capp _"
  | Cpur _ -> Format.fprintf fmt "Cpur _"

and print_cty fmt (cty : Ity.cty) =
  begin
    List.iter (fun p ->
      Format.fprintf fmt "@ requires { @[<hov 2>%a@] }"
        Pretty.print_term p
      ) cty.cty_pre ;
    List.iter (fun p ->
        Format.fprintf fmt "@ ensures { @[<hov 2>%a@] }"
          Pretty.print_term p
      ) cty.cty_post ;
  end

[@@@ warning "+32"]

(* -------------------------------------------------------------------------- *)
(* --- Sound Expression Detection                                         --- *)
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
  | Cany -> ce.c_cty.cty_args = [] (* this is not a val *)

(* -------------------------------------------------------------------------- *)
(* --- Module Declarations                                                --- *)
(* -------------------------------------------------------------------------- *)

let add_mtype henv (hs : signature) (it : Pdecl.its_defn) : signature =
  if it.itd_fields = [] && it.itd_constructors = [] then
    let its = it.itd_its in
    if nodef its.its_def then add henv hs (Type its.its_ts) else hs
  else hs

let is_undefined (ce : Expr.cexp) =
  match ce.c_node with
  | Cany -> true
  | Cfun { e_node = Eexec({ c_node = Cany },_) } -> true
  | _ -> false

let add_rsymbol henv hs (rs : Expr.rsymbol) (cexp : Expr.cexp) =
  if Id.lemma rs.rs_name then hs else
    match Expr.rs_kind rs with
    | RKlemma -> hs
    | RKfunc | RKpred | RKnone | RKlocal ->
      if is_undefined cexp then
        add henv hs (if constrained rs then Value rs else Param rs)
      else
      if not @@ safe_cexpr cexp then
        add henv hs (Unsafe rs.rs_name)
      else
        hs

let add_psymbol henv hs (pv : Ity.pvsymbol) (exp : Expr.expr) =
  if safe_expr exp then hs else add henv hs (Unsafe pv.pv_vs.vs_name)

let add_rec_def henv hs (def : Expr.rec_defn) =
  if safe_rec_defn def then hs else add henv hs (Unsafe def.rec_sym.rs_name)

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
