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
(* --- Why3 Utilities                                                     --- *)
(* -------------------------------------------------------------------------- *)

open Why3

(* -------------------------------------------------------------------------- *)
(* --- Iterators                                                          --- *)
(* -------------------------------------------------------------------------- *)

let iter_mi f (mi : Pmodule.mod_inst) =
  begin
    let open Ty in
    let open Ity in
    let open Term in
    let open Decl in
    let open Expr in
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

let iter_sm f (sm : Theory.symbol_map) =
  begin
    let open Ty in
    let open Term in
    let open Decl in
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

(* -------------------------------------------------------------------------- *)
(* --- Debug Printers                                                     --- *)
(* -------------------------------------------------------------------------- *)

let pp_thy fmt (th : Theory.theory) = Id.pp fmt th.th_name
let pp_mod fmt (m : Pmodule.pmodule) = pp_thy fmt m.mod_theory

let pp_decl fmt (d : Decl.decl) =
  match d.d_node with
  | Dtype ts ->
    begin match ts.ts_def with
      | NoDef ->
        Format.fprintf fmt "@ type %a" Id.pp ts.ts_name
      | Alias _ | Range _ | Float _ ->
        Format.fprintf fmt "@ type %a = .." Id.pp ts.ts_name
    end
  | Ddata ds ->
    List.iter (fun (ty,_) ->
        Format.fprintf fmt "@ type %a = <data>" Id.pp ty.Ty.ts_name
      ) ds
  | Dparam ls -> Format.fprintf fmt "@ function %a" Id.pp ls.ls_name
  | Dlogic lds ->
    List.iter (fun (ls,_) ->
        Format.fprintf fmt "@ function %a = .." Id.pp ls.Term.ls_name
      ) lds
  | Dind(_,lds) ->
    List.iter (fun (ls,_) ->
        Format.fprintf fmt "@ inductive %a = .." Id.pp ls.Term.ls_name
      ) lds
  | Dprop(Plemma, pr, _) -> Format.fprintf fmt "@ lemma %a" Id.pp pr.pr_name
  | Dprop(Paxiom, pr, _) -> Format.fprintf fmt "@ axiom %a" Id.pp pr.pr_name
  | Dprop(Pgoal, pr, _) -> Format.fprintf fmt "@ goal %a" Id.pp pr.pr_name

let pp_let_defn fmt (d : Expr.let_defn) =
  match d with
  | LDsym (rs, _) -> Format.fprintf fmt "@ val %a" Id.pp rs.rs_name
  | LDvar (pv, _) -> Format.fprintf fmt "@ let %a = .." Id.pp pv.pv_vs.vs_name
  | LDrec ls ->
    List.iter (fun (rd : Expr.rec_defn) ->
        Format.fprintf fmt "@ let rec %a = .." Id.pp rd.rec_sym.rs_name
      ) ls

let pp_pdecl fmt (d : Pdecl.pdecl) =
  match d.pd_node with
  | PDtype ts ->
    List.iter
      (fun (t : Pdecl.its_defn) ->
         let its = t.itd_its in
         Format.fprintf fmt "@ type %a" Id.pp its.its_ts.ts_name ;
         match its.its_def with
         | Alias _ | Range _ | Float _ -> Format.fprintf fmt " = .."
         | NoDef ->
           if its.its_private then
             Format.fprintf fmt " = private {..}"
           else
             Format.fprintf fmt " = {..}"
      ) ts
  | PDlet d -> pp_let_defn fmt d
  | PDexn e -> Format.fprintf fmt "@ exception %a" Id.pp e.xs_name
  | PDpure -> List.iter (pp_decl fmt) d.pd_pure

let rec pp_munit fmt (m : Pmodule.mod_unit) =
  match m with
  | Umeta _ -> ()
  | Uuse m -> Format.fprintf fmt "@ use %a" pp_mod m
  | Uscope(s,ms) ->
    Format.fprintf fmt "@ @[<v 0>@[<v 2>scope %s {" s ;
    List.iter (pp_munit fmt) ms ;
    Format.fprintf fmt "@]@ }@]"
  | Uclone mi ->
    Format.fprintf fmt "@ @[<v 0>@[<v 2>clone %a {" pp_mod mi.mi_mod ;
    iter_mi (fun a b ->
        Format.fprintf fmt "@ %a -> %a ;" Id.pp a Id.pp b
      ) mi ;
    Format.fprintf fmt "@]@ }@]"
  | Udecl d -> pp_pdecl fmt d

let pp_module fmt (m : Pmodule.pmodule) =
  begin
    Format.fprintf fmt "@[<hv 0>@[<hv 2>module %a {" pp_mod m ;
    List.iter (pp_munit fmt) m.mod_units ;
    Format.fprintf fmt "@]@ }@]" ;
  end

(* -------------------------------------------------------------------------- *)
