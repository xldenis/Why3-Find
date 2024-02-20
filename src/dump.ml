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
(* --- Dump AST                                                           --- *)
(* -------------------------------------------------------------------------- *)

open Why3
type 'a printer = Format.formatter -> 'a -> unit

(* -------------------------------------------------------------------------- *)
(* --- Basics                                                             --- *)
(* -------------------------------------------------------------------------- *)

let pp_id fmt (id : Ident.ident) = Format.fprintf fmt "%s" id.id_string

let pp_any fmt s = Format.fprintf fmt "(%s…)" s
[@@ warning "-32"]

let pp_nlist item fmt xs =
  let n = List.length xs in
  if n > 0 then Format.fprintf fmt "@,%s[%d]" item n
[@@ warning "-32"]

let pp_many item fmt empty =
  if not empty then Format.fprintf fmt "@,%s[…]" item
[@@ warning "-32"]

let pp_some item fmt opt =
  if opt then Format.fprintf fmt "%s" item
[@@ warning "-32"]

(* -------------------------------------------------------------------------- *)
(* --- Term                                                               --- *)
(* -------------------------------------------------------------------------- *)

let pp_term = Pretty.print_term

(* -------------------------------------------------------------------------- *)
(* --- Ity                                                                --- *)
(* -------------------------------------------------------------------------- *)

let pp_ity fmt (ity : Ity.ity) =
  match ity.ity_node with
  | Ityreg _ -> pp_any fmt "reg"
  | Ityvar x -> pp_id fmt x.tv_name
  | Ityapp(its,[],[]) -> Format.fprintf fmt "%a" pp_id its.its_ts.ts_name
  | Ityapp(its,xs,ys) ->
    Format.fprintf fmt "%a.%d.%d" pp_id its.its_ts.ts_name
      (List.length xs) (List.length ys)

let pp_pvsymbol_def fmt (pv : Ity.pvsymbol) =
  Format.fprintf fmt "@[<hov 2>(%a%a:%a)@]"
    (pp_some "ghost ") pv.pv_ghost
    pp_id pv.pv_vs.vs_name
    pp_ity pv.pv_ity

let pp_pvsymbol_use fmt (pv : Ity.pvsymbol) = pp_id fmt pv.pv_vs.vs_name

let pp_clause clause pp fmt c =
  Format.fprintf fmt "@ @[<hov 2>%s { %a }@]" clause pp c

let pp_cty fmt (cty : Ity.cty) =
  begin
    List.iter (Format.fprintf fmt "@,%a" pp_pvsymbol_def) cty.cty_args ;
    Format.fprintf fmt ":%a" pp_ity cty.cty_result ;
    List.iter (pp_clause "pre" pp_term fmt) cty.cty_pre ;
    List.iter (pp_clause "post" pp_term fmt) cty.cty_post ;
    pp_many " xpost" fmt (Ity.Mxs.is_empty cty.cty_xpost) ;
    pp_many " oldies" fmt (Ity.Mpv.is_empty cty.cty_oldies) ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Expr                                                               --- *)
(* -------------------------------------------------------------------------- *)

let pp_rs_kind fmt (rs : Expr.rs_kind) =
  match rs with
  | RKnone -> ()
  | RKlemma -> Format.fprintf fmt "lemma "
  | RKlocal -> Format.fprintf fmt "local "
  | RKfunc -> Format.fprintf fmt "function "
  | RKpred -> Format.fprintf fmt "predicate "

let pp_rsymbol_def fmt (rs : Expr.rsymbol) =
  Format.fprintf fmt "@[<hov 2>%a%a%a@]"
    pp_rs_kind (Expr.rs_kind rs)
    pp_id rs.rs_name pp_cty rs.rs_cty

let pp_rsymbol_use fmt (rs : Expr.rsymbol) =
  pp_id fmt rs.rs_name

let rec pp_expr fmt (expr : Expr.expr) =
  match expr.e_node with
  | Evar x -> pp_pvsymbol_use fmt x
  | Econst c -> Constant.print_def fmt c
  | Eassign _ -> pp_any fmt "Eassign"
  | Eif _ -> pp_any fmt "Eif"
  | Ematch _ -> pp_any fmt "Ematch"
  | Ewhile _ -> pp_any fmt "Ewhile"
  | Efor _ -> pp_any fmt "Efor"
  | Eraise _ -> pp_any fmt "Eraise"
  | Eexn _ -> pp_any fmt "Eexn"
  | Eassert(Assume,t) -> Format.fprintf fmt "@[<hov 2>(assume %a)@]" pp_term t
  | Eassert(Assert,t) -> Format.fprintf fmt "@[<hov 2>(assert %a)@]" pp_term t
  | Eassert(Check,t) -> Format.fprintf fmt "@[<hov 2>(check %a)@]" pp_term t
  | Eghost e -> Format.fprintf fmt "(ghost %a)" pp_expr e
  | Epure t -> Format.fprintf fmt "(pure %a)" pp_term t
  | Eabsurd -> Format.fprintf fmt "absurd"
  | Eexec(ce,_)-> Format.fprintf fmt "(exec %a)" pp_cexp ce
  | Elet(def,e) ->
    Format.fprintf fmt "@[<hv 0>%a@ in %a@]" pp_let_defn def pp_expr e

and pp_let_defn fmt (def : Expr.let_defn) =
  match def with
  | LDvar(pv,e) ->
    Format.fprintf fmt "@[<hov 4>let:pv %a = %a@]" pp_pvsymbol_def pv pp_expr e
  | LDsym(rs,e) ->
    Format.fprintf fmt "@[<hov 4>let:rs %a = %a@]" pp_rsymbol_def rs pp_cexp e
  | LDrec _ -> pp_any fmt "let:rec"

and pp_cexp fmt (cexp : Expr.cexp) =
  Format.fprintf fmt "@[<hv 2>" ;
  begin
    match cexp.c_node with
    | Cfun e -> Format.fprintf fmt "(Cfun %a)" pp_expr e
    | Capp(rs,pvs) ->
      Format.fprintf fmt "@[<hov 2>(Capp %a" pp_id rs.rs_name ;
      List.iter (Format.fprintf fmt "@ %a" pp_pvsymbol_use) pvs ;
      Format.fprintf fmt ")@]"
    | Cpur _ -> pp_any fmt "Cpur"
    | Cany -> Format.fprintf fmt "Cany"
  end ;
  pp_cty fmt cexp.c_cty ;
  Format.fprintf fmt "@]"

(* -------------------------------------------------------------------------- *)
