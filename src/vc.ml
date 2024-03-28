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
(* ---  Task Colorization                                                 --- *)
(* -------------------------------------------------------------------------- *)

module Loc = Why3.Loc
module Term = Why3.Term
module Decl = Why3.Decl
module Thy = Why3.Theory
module Tsk = Why3.Task
module Thy = Why3.Theory

type color = Goal | When | WhenNot
type colored =
  | Cat of colored * colored (* each is colored *)
  | Range of color * Loc.position
type shape = colored option

(* Collects all colored parts the two shapes *)
let (<|>) (a : shape) (b : shape) : shape =
  match a,b with
  | None,c | c,None -> c
  | Some a, Some b -> Some(Cat(a,b))

(* Combines a and b only if they are both colored shapes *)
let (<*>) (a : shape) (b : shape) : shape =
  match a,b with
  | None,_ | _,None -> None
  | Some a, Some b -> Some (Cat(a,b))

let range (c: color) = function None -> None | Some p -> Some(Range(c,p))

let apply c p = function Some _ as s -> s | None -> range c p

let rec term ~goal (t:Term.term) : shape =
  let hyp = term ~goal:false in
  let prv = term ~goal:true in
  apply (if goal then Goal else When) t.t_loc @@
  match t.t_node with
  | Tbinop(Timplies,h,p) when goal -> hyp h <*> prv p
  | Tbinop(Tand,a,b) when not goal -> hyp a <*> hyp b
  | Tnot p -> range WhenNot p.t_loc
  | Tlet(_,bnd) ->
    let _,p = Term.t_open_bound bnd in prv p
  | Tquant(Tforall,bnd) when goal ->
    let _,_,p = Term.t_open_quant bnd in prv p
  | _ -> None

let decl (d: Decl.decl) : shape =
  match d.d_node with
  | Dprop(Pgoal,pr,f) -> apply Goal pr.pr_name.id_loc @@ term ~goal:true f
  | Dprop(Paxiom,pr,f) -> apply When pr.pr_name.id_loc @@ term ~goal:false f
  | _ -> None

let tdecl (td : Thy.tdecl) : shape =
  match td.td_node with Decl d -> decl d | _ -> None

let rec task (t : Tsk.task) : shape =
  match t with
  | None -> None
  | Some { task_decl ; task_prev } ->
    tdecl task_decl <|> task task_prev

let rec collect s acc =
  match s with
  | Range(c,p) -> (c,p)::acc
  | Cat(a,b) -> collect a @@ collect b @@ acc

let ranges (t : Tsk.task) =
  match task t with
  | None -> []
  | Some s -> collect s []
