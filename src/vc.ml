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

let by_range (_,r1) (_,r2) = Range.compare_range r1 r2

let rank = function Goal -> 2 | WhenNot -> 1 | When -> 0

let rec segment = function
  | [] | [_] as rs -> rs
  | ((c1,r1) as u)::((c2,r2) as v)::w ->
    if Range.( r1 <<< r2 ) then u :: segment (v::w) else
    if c1 = c2 then segment ((c1, Range.union r1 r2)::w) else
    if rank c1 > rank c2 then
      (c1,r1) :: segment ((c2,Range.diff r2 r1) :: w)
    else
      (c1,Range.diff r1 r2) :: segment (v::w)

let rec collect ~file s acc =
  match s with
  | Cat(a,b) -> collect ~file a @@ collect ~file b @@ acc
  | Range(color,p) ->
    let f,a,b,c,d = Loc.get p in
    if f = file then (color,((a,b),(c,d))) :: acc else acc

let ranges ~file (t : Tsk.task) =
  match task t with
  | None -> []
  | Some s -> collect ~file s [] |> List.sort by_range |> segment

type cursor = {
  text : string ;
  mutable offset : int ;
  mutable pos : Range.pos ;
}

let forward print cursor =
  let k = cursor.offset in
  let c = cursor.text.[k] in
  if print then Format.print_char c ;
  cursor.pos <- Range.after cursor.pos c ;
  cursor.offset <- succ k

let skip = forward false
let print = forward true

let before cursor ~context u =
  let p = (fst @@ fst @@ snd u) - context in
  while fst cursor.pos < p do skip cursor done

let after cursor ~context v =
  let p = (fst @@ snd @@ snd v) + context in
  while fst cursor.pos <= p do print cursor done

let flush cursor =
  if snd cursor.pos > 0 then Format.print_newline () ;
  Format.printf "[...]@."

let open_color = function
  | Goal -> Format.printf "@{<bold>@{<bright red>"
  | When -> Format.printf "@{<green>"
  | WhenNot -> Format.printf "@{<magenta>"
let close_color = function
  | Goal -> Format.printf "@}@}"
  | When | WhenNot -> Format.printf "@}"

let rec decorate cursor u w =
  let open Range in
  let kd,(a,_) = u in
  if cursor.pos << a then
    (print cursor ; decorate cursor u w)
  else
    (open_color kd ; print cursor ; colored cursor u w)

and colored cursor u w =
  let open Range in
  let kd,(_,b) = u in
  if cursor.pos << b then
    (print cursor ; colored cursor u w)
  else
    (close_color kd ;
     match w with
     | [] -> u
     | v::w -> decorate cursor v w)

let dump ~file ~context task =
  ranges ~file task |>
  function [] -> () | u::w ->
    let text = Utils.readfile ~file in
    let cursor = { text ; offset = 0 ; pos = Range.start } in
    try
      Format.printf "[...]@." ;
      before cursor ~context u ;
      let v = decorate cursor u w in
      after cursor ~context v ;
      Format.printf "[...]@." ;
    with Invalid_argument _ | Exit ->
      flush cursor
