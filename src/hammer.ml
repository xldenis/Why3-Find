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
(* --- Hammer Proof Strategy                                              --- *)
(* -------------------------------------------------------------------------- *)

open Crc
open Fibers.Monad

type henv = {
  env : Wenv.env ;
  time : float ;
  provers : Runner.prover list ;
  transfs : string list ;
  minimize : bool ;
}

type node = {
  profile : Calibration.profile ;
  goal : Session.goal ;
  hint : crc ;
  replay : bool ;
  result : crc Fibers.var ;
}

(* -------------------------------------------------------------------------- *)
(* --- Queue Management                                                   --- *)
(* -------------------------------------------------------------------------- *)

let q1 : node Queue.t = Queue.create ()
let q2 : node Queue.t = Queue.create ()

let schedule profile ?(replay=false) goal hint =
  let result = Fibers.var () in
  Queue.push
    { profile ; goal ; hint ; replay ; result }
    (if complete hint then q2 else q1) ;
  Fibers.get result

let pop () =
  try Some (Queue.pop q1) with Queue.Empty ->
  try Some (Queue.pop q2) with Queue.Empty ->
    None

let stuck = Fibers.return Stuck

type strategy = node -> crc Fibers.t
let fail : strategy = fun _ -> stuck

let (@>) (h : strategy) (n : node) : crc Fibers.t =
  try h n with Not_found -> stuck

let (>>>) (h1 : strategy) (h2 : strategy) : strategy = fun n ->
  let* r = h1 @> n in
  if r <> Stuck then Fibers.return r else h2 @> n

let rec smap (f : 'a -> strategy) (xs : 'a list) : strategy =
  match xs with
  | [] -> fail
  | x::xs -> f x >>> smap f xs

(* -------------------------------------------------------------------------- *)
(* --- Try Prover on Node                                                 --- *)
(* -------------------------------------------------------------------------- *)

let local = ref false

let prove env ?cancel prv timeout : strategy = fun n ->
  let* alpha =
    if !local then
      Fibers.return 1.0
    else
      Calibration.velocity env n.profile prv
  in
  let time = timeout *. alpha in
  let task = Session.goal_task n.goal in
  let name = Session.goal_name n.goal in
  let+ result = Runner.prove env
      ?cancel ~name ~callback:(Session.result n.goal)
      task prv time
  in
  match result with
  | Valid t -> Prover( Runner.name prv, Utils.round @@ t /. alpha )
  | _ -> Stuck

(* -------------------------------------------------------------------------- *)
(* --- Try Transformation on Node                                         --- *)
(* -------------------------------------------------------------------------- *)

let rec subgoals n goals hints =
  match goals, hints with
  | [], _ -> []
  | g::gs, [] ->
    schedule n.profile ~replay:n.replay g Stuck :: subgoals n gs []
  | g::gs, h::hs ->
    schedule n.profile ~replay:n.replay g h :: subgoals n gs hs

let apply env tr hs : strategy = fun n ->
  match Session.apply env.Wenv.wenv tr n.goal with
  | None -> stuck
  | Some gs -> Crc.apply tr @+ Fibers.all @@ subgoals n gs hs

(* -------------------------------------------------------------------------- *)
(* --- Hammer Strategy                                                    --- *)
(* -------------------------------------------------------------------------- *)

let hammer0 env prvs time : strategy =
  smap (fun prv -> prove env prv time) prvs

let hammer1 env prvs time : strategy = fun n ->
  let cancel = Fibers.signal () in
  let watch r = if r <> Stuck then Fibers.emit cancel () ; r in
  let+ results =
    Fibers.all @@ List.map
      (fun prv -> watch @+ prove env ~cancel prv time n) prvs
  in try List.find (fun r -> r <> Stuck) results with Not_found -> Stuck

let hammer2 env trfs : strategy =
  smap (fun tr -> apply env tr []) trfs

let hammer henv =
  hammer0 henv.env henv.provers (henv.time *. 0.2) >>>
  hammer1 henv.env henv.provers henv.time >>>
  hammer2 henv.env henv.transfs >>>
  hammer1 henv.env henv.provers (henv.time *. 2.0)

(* -------------------------------------------------------------------------- *)
(* --- Node Processing                                                    --- *)
(* -------------------------------------------------------------------------- *)

let select p prvs = List.find (fun prv -> Runner.name prv = p) prvs

let overhead t = max (t *. 2.0) 1.0

let prove h p t = prove h.env (select p h.provers) (overhead t)
let update h p t = prove h p t >>> hammer h

let transf h id cs =
  apply h.env id cs >>>
  hammer1 h.env h.provers h.time >>>
  hammer2 h.env (List.filter (fun f -> f <> id) h.transfs) >>>
  hammer1 h.env h.provers (h.time *. 2.0)

let reduce h id cs =
  hammer1 h.env h.provers h.time >>>
  apply h.env id cs >>>
  hammer2 h.env (List.filter (fun f -> f <> id) h.transfs) >>>
  hammer1 h.env h.provers (h.time *. 2.0)

let process h : strategy = fun n ->
  match n.hint with
  | Stuck -> if n.replay then stuck else hammer h n
  | Prover(p,t) ->
    if n.replay then
      prove h p t n
    else
      update h p t n
  | Transf { id ; children } ->
    if h.minimize then
      reduce h id children n
    else if n.replay then
      apply h.env id children n
    else
      transf h id children n

(* -------------------------------------------------------------------------- *)
(* --- Main Loop                                                          --- *)
(* -------------------------------------------------------------------------- *)

let rec exec (s : strategy) =
  Fibers.yield () ;
  let n1 = Queue.length q1 + Runner.pending () in
  let n2 = Queue.length q2 in
  let nr = Runner.running () in
  Utils.progress "%d/%d/%d%t" n2 n1 nr Runner.pp_goals ;
  match pop () with
  | None ->
    if Fibers.pending () > 0 then (Unix.sleepf 0.01 ; exec s)
  | Some node ->
    Fibers.await (s @> node) (Fibers.set node.result) ; exec s

let run henv = exec (process henv)

(* -------------------------------------------------------------------------- *)
