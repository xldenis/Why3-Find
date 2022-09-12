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
}

type node = {
  profile : Calibration.profile ;
  goal : Session.goal ;
  hint : crc ;
  result : crc Fibers.var ;
}

(* -------------------------------------------------------------------------- *)
(* --- Queue Management                                                   --- *)
(* -------------------------------------------------------------------------- *)

let q1 : node Queue.t = Queue.create ()
let q2 : node Queue.t = Queue.create ()

let schedule profile goal hint =
  let result = Fibers.var () in
  Queue.push
    { profile ; goal ; hint ; result }
    (if complete hint then q2 else q1) ;
  Fibers.get result

let pop () =
  try Some (Queue.pop q1) with Queue.Empty ->
  try Some (Queue.pop q2) with Queue.Empty ->
    None

let stuck = Fibers.return Stuck

type strategy = node -> crc Fibers.t
let fail : strategy = fun _ -> stuck
let (>>>) (h1 : strategy) (h2 : strategy) : strategy = fun n ->
  match h1 n with
  | exception Not_found -> h2 n
  | job -> Fibers.bind job
             (fun r -> if r <> Stuck then Fibers.return r else h2 n)

(* -------------------------------------------------------------------------- *)
(* --- Try Prover on Node                                                 --- *)
(* -------------------------------------------------------------------------- *)

let prove env ?cancel prv timeout : strategy = fun n ->
  let* alpha = Calibration.velocity env n.profile prv in
  let time = timeout *. alpha in
  let task = Session.goal_task n.goal in
  let+ result =
    Runner.prove env ?cancel ~callback:(Session.result n.goal) task prv time
  in match result with
  | Valid t -> Prover( Runner.name prv, t /. alpha )
  | _ -> Stuck

(* -------------------------------------------------------------------------- *)
(* --- Hammer Strategy                                                    --- *)
(* -------------------------------------------------------------------------- *)

let rec hammer0 env prvs time : strategy =
  match prvs with
  | [] -> fail
  | prv::prvs -> prove env prv time >>> hammer0 env prvs time

let hammer1 env prvs time : strategy = fun n ->
  let cancel = Fibers.signal () in
  let watch r = if r <> Stuck then Fibers.emit cancel () ; r in
  let+ results =
    Fibers.all @@ List.map
      (fun prv -> watch @+ prove env ~cancel prv time n) prvs
  in List.find (fun r -> r <> Stuck) results

let hammer henv =
  hammer0 henv.env henv.provers 0.1 >>>
  hammer1 henv.env henv.provers 1.0 >>>
  hammer1 henv.env henv.provers 5.0

(* -------------------------------------------------------------------------- *)
(* --- Node Processing                                                    --- *)
(* -------------------------------------------------------------------------- *)

let select p prvs = List.find (fun prv -> Runner.name prv = p) prvs

let hint henv : strategy = fun n ->
  match n.hint with
  | Stuck -> stuck
  | Prover(p,time) -> prove henv.env (select p henv.provers) time n
  | Transf _ -> stuck

let process henv : strategy = hint henv >>> hammer henv

(* -------------------------------------------------------------------------- *)
(* --- Main Loop                                                          --- *)
(* -------------------------------------------------------------------------- *)

let rec run henv =
  Fibers.yield () ;
  let n1 = Queue.length q1 in
  let n2 = Queue.length q2 in
  let nr = Runner.running () in
  Utils.progress "%d/%d/%d" n2 n1 nr ;
  match pop () with
  | None ->
    if Fibers.pending () > 0 then
      begin
        Unix.sleepf 0.01 ;
        run henv
      end
  | Some node ->
    Fibers.await (process henv node) (Fibers.set node.result) ;
    run henv

(* -------------------------------------------------------------------------- *)
