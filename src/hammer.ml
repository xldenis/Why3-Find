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
  client : Client.client option ;
  maxdepth : int ;
  provers : Runner.prover list ;
  transfs : string list ;
  minimize : bool ;
}

type node = {
  depth : int ;
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

let schedule profile ?(replay=false) ?(depth=0) goal hint =
  let result = Fibers.var () in
  Queue.push
    { profile ; goal ; hint ; replay ; depth ; result }
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

let prove env ?client ?cancel prover timeout : strategy = fun n ->
  let task = Session.goal_task n.goal in
  let name = Session.goal_name n.goal in
  let* alpha = Calibration.velocity env n.profile prover in
  let to_profile = Runner.map (fun t -> t /. alpha) in
  let to_runner = Runner.map (fun t -> t *. alpha) in
  let runner_time = timeout *. alpha in
  let callback = Session.result n.goal in
  let+ verdict =
    match Runner.prove_cached prover task runner_time with
    | `Cached result ->
      Runner.notify env prover result callback ;
      Fibers.return (to_profile result)
    | `Prepared task ->
      let kill_runner = Fibers.signal () in
      let kill_server = Fibers.signal () in
      let watch kill =
        function Runner.Valid _ -> Fibers.emit kill () | _ -> () in
      let cache_runner = Runner.store_cached prover task in
      let cache_server r = Runner.store_cached prover task (to_runner r) in
      Fibers.hook ?signal:cancel ~handler:(Fibers.emit kill_runner) @@
      Fibers.hook ?signal:cancel ~handler:(Fibers.emit kill_server) @@
      let+ runner =
        Fibers.map to_profile @@
        Fibers.finally ~handler:cache_runner @@
        Fibers.finally ~handler:(watch kill_server) @@
        (Format.printf "PROVE PREPARED %s@." (Runner.digest task) ;
         Runner.prove_prepared env ~name ~cancel:kill_runner ~callback
          prover task runner_time)
      and* server =
        match client with
        | None -> Fibers.return Runner.NoResult
        | Some cl ->
          Fibers.finally ~handler:cache_server @@
          Fibers.finally ~handler:(Runner.store_cached prover task) @@
          (Format.printf "PROVE SERVER %s@." (Runner.digest task) ;
           Client.prove cl n.profile ~cancel:kill_server prover task timeout)
      in Runner.merge runner server
  in
  match verdict with
  | Valid t -> Prover( Runner.name prover, Utils.round t )
  | _ -> Stuck

(*
  let+ local = Runner.prove env
      ?cancel ~name ~callback:(Session.result n.goal)
      prv task time in
  match local with
  | Valid t -> Prover( Runner.name prv, Utils.round @@ t /. alpha )
  | _ -> Stuck
*)


(* -------------------------------------------------------------------------- *)
(* --- Try Transformation on Node                                         --- *)
(* -------------------------------------------------------------------------- *)

let rec subgoals ({ profile ; replay ; depth } as n) goals hints =
  match goals, hints with
  | [], _ -> []
  | g::gs, [] ->
    schedule profile ~replay ~depth:(succ depth) g Stuck :: subgoals n gs []
  | g::gs, h::hs ->
    schedule profile ~replay ~depth:(succ depth) g h :: subgoals n gs hs

let apply env depth tr hs : strategy = fun n ->
  if n.depth > depth then stuck else
    match Session.apply env.Wenv.wenv tr n.goal with
    | None -> stuck
    | Some gs -> Crc.apply tr @+ Fibers.all @@ subgoals n gs hs

(* -------------------------------------------------------------------------- *)
(* --- Hammer Strategy                                                    --- *)
(* -------------------------------------------------------------------------- *)

let hammer0 env prvs time : strategy =
  smap (fun prv -> prove env prv time) prvs

let hammer1 env prvs ?client time : strategy = fun n ->
  let cancel = Fibers.signal () in
  let watch r = if r <> Stuck then Fibers.emit cancel () ; r in
  let+ results =
    Fibers.all @@ List.map
      (fun prv -> watch @+ prove env ?client ~cancel prv time n) prvs
  in try List.find (fun r -> r <> Stuck) results with Not_found -> Stuck

let hammer2 env trfs depth : strategy =
  smap (fun tr -> apply env depth tr []) trfs

let hammer henv =
  hammer0 henv.env henv.provers (henv.time *. 0.2) >>>
  hammer1 henv.env henv.provers ?client:henv.client henv.time >>>
  hammer2 henv.env henv.transfs henv.maxdepth >>>
  hammer1 henv.env henv.provers ?client:henv.client (henv.time *. 2.0)

(* -------------------------------------------------------------------------- *)
(* --- Node Processing                                                    --- *)
(* -------------------------------------------------------------------------- *)

let select p prvs = List.find (fun prv -> Runner.name prv = p) prvs

let overhead t = max (t *. 2.0) 1.0

let replay h p t =
  let client = if t > h.time *. 0.2 then h.client else None in
  prove h.env ?client (select p h.provers) (overhead t)

let update h p t = replay h p t >>> hammer h

let transf h id cs =
  apply h.env h.maxdepth id cs >>>
  hammer1 h.env h.provers h.time >>>
  hammer2 h.env (List.filter (fun f -> f <> id) h.transfs) h.maxdepth >>>
  hammer1 h.env h.provers (h.time *. 2.0)

let reduce h id cs =
  hammer1 h.env h.provers h.time >>>
  apply h.env h.maxdepth id cs >>>
  hammer2 h.env (List.filter (fun f -> f <> id) h.transfs) h.maxdepth >>>
  hammer1 h.env h.provers (h.time *. 2.0)

let process h : strategy = fun n ->
  match n.hint with
  | Stuck -> if n.replay then stuck else hammer h n
  | Prover(p,t) ->
    if n.replay then
      replay h p t n
    else
      update h p t n
  | Transf { id ; children } ->
    if h.minimize then
      reduce h id children n
    else if n.replay then
      apply h.env h.maxdepth id children n
    else
      transf h id children n

(* -------------------------------------------------------------------------- *)
(* --- Main Loop                                                          --- *)
(* -------------------------------------------------------------------------- *)

let run henv =
  try
    while true do
      Fibers.yield () ;
      Option.iter Client.yield henv.client ;
      let n2 = Queue.length q2 in
      let n1 = Queue.length q1 in
      let nq = Runner.pending () in
      let nr = Runner.running () in
      Utils.progress "%d/%d/%d/%d%t" n2 n1 nq nr Runner.pp_goals ;
      match pop () with
      | Some node ->
        Fibers.await
          (process henv node)
          (Fibers.set node.result)
      | None ->
        if Fibers.pending () > 0
        then Unix.sleepf 0.01
        else raise Exit
    done
  with Exit ->
    Option.iter Client.terminate henv.client

(* -------------------------------------------------------------------------- *)
