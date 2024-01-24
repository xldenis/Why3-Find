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
  tactics : string list ;
  minimize : bool ;
}

type node = {
  depth : int ;
  profile : Calibration.profile ;
  goal : Session.goal ;
  hint : crc ;
  replay : bool ;
  result : crc Fibers.t ;
}

(* -------------------------------------------------------------------------- *)
(* --- Queue Management                                                   --- *)
(* -------------------------------------------------------------------------- *)

let q1 : node Queue.t = Queue.create ()
let q2 : node Queue.t = Queue.create ()

let schedule profile ~replay ~depth goal hint =
  let result = Fibers.var () in
  Queue.push
    { profile ; goal ; hint ; replay ; depth ; result }
    (if is_complete hint then q2 else q1) ;
  result

let pop () =
  try Some (Queue.pop q1) with Queue.Empty ->
  try Some (Queue.pop q2) with Queue.Empty ->
    None

let stuck n = Fibers.return (stuck (Some n.goal))

type strategy = node -> crc Fibers.t
let fail : strategy = fun n -> stuck n

let (@>) (h : strategy) (n : node) : crc Fibers.t =
  try h n with Not_found -> stuck n

let (>>>) (h1 : strategy) (h2 : strategy) : strategy = fun n ->
  let* r = h1 @> n in
  if not @@ is_stuck r then Fibers.return r else h2 @> n

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
  let runner_time = timeout *. alpha in
  let callback = Session.result n.goal in
  let+ verdict =
    match Runner.prove_cached prover task runner_time with
    | `Cached result ->
      Runner.notify env prover result callback ;
      Fibers.return (to_profile result)
    | `Prepared task ->
      let kill = Fibers.signal () in
      let runner =
        Fibers.monitor ?signal:cancel ~handler:(Fibers.emit kill) @@
        Fibers.map to_profile @@
        Fibers.finally ~callback:(Runner.update prover task) @@
        Runner.prove_prepared env ~name ~cancel:kill ~callback
          prover task runner_time
      in match client with
      | None -> runner
      | Some cl ->
        let signal = Client.request cl n.profile prover task timeout in
        let handler r =
          match Runner.crop ~timeout r with
          | Some r ->
            Fibers.set runner r ;
            Fibers.emit kill ()
          | None -> ()
        in Fibers.monitor ~signal ~handler runner
  in match verdict with
  | Valid t -> Crc.prover (Some n.goal) (Runner.name prover) (Utils.round t)
  | _ -> Crc.stuck (Some n.goal)

(* -------------------------------------------------------------------------- *)
(* --- Try Transformation on Node                                         --- *)
(* -------------------------------------------------------------------------- *)

let rec subgoals ({ profile ; replay ; depth } as n) goals hints =
  match goals, hints with
  | [], _ -> []
  | g::gs, [] ->
    let h = Crc.stuck None in
    schedule profile ~replay ~depth:(succ depth) g h :: subgoals n gs []
  | g::gs, h::hs ->
    schedule profile ~replay ~depth:(succ depth) g h :: subgoals n gs hs

let apply env depth tr hs : strategy = fun n ->
  if n.depth > depth then stuck n else
    match Session.apply env.Wenv.wenv tr n.goal with
    | None -> stuck n
    | Some gs -> Crc.tactic (Some n.goal) tr @+ Fibers.all @@ subgoals n gs hs

(* -------------------------------------------------------------------------- *)
(* --- Hammer Strategy                                                    --- *)
(* -------------------------------------------------------------------------- *)

let hammer0 h : strategy =
  let time = h.time *. 0.2 in
  smap (fun prv -> prove h.env prv time) h.provers

let hammer13 h time : strategy = fun n ->
  let cancel = Fibers.signal () in
  let watch r = if not @@ Crc.is_stuck r then Fibers.emit cancel () ; r in
  let+ results =
    Fibers.all @@ List.map
      (fun prv -> watch @+ prove h.env ?client:h.client ~cancel prv time n)
      h.provers in
  try List.find (fun r -> not @@ Crc.is_stuck r) results
  with Not_found -> Crc.stuck (Some n.goal)

let hammer1 h = hammer13 h h.time
let hammer3 h = hammer13 h (2.0 *. h.time)

let hammer2 ?excluded h : strategy =
  smap
    (fun tr -> apply h.env h.maxdepth tr [])
    (match excluded with
     | None -> h.tactics
     | Some id -> List.filter (fun t -> t <> id) h.tactics)

let hammer henv =
  hammer0 henv >>>
  hammer1 henv >>>
  hammer2 henv >>>
  hammer3 henv

(* -------------------------------------------------------------------------- *)
(* --- Node Processing                                                    --- *)
(* -------------------------------------------------------------------------- *)

let select p prvs = List.find (fun prv -> Runner.name prv = p) prvs

let overhead t = max (t *. 2.0) 1.0

let replay h p t =
  let client = if t > h.time *. 0.2 then h.client else None in
  prove h.env ?client (select p h.provers) (overhead t)

let update h p t = replay h p t >>> hammer h

let tactic h id cs =
  apply h.env h.maxdepth id cs >>>
  hammer1 h >>>
  hammer2 ~excluded:id h >>>
  hammer3 h

let reduce h id cs =
  hammer1 h >>>
  apply h.env h.maxdepth id cs >>>
  hammer2 ~excluded:id h >>>
  hammer3 h

let process h : strategy = fun n ->
  try
    match n.hint.state with
    | Stuck -> if n.replay then stuck n else hammer h n
    | Prover(p,t) ->
      if n.replay then
        replay h p t n
      else
        update h p t n
    | Tactic { id ; children } ->
      if h.minimize then
        reduce h id children n
      else if n.replay then
        apply h.env h.maxdepth id children n
      else
        tactic h id children n
  with exn ->
    Utils.log "Process Error (%s)" @@ Printexc.to_string exn ;
    stuck n

(* -------------------------------------------------------------------------- *)
(* --- Main Loop                                                          --- *)
(* -------------------------------------------------------------------------- *)

let run henv =
  let exception Break in
  try
    let p2 = ref 0 in
    let p1 = ref 0 in
    while true do
      Fibers.yield () ;
      Option.iter Client.yield henv.client ;
      let n2 = Queue.length q2 + !p2 in
      let n1 = Queue.length q1 + !p1 in
      let nq = Runner.pending () in
      let nr = Runner.running () in
      let total = n2 + n1 + nq + nr in
      Utils.progress "%d/%d/%d/%d%t" n2 n1 nq nr Runner.pp_goals ;
      match pop () with
      | Some node ->
        Fibers.background @@
        begin
          let pr = if is_complete node.hint then p2 else p1 in
          incr pr ;
          let* r = process henv node in
          decr pr ;
          Fibers.set node.result r ;
          Fibers.return ()
        end
      | None ->
        if total > 0
        then Unix.sleepf 0.01
        else raise Break
    done
  with Break ->
    Option.iter Client.terminate henv.client

(* -------------------------------------------------------------------------- *)
(* --- Why3 Hammer Config                                                 --- *)
(* -------------------------------------------------------------------------- *)

let session = ".why3find"
let hammer = ".why3find/hammer.cfg"

let config ~tactics ~provers ~time ~mem =
  let c_skip fmt = ignore fmt in
  let c_label l fmt = Format.fprintf fmt "%s:@\n" l in
  let c_goto l fmt = Format.fprintf fmt "g %s@\n" l in
  let c_prover ~time fmt =
    List.iter
      (fun p ->
         Format.fprintf fmt "c %s %d %d@\n"
           (Runner.id p) time mem
      ) provers in
  let c_tactic ~goto fmt =
    List.iter
      (fun t ->
         Format.fprintf fmt "t %s %s@\n" t goto
      ) tactics in
  let c_list pps fmt = List.iter (fun pp -> pp fmt) pps in
  let c_strategy fmt ~name ~descr ~shortcut pps =
    Format.fprintf fmt
      "[strategy]@\n\
       code = \"%t\"@\n\
       name = \"%s\"@\n\
       desc = \"%s\"@\n\
       shortcut = \"%c\"@\n@."
      (c_list pps) name descr shortcut
  in
  begin
    Utils.mkdirs session ;
    Utils.dump ~file:hammer
      begin fun fmt ->
        c_strategy fmt ~name:"hammer" ~shortcut:'H'
          ~descr:"Hammer (why3find provers & tactics)" [
          c_label "root" ;
          c_prover ~time ;
          c_tactic ~goto:"root" ;
          if time > 1 then c_prover ~time:(time * 2) else c_skip ;
        ] ;
        c_strategy fmt ~name:"provers" ~shortcut:'P'
          ~descr:"Hammer Prove (why3find provers)" [
          c_prover ~time ;
        ] ;
        c_strategy fmt ~name:"tactics" ~shortcut:'U'
          ~descr:"Hammer Unfold (why3find tactics)" [
          c_label "tactic" ;
          c_tactic ~goto:"prover" ;
          c_goto "final" ;
          c_label "prover" ;
          c_prover ~time ;
          c_goto "tactic" ;
          c_label "final" ;
          if time > 1 then c_prover ~time:(2 * time) else c_skip ;
        ] ;
      end ;
    hammer
  end

(* -------------------------------------------------------------------------- *)
