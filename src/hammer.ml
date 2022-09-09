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

(* -------------------------------------------------------------------------- *)
(* --- Queue Management                                                   --- *)
(* -------------------------------------------------------------------------- *)

type node = {
  profile : Calibration.profile ;
  task : Why3.Task.task ;
  hint : crc ;
  result : crc Fibers.var ;
}

let q1 : node Queue.t = Queue.create ()
let q2 : node Queue.t = Queue.create ()

let schedule profile task hint =
  let result = Fibers.var () in
  Queue.push
    { profile ; task ; hint ; result }
    (if complete hint then q2 else q1) ;
  Fibers.get result

let pop () =
  try Some (Queue.pop q1) with Queue.Empty ->
  try Some (Queue.pop q2) with Queue.Empty ->
    None

(* -------------------------------------------------------------------------- *)
(* --- Hammer Strategy                                                    --- *)
(* -------------------------------------------------------------------------- *)

type henv = {
  env : Wenv.env ;
  time : float ;
  provers : Runner.prover list ;
  transfs : string list ;
}

let hammer _henv _task _hint = Fibers.return Stuck

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
    Fibers.await (hammer henv node.task node.hint) (Fibers.set node.result) ;
    run henv

(* -------------------------------------------------------------------------- *)
