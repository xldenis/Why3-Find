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
(* --- Session Management                                                 --- *)
(* -------------------------------------------------------------------------- *)

module Th = Why3.Theory
module T = Why3.Task
module S = Why3.Session_itp
module Mid = Why3.Ident.Mid

type session =
  | Ths of Th.theory list
  | Sfile of S.file * S.session

let create ~session ~dir ~file ~format ths =
  if session then
    let s = S.empty_session dir in
    let f = Why3.Sysutil.relativize_filename dir file in
    let f = S.add_file_section s f ~file_is_detached:false ths format in
    Sfile (f,s)
  else
    Ths ths

let save = function
  | Ths _ -> ()
  | Sfile(_,s) -> S.save_session s

type theory =
  | Thy of Th.theory
  | Sth of S.session * S.theory

let theory = function
  | Thy th -> th
  | Sth(_,th) -> Th.restore_theory (S.theory_name th)

let theories = function
  | Ths ths -> List.map (fun t -> Thy t) ths
  | Sfile(f,s) -> List.map (fun t -> Sth(s,t)) (S.file_theories f)

let thy_name th = th.Th.th_name.id_string

let name = function
  | Thy th -> thy_name th
  | Sth(_,th) -> (S.theory_name th).id_string

type goal =
  | Task of T.task
  | Snode of S.session * S.proofNodeID * T.task

let tasks = List.map (fun t -> Task t)
let nodes s = List.map (fun n -> Snode(s,n,S.get_task s n))

let split = function
  | Thy th -> tasks @@ T.split_theory th None None
  | Sth(s,th) -> nodes s @@ S.theory_goals th

let proof_name =
  let names = ref Mid.empty in
  fun id ->
    try Mid.find id !names with Not_found ->
      let _,_,qid = Id.path id in
      let path = Id.cat qid in
      let name =
        if String.ends_with ~suffix:"'vc" path
        then String.sub path 0 (String.length path - 3) else path
      in names := Mid.add id name !names ; name

let task_name t = proof_name (T.task_goal t).pr_name
let task_expl t =
  let (_, expl, _) = Why3.Termcode.goal_expl_task ~root:false t in
  expl
let goal_task = function Task t | Snode(_,_,t) -> t
let goal_loc g = (T.task_goal (goal_task g)).pr_name.id_loc
let goal_name g = task_name @@ goal_task g
let goal_expl g = task_expl @@ goal_task g

let pp_goal fmt g =
  Format.pp_print_string fmt (goal_name g) ;
  let expl = goal_expl g in
  if expl <> "" then Format.fprintf fmt " [%s]" expl

let silent : S.notifier = fun _ -> ()

let result goal prv limit result =
  match goal with
  | Task _ -> ()
  | Snode(s,n,_) ->
    let _ = S.graft_proof_attempt s n prv ~limit in
    S.update_proof_attempt silent s n prv result

let apply env tactic = function
  | Task t ->
    begin
      try
        match Why3.Trans.apply_transform tactic env t with
        | [ t' ] when Why3.Task.task_equal t t' -> None
        | ts -> Some (tasks ts)
      with _ -> None
    end
  | Snode(s,n,_) ->
    begin
      try
        let allow_no_effect = false in
        let ts = S.apply_trans_to_goal s env ~allow_no_effect tactic [] n in
        let tid = S.graft_transf s n tactic [] ts in
        let ns = S.get_sub_tasks s tid in
        Some (nodes s ns)
      with _ -> None
    end

(* -------------------------------------------------------------------------- *)
