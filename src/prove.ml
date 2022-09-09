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
(* --- Proof Manager                                                      --- *)
(* -------------------------------------------------------------------------- *)

module M = Why3.Wstdlib.Mstr
module Th = Why3.Theory

type strategy = Crc.crc M.t M.t

(* -------------------------------------------------------------------------- *)
(* --- Proof File                                                         --- *)
(* -------------------------------------------------------------------------- *)

[@@@ warning "-32"]

let of_json cc js =
  let m = ref M.empty in
  Json.jiter (fun fd js -> m := M.add fd (cc js) !m) js ; !m

let to_json (cc : 'a -> Json.t) (m : 'a M.t) : Json.t =
  `Assoc (M.fold_left (fun js fd v -> (fd, cc v)::js) [] m)

[@@@ warning "+32"]

let load_proof file : string * Calibration.profile * strategy =
  let fp = Printf.sprintf "%s/proof.json" @@ Filename.basename file in
  let js = if Sys.file_exists fp then Json.of_file fp else `Null in
  let profile = Calibration.of_json @@ Json.jfield "profile" js in
  let strategy = of_json (of_json Crc.of_json) @@ Json.jfield "theories" js in
  fp, profile , strategy

let load_theories (env : Wenv.env) file : Th.theory list =
  let byloc a b =
    match a.Th.th_name.id_loc , b.Th.th_name.id_loc with
    | None,None -> 0
    | Some _,None -> (-1)
    | None,Some _ -> (+1)
    | Some la, Some lb -> Why3.Loc.compare la lb
  in
  try
    fst Why3.Env.(read_file base_language env.env file) |>
    M.bindings |> List.map snd |> List.sort byloc
  with error ->
    Utils.flush () ;
    Format.printf "%s@." (Printexc.to_string error) ;
    exit 2

(* -------------------------------------------------------------------------- *)
(* --- Single File Processing                                             --- *)
(* -------------------------------------------------------------------------- *)

let thy_name t = t.Th.th_name.id_string
let goal_name t = Why3.Task.(task_goal t).pr_name.id_string

let process ~env ~provers ~transfs file =
  begin
    let theories = load_theories env file in
    let _fp, profile, strategy = load_proof file in
    List.iter
      (fun theory ->
         let thy = thy_name theory in
         Format.printf "Theory %s@." thy ;
         let tasks = Why3.Task.split_theory theory None None in
         let proofs = M.find_def M.empty thy strategy in
         List.iter
           (fun task ->
              let goal = goal_name task in
              let crc = M.find_def Crc.Stuck goal proofs in
              Format.printf " + Goal %s : %a@." goal Crc.pretty crc ;
           ) tasks ;
         ignore proofs ;
      ) theories ;
    ignore env ;
    ignore profile ;
    ignore provers ;
    ignore transfs ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Main Prove Command                                                 --- *)
(* -------------------------------------------------------------------------- *)

let prove ~pkgs ~provers ~transfs ~files =
  begin
    let env = Wenv.init ~pkgs in
    let provers = Runner.select env provers in
    List.iter (process ~env ~provers ~transfs) files ;
    Fibers.flush () ;
    Runner.report_stats () ;
  end


(* -------------------------------------------------------------------------- *)
