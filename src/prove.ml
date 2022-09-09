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
module S = Why3.Session_itp
module Th = Why3.Theory
open Fibers.Monad

type profile = Calibration.profile
type theories = Crc.crc M.t M.t
type session = (Th.theory * (string * Crc.crc) list) list

(* -------------------------------------------------------------------------- *)
(* --- Theories                                                           --- *)
(* -------------------------------------------------------------------------- *)

let load_theories (env : Wenv.env) file : Th.theory list =
  let byloc a b =
    match a.Th.th_name.id_loc , b.Th.th_name.id_loc with
    | None,None -> 0
    | Some _,None -> (-1)
    | None,Some _ -> (+1)
    | Some la, Some lb -> Why3.Loc.compare la lb
  in
  try
    fst Why3.Env.(read_file base_language env.wenv file) |>
    M.bindings |> List.map snd |> List.sort byloc
  with error ->
    Utils.flush () ;
    Format.printf "%s@." (Printexc.to_string error) ;
    exit 2

let thy_name t = t.Th.th_name.id_string
let goal_name t = Why3.Task.(task_goal t).pr_name.id_string

(* -------------------------------------------------------------------------- *)
(* --- Proof Files                                                        --- *)
(* -------------------------------------------------------------------------- *)

let jmap cc js =
  let m = ref M.empty in
  Json.jiter (fun fd js -> m := M.add fd (cc js) !m) js ; !m

let jsession (trs : session) : Json.t =
  `Assoc (List.map (fun (th,rs) ->
      thy_name th,
      `Assoc (List.map (fun (g,r) -> g, Crc.to_json r) rs)
    ) trs)

let load_proofs file : profile * theories =
  let js = if Sys.file_exists file then Json.of_file file else `Null in
  let profile = Calibration.of_json @@ Json.jfield "profile" js in
  let strategy = jmap (jmap Crc.of_json) @@ Json.jfield "theories" js in
  profile , strategy

let save_proofs file profile (trs : session) =
  Json.to_file file @@ `Assoc [
    "profile", Calibration.to_json profile ;
    "theories", jsession trs ;
  ]

(* -------------------------------------------------------------------------- *)
(* --- Single File Processing                                             --- *)
(* -------------------------------------------------------------------------- *)

let process ~env ~success file =
  begin
    let dir = Filename.chop_extension file in
    let path =
      String.concat "." @@ String.split_on_char '/' @@
      if Filename.is_relative file then dir else Filename.basename dir in
    let fp = Printf.sprintf "%s/proof.json" @@ dir in
    let theories = load_theories env file in
    let profile, strategy = load_proofs fp in
    Utils.mkdirs dir ;
    let driver =
      Fibers.all @@ List.map
        (fun theory ->
           let thy = thy_name theory in
           let tasks = Why3.Task.split_theory theory None None in
           let hints = M.find_def M.empty thy strategy in
           let+ proofs =
             Fibers.all @@ List.map
               (fun task ->
                  let goal = goal_name task in
                  let hint = M.find_def Crc.Stuck goal hints in
                  let+ crc = Hammer.schedule profile task hint in
                  if not (Crc.complete crc) then
                    begin
                      success := false ;
                      Format.printf "%s.%s.%s : %a@." path thy goal
                        Crc.pretty crc ;
                    end ;
                  goal, crc
               ) tasks
           in theory, proofs
        ) theories
    in
    Fibers.await driver (save_proofs fp profile)
end

(* -------------------------------------------------------------------------- *)
(* --- Main Prove Command                                                 --- *)
(* -------------------------------------------------------------------------- *)

let command ~time ~pkgs ~provers ~transfs ~files =
  begin
    let time = float time in
    let env = Wenv.init ~pkgs in
    let provers = Runner.select env provers in
    let transfs = [ "split_vc" ; "inline_goal" ] @ transfs in
    let success = ref true in
    List.iter (process ~env ~success) files ;
    Hammer.run { env ; time ; provers ; transfs } ;
    Runner.report_stats () ;
    if not !success then exit 1
  end


(* -------------------------------------------------------------------------- *)
