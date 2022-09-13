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
open Fibers.Monad

type profile = Calibration.profile
type theories = Crc.crc M.t M.t
type proofs = (Session.theory * (string * Crc.crc) list) list

(* -------------------------------------------------------------------------- *)
(* --- Theories                                                           --- *)
(* -------------------------------------------------------------------------- *)

let load_theories (env : Wenv.env) file =
  let byloc a b =
    match a.Th.th_name.id_loc , b.Th.th_name.id_loc with
    | None,None -> 0
    | Some _,None -> (-1)
    | None,Some _ -> (+1)
    | Some la, Some lb -> Why3.Loc.compare la lb
  in
  try
    let tmap,format = Why3.Env.(read_file base_language env.wenv file) in
    M.bindings tmap |> List.map snd |> List.sort byloc , format
  with error ->
    Utils.flush () ;
    Format.printf "%s@." (Printexc.to_string error) ;
    exit 2

(* -------------------------------------------------------------------------- *)
(* --- Proof Files                                                        --- *)
(* -------------------------------------------------------------------------- *)

let jmap cc js =
  let m = ref M.empty in
  Json.jiter (fun fd js -> m := M.add fd (cc js) !m) js ; !m

let jproofs (prfs : proofs) : Json.t =
  `Assoc (List.map (fun (th,rs) ->
      Session.name th,
      `Assoc (List.map (fun (g,r) -> g, Crc.to_json r) rs)
    ) prfs)

let load_proofs file : profile * theories =
  let js = if Sys.file_exists file then Json.of_file file else `Null in
  let profile = Calibration.of_json @@ Json.jfield "profile" js in
  let strategy = jmap (jmap Crc.of_json) @@ Json.jfield "proofs" js in
  profile , strategy

let save_proofs ~mode dir file profile (prfs : proofs) =
  match mode with
  | `Replay -> ()
  | `Update | `All ->
    Utils.mkdirs dir ;
    Json.to_file file @@ `Assoc [
      "profile", Calibration.to_json profile ;
      "proofs", jproofs prfs ;
    ]

(* -------------------------------------------------------------------------- *)
(* --- Single File Processing                                             --- *)
(* -------------------------------------------------------------------------- *)

type mode = [ `Update | `All | `Replay ]

let process ~env ~mode ~session ~verbose ~success file =
  begin
    let dir = Filename.chop_extension file in
    let path =
      String.concat "." @@ String.split_on_char '/' @@
      if Filename.is_relative file then dir else Filename.basename dir in
    let fp = Printf.sprintf "%s/proof.json" @@ dir in
    let theories, format = load_theories env file in
    let session = Session.create ~session ~dir ~file ~format theories in
    let profile, strategy = load_proofs fp in
    let total = ref 0 in
    let proved = ref 0 in
    let driver =
      Fibers.all @@ List.map
        (fun theory ->
           let thy = Session.name theory in
           let tasks = Session.split theory in
           let hints = M.find_def M.empty thy strategy in
           let+ proofs =
             Fibers.all @@ List.map
               (fun task ->
                  let goal = Session.goal_name task in
                  let hint = M.find_def Crc.Stuck goal hints in
                  incr total ;
                  let+ crc =
                    match mode with
                    | `All ->
                      Hammer.schedule profile task Stuck
                    | `Replay ->
                      Hammer.schedule profile task hint
                    | `Update ->
                      Crc.merge hint @+ Hammer.schedule profile task hint
                  in
                  if Crc.complete crc then
                    incr proved
                  else
                    begin
                      success := false ;
                      Format.printf "> %s.%s.%s : %a@." path thy goal
                        Crc.pretty crc ;
                    end ;
                  goal, crc
               ) tasks
           in
           theory, proofs
        ) (Session.theories session)
    in
    Fibers.await driver
      begin fun proofs ->
        Session.save session ;
        save_proofs ~mode dir fp profile proofs ;
        Utils.flush () ;
        if verbose then
          List.iter
            (fun (th,goals) ->
               let tn = Session.name th in
               if goals = [] then
                 Format.printf "Theory %s.%s: no goal@." path tn
               else
                 begin
                   Format.printf "Theory %s.%s:@." path tn ;
                   List.iter
                     (fun (g,p) ->
                        Format.printf "  Goal %s: %a@."
                          g Crc.pretty p
                     ) goals
                 end
            ) proofs
        else
          let np = !proved in
          let nt = !total in
          let style =
            if np = nt then "green" else
            if np = 0 then "red" else "orange"
          in Format.printf "%d/%d @{<%s>%s@}@." np nt style path
      end
end

(* -------------------------------------------------------------------------- *)
(* --- Prove Command                                                      --- *)
(* -------------------------------------------------------------------------- *)

let command ~time ~mode ~session ~verbose ~pkgs ~provers ~transfs ~files =
  begin
    let time = float time in
    let env = Wenv.init ~pkgs in
    let provers = Runner.select env provers in
    let transfs = [ "split_vc" ; "inline_goal" ] @ transfs in
    let success = ref true in
    List.iter (process ~env ~mode ~session ~verbose ~success) files ;
    Hammer.run { env ; time ; provers ; transfs } ;
    Runner.report_stats () ;
    if not !success then exit 1
  end

(* -------------------------------------------------------------------------- *)
