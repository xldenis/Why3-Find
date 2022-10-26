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
  let default = Calibration.(if !Hammer.local then empty () else default ()) in
  let profile = Calibration.of_json ~default @@ Json.jfield "profile" js in
  let strategy = jmap (jmap Crc.of_json) @@ Json.jfield "proofs" js in
  profile , strategy

let save_proofs ~mode dir file profile (prfs : proofs) =
  match mode with
  | `Replay -> ()
  | `Minimize | `Update | `Force ->
    Utils.mkdirs dir ;
    Json.to_file file @@ `Assoc [
      "profile", Calibration.to_json profile ;
      "proofs", jproofs prfs ;
    ]

(* -------------------------------------------------------------------------- *)
(* --- Proof Driver                                                       --- *)
(* -------------------------------------------------------------------------- *)

let prove_theory mode profile strategy theory =
  let thy = Session.name theory in
  let tasks = Session.split theory in
  let hints = M.find_def M.empty thy strategy in
  let+ proofs =
    Fibers.all @@ List.map
      (fun task ->
         let goal = Session.goal_name task in
         let hint = M.find_def Crc.Stuck goal hints in
         let+ crc =
           match mode with
           | `Force ->
             Hammer.schedule profile task Stuck
           | `Replay ->
             Hammer.schedule profile task hint
           | `Update | `Minimize ->
             Crc.merge hint @+ Hammer.schedule profile task hint
         in
         if Utils.tty then Crc.stats hint crc ;
         if not (Crc.complete crc) then
           begin
             Utils.flush () ;
             begin
               match Session.goal_loc task with
               | Some loc ->
                 Format.printf "%a: proof failed@."
                   Why3.Loc.gen_report_position loc
               | None -> ()
             end ;
             Format.printf "Goal @{<red>%s@}: %a@."
               goal Crc.pretty crc
           end ;
         goal, crc
      ) tasks
  in
  theory, proofs

(* -------------------------------------------------------------------------- *)
(* --- Axioms Logger                                                      --- *)
(* -------------------------------------------------------------------------- *)

let stdlib = ref false
let axioms = ref false
let externals = ref false
let builtins = ref false

let report_axioms henv theories =
  Axioms.iter henv
    (fun _id prm ->
       begin
         try
           let id = Axioms.ident prm.kind in
           let ident = Id.resolve ~lib:[] id in
           if ident.id_pkg = `Stdlib && not !stdlib then raise Not_found ;
           let axiom = !axioms && Axioms.is_external prm in
           let builtin = !builtins && prm.builtin <> [] in
           let extern = !externals && prm.extern <> None in
           if axiom || builtin || extern then
             let kind = match prm.kind with
               | Type _ -> "type"
               | Logic _ -> "logic"
               | Value _ -> "value"
               | Axiom _ -> "axiom" in
             let categories = List.filter (fun c -> c <> "") [
                 if axiom then "axiom" else "" ;
                 if builtin then "builtin" else "" ;
                 if extern then "extern" else "" ;
               ] in
             Format.printf "  Assumed %s %a (%s)@\n" kind Id.pp_title ident
               (String.concat ", " categories) ;
         with Not_found -> ()
       end
    ) theories

(* -------------------------------------------------------------------------- *)
(* --- Proof Logger                                                       --- *)
(* -------------------------------------------------------------------------- *)

let report_results log path henv proofs =
  begin
    let stuck = ref 0 in
    let proved = ref 0 in
    let failed = ref false in
    List.iter
      (fun (_,goals) ->
         List.iter
           (fun (_,crc) ->
              stuck := !stuck + Crc.stuck crc ;
              proved := !proved + Crc.proved crc ;
              if not @@ Crc.complete crc then failed := true ;
           ) goals
      ) proofs ;
    begin
      match log with
      | `Modules ->
        Format.printf "Module %s: %t@."
          path (Crc.pp_result ~stuck:!stuck ~proved:!proved) ;
        Option.iter (fun h ->
            report_axioms h @@
            List.map (fun (th,_) -> Session.theory th) proofs
          ) henv ;
      | `Theories | `Goals | `Proofs ->
        List.iter
          (fun (th,goals) ->
             let thy = Session.theory th in
             let tn = Session.name th in
             let (s,p) = List.fold_left
                 (fun (s,p) (_,c) -> s + Crc.stuck c, p + Crc.proved c)
                 (0,0) goals in
             Format.printf "Theory %s.%s: %t@." path tn
               (Crc.pp_result ~stuck:s ~proved:p) ;
             begin
               match log with
               | `Modules | `Theories -> ()
               | `Goals ->
                 List.iter
                   (fun (g,c) ->
                    Format.printf "  Goal %s: %a@." g Crc.pretty c
                   ) goals
               | `Proofs ->
                 List.iter
                   (fun (g,c) ->
                      Format.printf "  @[<hv 2>Goal %s %a@ %a@]@." g
                        Utils.pp_mark (Crc.complete c) Crc.dump c
                   ) goals
             end ;
             Option.iter (fun h -> report_axioms h [thy]) henv ;
          ) proofs
    end ;
    !failed
  end

(* -------------------------------------------------------------------------- *)
(* --- Single File Processing                                             --- *)
(* -------------------------------------------------------------------------- *)

type mode = [ `Force | `Update | `Minimize | `Replay ]
type log0 = [ `Modules | `Theories | `Goals | `Proofs ]
type log = [ `Default | log0 ]

let process ~env ~mode ~session ~(log : log0) ~unsuccess file =
  begin
    if not @@ String.ends_with ~suffix:".mlw" file then
      begin
        Format.eprintf "Invalid file name: %S@." file ;
        exit 2
      end ;
    let dir = Filename.chop_extension file in
    let path =
      String.concat "." @@ String.split_on_char '/' @@
      if Filename.is_relative file then dir else Filename.basename dir in
    let fp = Filename.concat dir "proof.json" in
    let theories, format = load_theories env file in
    let session = Session.create ~session ~dir ~file ~format theories in
    let profile, strategy = load_proofs fp in
    let results =
      Fibers.all @@ List.map
        (prove_theory mode profile strategy)
        (Session.theories session)
    in
    Fibers.await results
      begin fun proofs ->
        Utils.flush () ;
        Session.save session ;
        save_proofs ~mode dir fp profile proofs ;
        let henv =
          if !axioms || !externals || !builtins then
            Some (Axioms.init env)
          else None in
        let failed = report_results log path henv proofs in
        if failed then unsuccess := file :: !unsuccess ;
      end
  end

(* -------------------------------------------------------------------------- *)
(* --- Prove Command                                                      --- *)
(* -------------------------------------------------------------------------- *)

let prove_files ~mode ~session ~log ~files =
  begin
    let env = Wenv.init () in
    let time = Wenv.time () in
    let provers = Wenv.provers () in
    let transfs = Wenv.transfs () in
    let provers = Runner.select env provers in
    let unsuccess = ref [] in
    let minimize = (mode = `Minimize) in
    let log : log0 = match log with
      | `Default -> if List.length files > 1 then `Modules else `Theories
      | #log0 as l -> l in
    List.iter (process ~env ~mode ~session ~log ~unsuccess) files ;
    Hammer.run { env ; time ; provers ; transfs ; minimize } ;
    if Utils.tty then
      begin
        Runner.print_stats () ;
        Crc.print_stats () ;
      end ;
    List.rev !unsuccess ;
  end

(* -------------------------------------------------------------------------- *)
