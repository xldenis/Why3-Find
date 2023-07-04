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
    Utils.log "%s@." (Printexc.to_string error) ;
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
  let default = Calibration.default () in
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
             Hammer.schedule profile ~replay:false ~depth:0 task Stuck
           | `Replay ->
             Hammer.schedule profile ~replay:true ~depth:0 task hint
           | `Update | `Minimize ->
             Crc.merge hint @+
             Hammer.schedule ~replay:false ~depth:0 profile task hint
         in
         if Utils.tty then Crc.stats hint crc ;
         if not (Crc.complete crc) then
           begin
             Utils.flush () ;
             begin
               match Session.goal_loc task with
               | Some loc ->
                 Format.printf "%a: proof failed@." Why3.Loc.pp_position loc
               | None -> ()
             end ;
             Format.printf "Goal @{<red>%s@}: %a@." goal Crc.pretty crc
           end ;
         goal, crc
      ) tasks
  in
  theory, proofs

(* -------------------------------------------------------------------------- *)
(* --- Axioms Logger                                                      --- *)
(* -------------------------------------------------------------------------- *)

let stdlib = ref false
let externals = ref false
let builtins = ref false

let standardized = "Standard  ",ref 0
let dependencies = "Assumed   ",ref 0
let hypotheses   = "Hypothesis",ref 0
let parameters   = "Parameter ",ref 0
let procedures   = "Procedure ",ref 0
let externalized = "External  ",ref 0

let print_axioms_stats () =
  let p = !(snd parameters) in
  let h = !(snd hypotheses) in
  let r = !(snd procedures) in
  let e = !(snd externalized) in
  let d = !(snd dependencies) in
  let s = !(snd standardized) in
  if p+h+r+e+d+s = 0 then
    Format.printf "Hypotheses: none@."
  else
    begin
      Format.printf "Hypotheses:@\n" ;
      if p > 0 then
        Format.printf " - parameter%a: @{<green>%d@}@\n" Utils.pp_s p p ;
      if h > 0 then
        Format.printf " - hypothes%a: @{<orange>%d@}@\n" Utils.pp_yies h h ;
      if r > 0 then
        Format.printf " - procedure%a: @{<orange>%d@}@\n" Utils.pp_s r r ;
      if e > 0 then
        Format.printf " - externalized: @{<orange>%d@}@\n" e ;
      if s > 0 then
        Format.printf " - standardized: @{<orange>%d@}@\n" s ;
      if d > 0 then
        Format.printf " - dependenc%a: @{<red>%d@}@\n" Utils.pp_yies d d ;
      Format.print_flush ()
    end

let report_parameter ~lib ~signature (prm : Axioms.parameter) =
  try
    let id = Axioms.ident prm.param in
    let ident = Id.resolve ~lib id in
    let std = ident.id_pkg = `Stdlib in
    let builtin = prm.builtin <> [] in
    let extern = prm.extern <> None in
    if (!stdlib || not std) &&
       ((!builtins && builtin) || (!externals || not extern))
    then
      begin
        let kind, param =
          match prm.param with
          | Type _ -> "type ", parameters
          | Logic _ -> "logic", parameters
          | Param _ -> "param", parameters
          | Value _ -> "value", procedures
          | Axiom _ -> "axiom", hypotheses
        in
        let action, count =
          if builtin || extern then externalized else
          if signature then param else
          if std then standardized else dependencies
        in incr count ;
        Format.printf "  %s %s %a" action kind Id.pp_title ident ;
        let categories = List.filter (fun c -> c <> "") [
            if std then "stdlib" else "" ;
            if builtin then "builtin" else "" ;
            if extern then "extern" else "" ;
          ] in
        if categories <> [] then
          Format.printf " (%s)" (String.concat ", " categories) ;
        Format.printf "@\n" ;
      end
  with Not_found -> ()

let report_signature henv ~lib (th : Th.theory) =
  match henv with
  | None -> ()
  | Some henv ->
    List.iter
      (report_parameter ~lib ~signature:true)
      (Axioms.parameters @@ Axioms.signature henv th)

let report_hypotheses henv ~lib (ths : Th.theory list) =
  match henv with
  | None -> ()
  | Some henv ->
    let once = ref true in
    Axioms.iter henv
      (fun prm ->
         if !once then (Format.printf "Dependencies:@\n" ; once := false) ;
         report_parameter ~lib ~signature:false prm)
      ths

(* -------------------------------------------------------------------------- *)
(* --- Proof Logger                                                       --- *)
(* -------------------------------------------------------------------------- *)

let report_results log henv ~lib proofs =
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
    let ths = List.map (fun (th,_) -> Session.theory th) proofs in
    begin
      let path = String.concat "." lib in
      match log with
      | `Modules ->
        Format.printf "Library %s: %t@."
          path (Crc.pp_result ~stuck:!stuck ~proved:!proved) ;
        List.iter (report_signature henv ~lib) ths
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
             report_signature henv ~lib thy
          ) proofs
    end ;
    report_hypotheses henv ~lib ths ;
    !failed
  end

(* -------------------------------------------------------------------------- *)
(* --- Single File Processing                                             --- *)
(* -------------------------------------------------------------------------- *)

type mode = [ `Force | `Update | `Minimize | `Replay ]
type log0 = [ `Modules | `Theories | `Goals | `Proofs ]
type log = [ `Default | log0 ]

let process ~env ~mode ~session ~(log : log0) ~axioms ~unsuccess file =
  Fibers.background @@
  begin
    if not @@ String.ends_with ~suffix:".mlw" file then
      begin
        Format.eprintf "Error: nvalid file name: %S@." file ;
        exit 2
      end ;
    let dir = Filename.chop_extension file in
    let lib = String.split_on_char '/' @@
      if Filename.is_relative file then dir else Filename.basename dir in
    let fp = Filename.concat dir "proof.json" in
    let theories, format = load_theories env file in
    let session = Session.create ~session ~dir ~file ~format theories in
    let profile, strategy = load_proofs fp in
    let* proofs =
      Fibers.all @@ List.map
        (prove_theory mode profile strategy)
        (Session.theories session)
    in
    Session.save session ;
    save_proofs ~mode dir fp profile proofs ;
    let henv =
      if axioms then
        Some (Axioms.init env)
      else None in
    Utils.flush () ;
    let failed = report_results log henv ~lib proofs in
    if failed then unsuccess := file :: !unsuccess ;
    Fibers.return ()
  end

(* -------------------------------------------------------------------------- *)
(* --- Prove Command                                                      --- *)
(* -------------------------------------------------------------------------- *)

let prove_files ~mode ~session ~log ~axioms ~files =
  begin
    let env = Wenv.init () in
    let time = Wenv.time () in
    let maxdepth = Wenv.depth () in
    let patterns = Wenv.provers () in
    let tactics = Wenv.tactics () in
    let provers = Runner.select env ~patterns in
    let unsuccess = ref [] in
    let minimize = (mode = `Minimize) in
    let log : log0 = match log with
      | `Default -> if List.length files > 1 then `Modules else `Theories
      | #log0 as l -> l in
    List.iter (process ~env ~mode ~session ~log ~axioms ~unsuccess) files ;
    Hammer.run {
      env ;
      client = Client.connect env ;
      time ; maxdepth ; provers ; tactics ; minimize ;
    } ;
    if Utils.tty then
      begin
        Utils.flush () ;
        Runner.print_stats () ;
        Crc.print_stats () ;
        if axioms then print_axioms_stats () ;
      end ;
    List.rev !unsuccess ;
  end

(* -------------------------------------------------------------------------- *)
