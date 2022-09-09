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

open Fibers.Monad
module M = Why3.Wstdlib.Mstr

type strategy = Crc.crc M.t M.t

(* -------------------------------------------------------------------------- *)
(* --- Proof File                                                         --- *)
(* -------------------------------------------------------------------------- *)

[@@@ warning "-32"]

let to_jmap cc js =
  let m = ref M.empty in
  Json.jiter (fun fd js -> m := M.add fd (cc js) !m) js ; !m

let of_jmap (cc : 'a -> Json.t) (m : 'a M.t) : Json.t =
  `Assoc (M.fold_left (fun js fd v -> (fd, cc v)::js) [] m)

[@@@ warning "+32"]

let load_proof fp : Calibration.profile * strategy =
  let js = if Sys.file_exists fp then Json.of_file fp else `Null in
  let profile = Calibration.of_json @@ Json.jfield "profile" js in
  let strategy = to_jmap (to_jmap Crc.of_json) @@ Json.jfield "theories" js in
  profile , strategy

(* -------------------------------------------------------------------------- *)
(* --- Single File Processing                                             --- *)
(* -------------------------------------------------------------------------- *)

let process ~env ~provers ~transfs file =
  begin
    Utils.progress "loading %s" file ;
    let wenv = env.Wenv.env in
    let theories,_ =
      try Why3.Env.(read_file base_language wenv file)
      with error ->
        Utils.flush () ;
        Format.printf "%s@." (Printexc.to_string error) ;
        exit 2
    in
    let profile, strategy = load_proof file in
    let proved = ref 0 in
    let total = ref 0 in
    M.iter
      begin fun tname thy ->
        let tasks = Why3.Task.split_theory thy None None in
        let proofs = M.find_def M.empty tname strategy in
        ignore tasks ;
        ignore proofs ;
        ()
      end theories ;
    ignore env ;
    ignore profile ;
    ignore provers ;
    ignore transfs ;
    ignore Hammer.schedule ;
    Fibers.return (file,!proved,!total)
  end

(* -------------------------------------------------------------------------- *)
(* --- Main Prove Command                                                 --- *)
(* -------------------------------------------------------------------------- *)

let prove ~pkgs ~provers ~transfs ~files =
  Fibers.run @@
  begin
    let env = Wenv.init ~pkgs in
    let provers = Runner.select env provers in
    let* results =
      Fibers.all @@ List.map (process ~env ~provers ~transfs) files
    in
    Utils.flush () ;
    List.iter (fun (file,proved,total) ->
        Format.printf "%d/%d @{<%s>%s@}@." proved total
          (if total < proved then "red" else "green")
          (Filename.basename file)
      ) results ;
    Runner.report_stats () ;
    Fibers.return ()
  end

(* -------------------------------------------------------------------------- *)
