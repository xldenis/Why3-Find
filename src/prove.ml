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

type strategy = (string * string,Crc.crc) Hashtbl.t

(* -------------------------------------------------------------------------- *)
(* --- Proof File                                                         --- *)
(* -------------------------------------------------------------------------- *)

let load_proof file : Calibration.profile * strategy =
  let fp = Printf.sprintf "%s/proof.json" (Filename.chop_extension file) in
  let js = if Sys.file_exists fp then Json.of_file fp else `Null in
  let profile = Calibration.of_json (Json.jfield "profile" js) in
  let strategy = Hashtbl.create 0 in
  begin
    Json.jfield "theories" js |>
    Json.jiter (fun thy js ->
        Json.jiter (fun goal js ->
            Hashtbl.add strategy (thy,goal) (Crc.of_json js)
          ) js
      ) ;
  end ;
  profile , strategy

(* -------------------------------------------------------------------------- *)
(* --- Single File Processing                                             --- *)
(* -------------------------------------------------------------------------- *)

let process ~env ~provers ~transfs file =
  begin
    Utils.progress "loading %s" file ;
    let wenv = env.Wenv.env in
    let theories =
      try Why3.Env.(read_file base_language wenv file)
      with error ->
        Utils.flush () ;
        Format.printf "%s@." (Printexc.to_string error) ;
        exit 2
    in
    let profile, strategy = load_proof file in
    let proved = ref 0 in
    let total = ref 0 in
    ignore env ;
    ignore theories ;
    ignore profile ;
    ignore strategy ;
    ignore provers ;
    ignore transfs ;
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
        Format.printf "%d/%d %s@." proved total (Filename.basename file)
      ) results ;
    Runner.report_stats () ;
    Fibers.return ()
  end

(* -------------------------------------------------------------------------- *)
