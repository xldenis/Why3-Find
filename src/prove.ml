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

open Crc
open Fibers.Monad

let process ~env ~provers ~transfs file =
  begin
    Format.printf "Proving %s...@." file ;
    ignore env ;
    ignore Stuck ;
    ignore provers ;
    ignore transfs ;
    exit 2
  end

let prove ~pkgs ~provers ~transfs ~files =
  Fibers.run @@
  begin
    let env = Wenv.init ~pkgs in
    let provers = Runner.select env provers in
    let* results =
      Fibers.all @@ List.map (process ~env ~provers ~transfs) files
    in
    List.iter (fun (file,proved,total) ->
        Format.printf "[ %4d / %4d ] %s@." proved total file
      ) results ;
    Runner.report_stats () ;
    Fibers.return ()
  end

(* -------------------------------------------------------------------------- *)
