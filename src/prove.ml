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

let process ~env ~provers ~transfs file =
  begin
    Format.printf "Proving %s...@." file ;
    ignore env ;
    ignore Stuck ;
    ignore provers ;
    ignore transfs ;
    ignore Calibration.generate ;
  end

let prove ~pkgs ~provers ~transfs ~files =
  begin
    let env = Wenv.init ~pkgs in
    let provers = Runner.select env provers in
    List.iter (process ~env ~provers ~transfs) files ;
    exit 2 ;
  end

(* -------------------------------------------------------------------------- *)
