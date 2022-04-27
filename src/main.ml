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
(* --- Why-3 find main entry point                                        --- *)
(* -------------------------------------------------------------------------- *)

let current = ref 2

let version () : unit =
  begin
    Format.printf "why3find v%s@." Version.version ;
    exit 0
  end

let help () : unit =
  begin
    Format.printf "why3find [-h|--help]@\n" ;
    Format.printf "why3find [-v|--version]@\n" ;
    Format.printf "why3find COMMAND [ARGS...]@\n" ;
    exit 0
  end

let main () =
  try
    match Sys.argv.(1) with
    | "-v" | "-version" | "--version" -> version ()
    | "-h" | "-help" | "--help" | "help" -> help ()
    | cmd -> failwith (Printf.sprintf "unknown command '%s'" cmd)
  with Failure msg ->
    Format.eprintf "why3find: %s@." msg ; exit 1

let () = Printexc.catch main ()

(* -------------------------------------------------------------------------- *)
