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

let version () : unit =
  begin
    Format.printf "why3find v%s@." Version.version ;
    exit 0
  end

let help () : unit =
  begin
    Format.printf "why3find COMMAND [OPTIONS...] [ARGS...]@\n" ;
    exit 0
  end

let main () =
  let p = ref 1 in
  while !p < Array.length Sys.argv do
    match Sys.argv.(!p) with
    | "-v" | "-version" | "--version" -> version ()
    | "-h" | "-help" | "--help" | "help" -> help ()
    | a ->
        Format.eprintf "[why3find] Error: unknown command %S" a ;
        exit 1
  done

let () = Printexc.catch main ()

(* -------------------------------------------------------------------------- *)
