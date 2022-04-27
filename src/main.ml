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
    Format.printf "why3find [-h|--help]@\n" ;
    Format.printf "why3find [-v|--version]@\n" ;
    Command.iter
      (fun cmd ->
         Format.printf "why3find %s [ARGS...]@\n" cmd
      ) ;
    Format.printf "why3find COMMAND [ARGS...]@\n" ;
    exit 0
  end

let main () =
  try
    let n = Array.length Sys.argv in
    if n < 2 then help () else
      match Sys.argv.(1) with
      | "-v" | "-version" | "--version" -> version ()
      | "-h" | "-help" | "--help" | "help" -> help ()
      | cmd -> Command.exec cmd (Array.sub Sys.argv 2 (n-2))
  with
  | Failure msg ->
      Format.eprintf "why3find: %s@." msg ;
      exit 1
  | Unix.Unix_error(err,_,arg) ->
      Format.eprintf "why3find: %s (%s)@."
        (Unix.error_message err) arg ;
      exit 1
  | exn ->
      Format.eprintf "why3find: fatal error (%s)@."
        (Printexc.to_string exn) ;
      exit 1

let () = Printexc.catch main ()

(* -------------------------------------------------------------------------- *)
