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
(* --- Logging Facilities                                                 --- *)
(* -------------------------------------------------------------------------- *)

let messages = ref 0
let errors = ref 0
let warnings = ref []

type level = [ `Message | `Warning | `Error ]

let display : level -> _ format6 = function
  | `Message -> ""
  | `Warning -> "@{<bold>@{<bright magenta>Warning:@}@} "
  | `Error -> "@{<bold>@{<bright red>Error:@}@} "

let add_summary (lvl: level) print =
  incr messages;
  match lvl with
  | `Message -> ()
  | `Warning -> warnings := print :: !warnings
  | `Error -> incr errors

let log : type a. ?level:level -> (a, Format.formatter, unit) format -> a =
  fun ?(level=`Message) format ->
  let cont print =
    Utils.flush ();
    Format.eprintf "%t@." print;
    add_summary level print in
  Format.kdprintf cont ("@[<hov>" ^^ display level ^^ format ^^ "@]")

let warning format =
  log ~level:`Warning format

let error format =
  log ~level:`Error format

let summary () =
  if !warnings <> [] then
    begin
      Utils.flush ();
      Format.eprintf "Warning summary:@.";
      List.iter (Format.eprintf "%t@.") (List.rev !warnings);
      let len = List.length !warnings in
      Format.eprintf "Emitted %d warning%a" len Utils.pp_s len;
      if !errors <> 0 then
        Format.eprintf ", %d error%a" !errors Utils.pp_s !errors;
      Format.eprintf "@."
    end

let exit_summary () =
  try summary () with _ -> ()

let () =
  at_exit exit_summary
