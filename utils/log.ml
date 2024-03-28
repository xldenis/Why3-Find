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
let warnings = ref 0
let errors = ref 0
let summary = Queue.create ()

type level = [ `Message | `Warning | `Error ]

let display : level -> _ format6 = function
  | `Message -> ""
  | `Warning -> "@{<bold>@{<bright magenta>Warning:@}@} "
  | `Error -> "@{<bold>@{<bright red>Error:@}@} "

let publish (lvl: level) print =
  Utils.flush () ;
  let stderr =
    match lvl with
    | `Message -> incr messages ; false
    | `Warning -> incr warnings ; true
    | `Error -> incr errors ; true
  in
  if stderr then
    ( Queue.push print summary ; Format.eprintf "%t@." print )
  else
    Format.printf "%t@." print

let nwl n = messages := n + !messages

let emit : type a. ?level:level -> (a, Format.formatter, unit) format -> a =
  fun ?(level=`Message) format ->
  Format.kdprintf (publish level) (display level ^^ format)

let warning format =
  emit ~level:`Warning format

let error format =
  emit ~level:`Error format

let summary () =
  let wrn = !warnings in
  if wrn > 0 then
    begin
      Utils.flush ();
      let dump =
        not Utils.tty ||
        !messages > Option.value ~default:25 @@ Terminal_size.get_rows () in
      if dump then
        begin
          Format.printf "Summary:@.";
          Queue.iter (Format.printf "%t@.") summary ;
        end ;
      Format.printf "Emitted %d warning%a" wrn Utils.pp_s wrn ;
      let err = !errors in
      if err > 0 then
        Format.printf ", %d error%a" err Utils.pp_s err ;
      Format.printf "@."
    end

let exit_summary () =
  try summary () with _ -> ()

let () =
  at_exit exit_summary
