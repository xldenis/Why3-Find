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

let warnings = ref 0
let errors = ref 0
let summary = Queue.create ()

type level = [ `Message | `Warning | `Error ]

let pp_error pp = pp Format.err_formatter
let summarize pp = Queue.push pp summary ; pp_error pp

let output level pp =
  match level with
  | `Message -> Format.printf "@[<hov>%t@]@." pp
  | `Warning ->
    incr warnings ;
    Format.kdprintf summarize
      "@[<hov>@{<bold>@{<bright magenta>Warning:@}@} %t@]@." pp
  | `Error ->
    incr errors ;
    Format.kdprintf summarize
      "@[<hov>@{<bold>@{<bright red>Error:@}@} %t@]@." pp

let emit ?(level=`Message) msg = Format.kdprintf (output level) msg

let warning format =
  emit ~level:`Warning format

let error format =
  emit ~level:`Error format

let summary () =
  let wrn = !warnings in
  if wrn > 0 then
    begin
      Utils.flush ();
      if not Utils.tty || Utils.overflows () then
        begin
          Format.printf "Summary:@.";
          Queue.iter pp_error summary ;
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
