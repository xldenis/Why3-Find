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
(* --- System Utils                                                       --- *)
(* -------------------------------------------------------------------------- *)

module F = Filename

let rec cleanup path =
  if Sys.file_exists path then
    if Sys.is_directory path then
      begin
        Array.iter
          (fun d -> cleanup (F.concat path d))
          (Sys.readdir path) ;
        Sys.rmdir path
      end
    else
      Sys.remove path

let rec mkdirs = function
  | "/" | "." -> ()
  | path ->
    if not (Sys.file_exists path) then
      begin
        mkdirs (F.dirname path) ;
        Sys.mkdir path 0o755 ;
      end

let copy ~src ~tgt =
  mkdirs (F.dirname tgt) ;
  let buffer = Bytes.create 2048 in
  let inc = open_in src in
  let out = open_out tgt in
  let rec walk () =
    let n = Stdlib.input inc buffer 0 (Bytes.length buffer) in
    if n > 0 then
      ( Stdlib.output out buffer 0 n ; walk () )
  in walk () ; close_in inc ; close_out out

let locate files =
  let rec lookup dir prefix =
    match dir with
    | "/" | "." -> None
    | _ ->
      if List.exists
          (fun f -> Sys.file_exists (F.concat dir f))
          files
      then Some (dir,prefix)
      else lookup (F.dirname dir) (F.concat (F.basename dir) prefix)
  in lookup (Sys.getcwd ()) ""

let chdir dir =
  Format.printf "Entering directory '%s'@." dir ;
  Sys.chdir dir

(* -------------------------------------------------------------------------- *)
(* --- Time Printing                                                      --- *)
(* -------------------------------------------------------------------------- *)

let pp_time fmt t =
  let t = if t < 0.0 then (Format.pp_print_char fmt '-' ; -. t) else t in
  if t < 1e-3 then Format.fprintf fmt "%dns" (int_of_float @@ t *. 1e6) else
  if t < 1.0 then Format.fprintf fmt "%dms" (int_of_float @@ t *. 1e3) else
  if t < 20.0 then Format.fprintf fmt "%.1fs" t else
    Format.fprintf fmt "%ds" (int_of_float t)

let pp_ok fmt = Format.fprintf fmt "@{<green>\u{2714}@}"
let pp_ko fmt = Format.fprintf fmt "@{<red>\u{2718}@}"
let pp_weak fmt = Format.fprintf fmt "@{<orange>\u{2718}@}"
let pp_mark fmt b = if b then pp_ok fmt else pp_ko fmt

(* -------------------------------------------------------------------------- *)
(* --- Terminal Facilities                                                --- *)
(* -------------------------------------------------------------------------- *)

let tty = Unix.isatty Unix.stdout

let progress msg =
  let buffer = Buffer.create 80 in
  Format.kfprintf
    (fun fmt ->
       Format.pp_print_flush fmt () ;
       if tty then
         let msg = Buffer.contents buffer in
         let len = String.length msg in
         if len <= 80 then
           Format.printf "> %s\027[K\r@?" msg
         else
           Format.printf "> %s…\027[K\r@?" (String.sub msg 0 80)
    ) (Format.formatter_of_buffer buffer) msg

let flush () = if tty then Format.printf "\r\027[K"

open Format

let nop _ = ()

let mark_open_stag = function
  | String_tag "red" -> "\027[31m"
  | String_tag "green" -> "\027[32m"
  | String_tag "orange" -> "\027[33m"
  | _ -> ""

let mark_close_stag = function
  | String_tag ("red"|"green"|"orange") -> "\027[39m"
  | _ -> ""

let () = if tty then
    begin
      set_tags true ;
      set_formatter_stag_functions {
        mark_open_stag ;
        mark_close_stag ;
        print_open_stag = nop ;
        print_close_stag = nop ;
      }
    end

(* -------------------------------------------------------------------------- *)
