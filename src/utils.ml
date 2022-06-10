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

let rec cleanup path =
  if Sys.file_exists path then
    if Sys.is_directory path then
      begin
        Array.iter
          (fun d -> cleanup (Filename.concat path d))
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
        mkdirs (Filename.dirname path) ;
        Sys.mkdir path 0o755 ;
      end

let copy ~src ~tgt =
  mkdirs (Filename.dirname tgt) ;
  let buffer = Bytes.create 2048 in
  let inc = open_in src in
  let out = open_out tgt in
  let rec walk () =
    let n = Stdlib.input inc buffer 0 (Bytes.length buffer) in
    if n > 0 then
      ( Stdlib.output out buffer 0 n ; walk () )
  in walk () ; close_in inc ; close_out out

(* -------------------------------------------------------------------------- *)
