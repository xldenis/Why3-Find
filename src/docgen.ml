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
(* --- HTML Documentation Generator                                       --- *)
(* -------------------------------------------------------------------------- *)

module T = Token
module L = Lexer
module P = Pdoc

(* -------------------------------------------------------------------------- *)
(* --- File Processing                                                     --- *)
(* -------------------------------------------------------------------------- *)

let process ~env ~out file =
  begin
    let src = Docref.parse ~env file in
    let libname = String.concat "." src.lib in
    let title = Printf.sprintf "Library <tt>%s</tt>" libname in
    let out =
      let file = Filename.concat out src.url in
      Pdoc.output ~file ~title
    in
    Pdoc.printf out "<h1>%s</h1>@\n" title ;
    Pdoc.close out ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Main Doc Command                                                   --- *)
(* -------------------------------------------------------------------------- *)

let main ~pkgs ~out ~files =
  begin
    let env = Docref.init ~pkgs in
    List.iter (process ~env ~out) files
  end

(* -------------------------------------------------------------------------- *)
