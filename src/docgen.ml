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
(* --- Identifiers                                                        --- *)
(* -------------------------------------------------------------------------- *)

let process_ident out (resolve : unit -> Docref.href) s =
  if Docref.is_keyword s then
    Pdoc.pp_html_s out ~className:"keyword" s
  else
    match resolve () with
    | Def name ->
      Pdoc.printf out "<a name=\"%s\">%a</a>" name Pdoc.pp_html s
    | Ref(url,name) ->
      Pdoc.printf out "<a href=\"%s#%s\">%a</a>" url name Pdoc.pp_html s
    | _ ->
      Pdoc.pp_html_s out s

(* -------------------------------------------------------------------------- *)
(* --- File Processing                                                    --- *)
(* -------------------------------------------------------------------------- *)

let process_file ~env ~out file =
  let src = Docref.parse ~env file in
  let libname = String.concat "." src.lib in
  let title = Printf.sprintf "Library <tt>%s</tt>" libname in
  let out =
    let file = Filename.concat out src.url in
      Pdoc.output ~file ~title
  in
  let input = Token.input file in
  let resolve ?(infix=false) () =
    Docref.resolve ~pkg:src.pkg ~infix (Token.position input)
  in
  begin
    Pdoc.printf out "<h1>%s</h1>@\n" title ;
    Pdoc.printf out "<pre class=\"src\">@\n" ;
    while not (Token.eof input) do
      match Token.token input with
      | Eof -> ()
      | Space -> Pdoc.pp out Format.pp_print_char ' '
      | Char c -> Pdoc.pp_html_c out c
      | Text s -> Pdoc.pp_html_s out s
      | Ident s -> process_ident out resolve s
      | Infix s -> process_ident out (resolve ~infix:true) s
      | Comment s -> Pdoc.pp_html_s out ~className:"comment" s
      | Newline -> Pdoc.printf out "@\n"
      | OpenDoc ->
        if not @@ Token.startline input then Pdoc.printf out "@\n" ;
        Pdoc.printf out "</pre>@\n<div class=\"doc\">" ;
      | CloseDoc ->
        Pdoc.printf out "</div>@\n<pre class=\"src\">@\n" ;
      | _ -> ()
    done ;
    Pdoc.printf out "</pre>@\n" ;
    Pdoc.close out ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Main Doc Command                                                   --- *)
(* -------------------------------------------------------------------------- *)

let install ~out ~file =
  let tgt = Filename.concat out file in
  if not (Sys.file_exists tgt) then
    let src = Meta.shared file in
    Utils.copy ~src ~tgt

let main ~pkgs ~out ~files =
  begin
    let env = Docref.init ~pkgs in
    Utils.mkdirs out ;
    install ~out ~file:"style.css" ;
    List.iter (process_file ~env ~out) files
  end

(* -------------------------------------------------------------------------- *)
