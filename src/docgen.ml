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
(* --- HTML Mode                                                          --- *)
(* -------------------------------------------------------------------------- *)

type mode = Body | Pre | Div

let open_mode out = function
  | Body -> ()
  | Div -> Pdoc.printf out "<div class=\"doc\">"
  | Pre -> Pdoc.printf out "<pre class=\"src\">@\n"

let close_mode out = function
  | Body -> ()
  | Div -> Pdoc.printf out "</div>@\n"
  | Pre -> Pdoc.printf out "</pre>@\n"

let is_opening = function
  | "scope" | "match" | "try" | "begin" -> true
  | _ -> false

let is_closing = function
  | "end" -> true
  | _ -> false

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
  let out = Pdoc.output ~file:(Filename.concat out src.url) ~title in
  let input = Token.input file in
  let resolve ?(infix=false) () =
    Docref.resolve ~pkg:src.pkg ~infix (Token.position input)
  in
  let mode = ref Body in
  let opened = ref 0 in
  begin
    Pdoc.printf out "<header>%s</header>@\n" title ;
    while not (Token.eof input) do
      match Token.token input with
      | Eof -> close_mode out !mode
      | Char c -> Pdoc.pp_html_c out c
      | Text s -> Pdoc.pp_html_s out s
      | OpenDoc ->
        begin
          if not @@ Token.startline input then Pdoc.printf out "@\n" ;
          close_mode out !mode ;
          mode := Div ;
          open_mode out Div ;
        end
      | CloseDoc ->
        begin
          close_mode out Div ;
          mode := Body ;
        end
      | Space ->
        begin match !mode with
          | Body -> ()
          | Pre | Div -> Pdoc.pp out Format.pp_print_char ' '
        end
      | Newline ->
        begin match !mode with
          | Body -> if Token.emptyline input then Pdoc.flush out
          | Pre | Div -> Pdoc.printf out "@\n"
        end
      | Ident (("module" | "theory") as key) ->
        begin
          if not (!opened = 0) then
            Token.error input "unexpected module or theory" ;
          close_mode out !mode ;
          open_mode out Pre ;
          mode := Pre ;
          Pdoc.printf out "<span class=\"keyword\">%s</span>" key ;
        end
      | Ident ("end" as key) when !opened = 0 ->
        begin
          Pdoc.printf out "<span class=\"keyword\">%s</span>@\n" key ;
          close_mode out Pre ;
          mode := Body ;
        end
      | Ident s ->
        begin
          if is_opening s then
            begin
              incr opened ;
              Pdoc.pp_html_s out ~className:"keyword" s ;
            end
          else
          if is_closing s then
            begin
              decr opened ;
              Pdoc.pp_html_s out ~className:"keyword" s ;
            end
          else
            process_ident out resolve s
        end
      | Infix s -> process_ident out (resolve ~infix:true) s
      | Comment s -> Pdoc.pp_html_s out ~className:"comment" s
      | Verb s | Ref s ->
        Pdoc.printf out "<code class=\"src\">%a</code>" Pdoc.pp_html s
      | Style _ -> ()
    done ;
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
