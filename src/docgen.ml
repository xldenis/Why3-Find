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

type mode = Body | Pre | Doc | Emph | Bold

let open_mode out = function
  | Body -> ()
  | Doc -> Pdoc.printf out "<div class=\"doc\">"
  | Pre -> Pdoc.printf out "<pre class=\"src\">@\n"
  | Emph -> Pdoc.printf out "<em>"
  | Bold -> Pdoc.printf out "<strong>"

let close_mode out = function
  | Body -> ()
  | Doc -> Pdoc.printf out "</div>@\n"
  | Pre -> Pdoc.printf out "</pre>@\n"
  | Emph -> Pdoc.printf out "</em>"
  | Bold -> Pdoc.printf out "</strong>"

let switch out ~mode style =
  if mode = style then
    (close_mode out mode ; Doc)
  else
    ((if mode <> Doc then close_mode out mode) ; open_mode out style ; style)

let is_opening = function
  | "scope" | "match" | "try" | "begin" -> true
  | _ -> false

let is_closing = function
  | "end" -> true
  | _ -> false

(* -------------------------------------------------------------------------- *)
(* --- Environment                                                        --- *)
(* -------------------------------------------------------------------------- *)

type env = {
  dir : string ; (* destination directory *)
  src : Docref.source ; (* source file infos *)
  input : Token.input ; (* input lexer *)
  out : Pdoc.output ; (* output buffer *)
  mutable mode : mode ; (* current output state *)
  mutable file : mode ; (* default file mode *)
  mutable opened : int ; (* number of pending 'end' *)
}

(* -------------------------------------------------------------------------- *)
(* --- References                                                         --- *)
(* -------------------------------------------------------------------------- *)

let rec fetch_id input =
  match Token.token input with
  | Space | Newline -> fetch_id input
  | Ident s -> s
  | _ -> Token.error input "missing module or theory name"

let resolve env ?(infix=false) () =
  Docref.resolve ~pkg:env.src.pkg ~infix (Token.position env.input)

let process_ref env (href : Docref.href) s =
  match href with
  | Docref.Def name ->
    Pdoc.printf env.out "<a name=\"%s\">%a</a>" name Pdoc.pp_html s
  | Docref.Ref(url,name) ->
    Pdoc.printf env.out "<a href=\"%s#%s\">%a</a>" url name Pdoc.pp_html s
  | Docref.NoRef ->
    Pdoc.pp_html_s env.out s

(* -------------------------------------------------------------------------- *)
(* --- Module & Theory Processing                                         --- *)
(* -------------------------------------------------------------------------- *)

let process_module env key =
  begin
    if not (env.mode = Body && env.opened = 0) then
      Token.error env.input "unexpected module or theory" ;
    let prelude = Pdoc.buffered env.out in
    let id = fetch_id env.input in
    let href = resolve env () in
    let kind = String.capitalize_ascii key in
    let url = Docref.derived env.src id in
    Pdoc.printf env.out
      "<div class=\"src %s\">%s <tt><a href=\"%s\">%s</a></tt></div>@."
      key key url id ;
    let file = Filename.concat env.dir url in
    let title = Printf.sprintf "%s %s.%s" kind env.src.name id in
    Pdoc.fork env.out ~file ~title ;
    Pdoc.printf env.out
      "<header>%s <tt><a href=\"%s\">%s</a>.%s</tt></header>@\n"
      kind env.src.url env.src.name id
    ;
    Pdoc.printf env.out "%s" prelude ;
    open_mode env.out Pre ;
    env.mode <- Pre ;
    env.file <- Pre ;
    Pdoc.printf env.out "%a " Pdoc.pp_keyword key ;
    process_ref env href id
  end

let process_close env key =
  begin
    Pdoc.printf env.out "%a@\n" Pdoc.pp_keyword key ;
    close_mode env.out Pre ;
    env.mode <- Body ;
    env.file <- Body ;
    Pdoc.close env.out ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Identifiers                                                        --- *)
(* -------------------------------------------------------------------------- *)

let process_ident env s =
  if Docref.is_keyword s then
    begin
      if s = "module" || s = "theory" then
        process_module env s
      else if s = "end" && env.opened = 0 then
        process_close env s
      else
        begin
          if is_opening s then env.opened <- succ env.opened ;
          if is_closing s then env.opened <- pred env.opened ;
          Pdoc.pp env.out Pdoc.pp_keyword s ;
        end
    end
  else
    let href = resolve env () in
    process_ref env href s

(* -------------------------------------------------------------------------- *)
(* --- Newline Processing                                                 --- *)
(* -------------------------------------------------------------------------- *)

let process_newline env =
  match env.mode with
  | Body ->
    if Token.emptyline env.input then Pdoc.flush env.out
  | Doc | Emph | Bold ->
    Pdoc.printf env.out "@\n"
  | Pre ->
    Pdoc.printf env.out "@\n" ;
    Pdoc.flush env.out

(* -------------------------------------------------------------------------- *)
(* --- Style Processing                                                   --- *)
(* -------------------------------------------------------------------------- *)

let process_style env m =
  env.mode <- switch env.out ~mode:env.mode m

(* -------------------------------------------------------------------------- *)
(* --- File Processing                                                    --- *)
(* -------------------------------------------------------------------------- *)

let process_file ~env ~out:dir file =
  let src = Docref.parse ~env file in
  let title = Printf.sprintf "Library %s" src.name in
  let out = Pdoc.output ~file:(Filename.concat dir src.url) ~title in
  let input = Token.input file in
  let env = {
    dir ; src ; input ; out ;
    file = Body ; mode = Body ;
    opened = 0 ;
  } in
  begin
    Pdoc.printf out "<header>Library <tt>%s</tt></header>@\n" src.name ;
    while not (Token.eof env.input) do
      match Token.token env.input with
      | Eof -> close_mode out env.mode
      | Char c -> Pdoc.pp_html_c out c
      | Text s -> Pdoc.pp_html_s out s
      | Comment s ->
        Pdoc.pp_html_s out ~className:"comment" s
      | Verb s | Ref s ->
        Pdoc.printf out "<code class=\"src\">%a</code>" Pdoc.pp_html s
      | Style(Emph,_) -> process_style env Emph
      | Style(Bold,_) -> process_style env Bold
      | Style _ -> ()
      | OpenDoc ->
        begin
          Pdoc.flush ~indent:false out ;
          close_mode out env.mode ;
          env.mode <- Doc ;
          open_mode out Doc ;
        end
      | CloseDoc ->
        begin
          close_mode out Doc ;
          env.mode <- env.file ;
          open_mode out env.file ;
        end
      | Space ->
        begin match env.mode with
          | Body -> ()
          | Pre | Doc | Emph | Bold -> Pdoc.pp out Format.pp_print_char ' '
        end
      | Newline -> process_newline env
      | Ident s -> process_ident env s
      | Infix s ->
        let href = resolve env ~infix:true () in
        process_ref env href s
    done ;
    Pdoc.close_all out ;
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
