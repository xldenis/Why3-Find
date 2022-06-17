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

type mode =
  | Body
  | Pre
  | Div
  | Par
  | Emph
  | Bold
  | Head of Pdoc.buffer * int
  | List of int (* indent *)
  | Item of int (* indent *)

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
  mutable scope : string option ; (* current module name *)
  mutable space : bool ; (* leading space in Par mode *)
  mutable mode : mode ; (* current output mode *)
  mutable file : mode ; (* global file output mode *)
  mutable stack : mode list ; (* currently opened modes *)
  mutable opened : int ; (* number of pending 'end' *)
}

let clear env =
  env.space <- false

let space env =
  if env.space then
    begin
      Pdoc.pp_print_char env.out ' ' ;
      clear env ;
    end

let push env m =
  env.stack <- env.mode :: env.stack ;
  env.mode <- m ;
  match m with
  | Body | Head _ -> ()
  | Div -> Pdoc.pp_print_string env.out "<div class=\"doc\">\n"
  | Pre -> Pdoc.pp_print_string env.out "<pre class=\"src\">\n"
  | Par -> Pdoc.pp_print_string env.out "<p>"
  | List _ -> Pdoc.pp_print_string env.out "<ul>\n"
  | Item _ -> Pdoc.pp_print_string env.out "<li>"
  | Emph -> space env ; Pdoc.pp_print_string env.out "<em>"
  | Bold -> space env ; Pdoc.pp_print_string env.out "<strong>"

let pop env =
  let m = env.mode in
  begin match env.stack with [] -> () | old :: stk ->
    env.mode <- old ;
    env.stack <- stk ;
  end ;
  match m with
  | Body | Head _ -> ()
  | Pre -> Pdoc.pp_print_string env.out "</pre>\n" ; clear env
  | Div -> Pdoc.pp_print_string env.out "</div>\n" ; clear env
  | Par -> Pdoc.pp_print_string env.out "</p>\n" ; clear env
  | List _ -> Pdoc.pp_print_string env.out "</ul>\n" ; clear env
  | Item _ -> Pdoc.pp_print_string env.out "</li>\n" ; clear env
  | Emph -> Pdoc.pp_print_string env.out "</em>" (* keep space *)
  | Bold -> Pdoc.pp_print_string env.out "</strong>" (* keep space *)

let text env =
  match env.mode with
  | Body -> push env env.file
  | Div -> push env Par
  | List n -> push env (Item n)
  | Pre -> ()
  | Par | Item _ | Head _ | Emph | Bold -> space env

let rec close env =
  match env.mode with
  | Body -> ()
  | Emph -> Token.error env.input "unclosed emphasis style"
  | Bold -> Token.error env.input "unclosed bold style"
  | Head _ -> Token.error env.input "unclose headings"
  | Div | Par | List _ | Item _ | Pre -> pop env ; close env

(* -------------------------------------------------------------------------- *)
(* --- References                                                         --- *)
(* -------------------------------------------------------------------------- *)

let rec fetch_id input =
  match Token.token input with
  | Space | Newline -> fetch_id input
  | Ident s -> s
  | _ -> Token.error input "missing module or theory name"

let resolve env ?(infix=false) () =
  Docref.resolve ~src:env.src ~infix (Token.position env.input)

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
      kind env.src.url env.src.name id ;
    Pdoc.pp env.out Format.pp_print_string prelude ;
    push env Pre ;
    env.file <- Pre ;
    env.scope <- Some id ;
    Pdoc.pp env.out Pdoc.pp_keyword key ;
    Pdoc.pp_print_char env.out ' ' ;
    process_ref env href id
  end

let process_close env key =
  begin
    Pdoc.printf env.out "%a@\n</pre>@\n" Pdoc.pp_keyword key ;
    env.mode <- Body ;
    env.file <- Body ;
    env.scope <- None ;
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
(* --- Style Processing                                                   --- *)
(* -------------------------------------------------------------------------- *)

let process_style env m =
  text env ;
  if env.mode = m then pop env else push env m

let rec list env n =
  match env.mode with
  | Body | Head _ | Pre -> ()
  | Emph -> Token.error env.input "unclosed bold style"
  | Bold -> Token.error env.input "unclosed emphasis style"
  | Par -> pop env ; push env (List n)
  | Div -> push env (List n)
  | Item _ -> pop env ; list env n
  | List m ->
    if n < m then
      begin
        pop env ;
        list env n ;
      end
    else
    if n > m then
      push env (List n)

let process_dash env s =
  let s = String.length s in
  if s = 1 && Token.startline env.input then
    list env (Token.indent env.input)
  else
    begin
      text env ;
      Pdoc.printf env.out "&%cdash;" (if s > 1 then 'm' else 'n')
    end

let process_header env h =
  begin
    let level = String.length h in
    let buffer = Pdoc.push env.out in
    push env (Head(buffer,level))
  end

(* -------------------------------------------------------------------------- *)
(* --- Space & Newline Processing                                           --- *)
(* -------------------------------------------------------------------------- *)

let process_space env =
  match env.mode with
  | Body | Div | List _ -> ()
  | Par | Item _ | Head _ | Emph | Bold -> env.space <- true
  | Pre ->
    Pdoc.pp_print_char env.out ' '

let process_newline env =
  match env.mode with
  | Body -> if Token.emptyline env.input then Pdoc.flush env.out
  | Div | List _ -> ()
  | Par | Item _ ->
    if Token.emptyline env.input then
      pop env
    else
      env.space <- true
  | Emph | Bold -> env.space <- true
  | Head(buffer,level) ->
    let title = Pdoc.buffered env.out in
    Pdoc.pop env.out buffer ;
    Pdoc.header env.out ~level ~title () ;
    pop env
  | Pre ->
    Pdoc.pp_print_char env.out '\n' ;
    Pdoc.flush env.out

(* -------------------------------------------------------------------------- *)
(* --- References                                                         --- *)
(* -------------------------------------------------------------------------- *)

let process_reference ~why3env ~env r =
  try
    let scope = match env.scope with None -> "" | Some m -> m in
    let url,name = Docref.reference ~why3env ~src:env.src ~scope r in
    text env ;
    Pdoc.printf env.out
      "<code class=\"src\"><a href=\"%s\">%s</a></code>" url name
  with
  | Not_found -> Token.error env.input "unknown reference"
  | Failure msg -> Token.error env.input "%s" msg

(* -------------------------------------------------------------------------- *)
(* --- File Processing                                                    --- *)
(* -------------------------------------------------------------------------- *)

let process_file ~why3env ~out:dir file =
  let src = Docref.parse ~why3env file in
  let title = Printf.sprintf "Library %s" src.name in
  let out = Pdoc.output ~file:(Filename.concat dir src.url) ~title in
  let input = Token.input file in
  let env = {
    dir ; src ; input ; out ; space = false ; scope = None ;
    mode = Body ; file = Body ; stack = [] ; opened = 0 ;
  } in
  begin
    Pdoc.printf out "<header>Library <tt>%s</tt></header>@\n" src.name ;
    while not (Token.eof env.input) do
      match Token.token env.input with
      | Eof -> close env
      | Char c -> text env ; Pdoc.pp_html_c out c
      | Text s -> text env ; Pdoc.pp_html_s out s
      | Comment s ->
        text env ;
        Pdoc.pp_html_s out ~className:"comment" s
      | Verb s ->
        text env ;
        Pdoc.printf out "<code class=\"src\">%a</code>" Pdoc.pp_html s
      | Ref s -> process_reference ~why3env ~env s
      | Style(Emph,_) -> process_style env Emph
      | Style(Bold,_) -> process_style env Bold
      | Style(Head,h) -> process_header env h
      | Style(Dash,n) -> process_dash env n
      | Style _ -> ()
      | OpenDoc ->
        begin
          Pdoc.flush ~onlyspace:false out ;
          close env ;
          push env Div ;
        end
      | CloseDoc ->
        begin
          close env ;
          push env env.file ;
        end
      | Space -> process_space env
      | Newline -> process_newline env
      | Ident s -> text env ; process_ident env s
      | Infix s ->
        text env ;
        let href = resolve env ~infix:true () in
        process_ref env href s
    done ;
    Pdoc.close_all out ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Main Doc Command                                                   --- *)
(* -------------------------------------------------------------------------- *)

let shared ~out ~file =
  let tgt = Filename.concat out file in
  if not (Sys.file_exists tgt) then
    let src = Meta.shared file in
    Utils.copy ~src ~tgt

let main ~pkgs ~out ~files =
  begin
    let why3env = Docref.init ~pkgs in
    Utils.mkdirs out ;
    shared ~out ~file:"style.css" ;
    List.iter (process_file ~why3env ~out) files
  end

(* -------------------------------------------------------------------------- *)
