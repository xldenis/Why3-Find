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
(* --- HTML Doc Printer                                                   --- *)
(* -------------------------------------------------------------------------- *)

type 'a fmt = Format.formatter -> 'a -> unit
type 'a printf =  ('a, Format.formatter, unit) format -> 'a

let pp_span ?className pp fmt v =
  match className with
  | None -> pp fmt v
  | Some cla -> Format.fprintf fmt "<span class=\"%s\">%a</span>" cla pp v

let urichars = function
  | 'A'..'Z'
  | 'a'..'z'
  | '0'..'9'
  | '-' | '.' | '_' | '~' | '!' | '$'
  | '\'' | '(' | ')' | '*' | '+'
  | ',' | ';' | '=' | ':' | '@' | '/' | '?'
    -> true
  | _ -> false

let pp_name fmt a =
  String.iter
    begin fun c ->
      if urichars c then
        Format.pp_print_char fmt c
      else
        Format.fprintf fmt "%%%02X" (Char.code c)
    end a

let pp_html = Why3.Pp.html_string
let to_html = Format.asprintf "%a" pp_html
let pp_keyword fmt s =
  Format.fprintf fmt "<span class=\"keyword\">%s</span>" s

(* -------------------------------------------------------------------------- *)
(* --- Html Buffers                                                       --- *)
(* -------------------------------------------------------------------------- *)

type buffer = {
  buffer : Buffer.t ;
  fmt : Format.formatter ;
}

let buffer () =
  let buffer = Buffer.create 80 in
  let fmt = Format.formatter_of_buffer buffer in
  { buffer ; fmt }

let bprintf (b : buffer) msg = Format.fprintf b.fmt msg

let contents (b : buffer) =
  Format.pp_print_flush b.fmt () ;
  let s = Buffer.contents b.buffer in
  Buffer.clear b.buffer ; s

let pp_buffer fmt b =
  Format.pp_print_string fmt (contents b)

(* -------------------------------------------------------------------------- *)
(* --- Html Output                                                        --- *)
(* -------------------------------------------------------------------------- *)

type header = {
  level: int;
  name: string;
  title: string;
}

type target = {
  file: string;
  htitle: string;
  mutable hbase: int; (* base level *)
  mutable headers: header list; (* in reverse order *)
  contents: Buffer.t;
  forked: target option;
}

type output = {
  mutable target: target;
  mutable current: buffer;
}

let output ~file ~title =
  {
    target = {
      file ; htitle = title ; hbase = 0 ; headers = [] ;
      contents = Buffer.create 80 ;
      forked = None ;
    } ;
    current = buffer () ;
  }

let buffered output = contents output.current

let spaceonly s =
  try
    for i = 0 to String.length s - 1 do
      match s.[i] with ' ' | '\t' | '\n' -> () | _ -> raise Exit
    done ; true
  with Exit -> false

let flush ?(indent=true) output =
  let trailing = buffered output in
  if indent || not (spaceonly trailing) then
    Buffer.add_string output.target.contents trailing

let fork output ~file ~title =
  flush output ;
  output.target <- {
    file ; htitle = title ; hbase = 0 ; headers = [] ;
    contents = Buffer.create 80 ;
    forked = Some output.target ;
  }

let printf output msg = Format.fprintf output.current.fmt msg
let pp output pp v = pp output.current.fmt v

let pp_html_c output c = Why3.Pp.html_char output.current.fmt c
let pp_html_s output ?className s =
  pp_span ?className Why3.Pp.html_string output.current.fmt s

let header output ~level ~title ?(toc=title) () =
  let target = output.target in
  let name = Printf.sprintf "hd%d" (succ @@ List.length target.headers) in
  if target.hbase <= 0 then target.hbase <- level ;
  let level = 1 + level - target.hbase in
  target.headers <- { level ; name ; title = toc } :: target.headers ;
  Format.fprintf output.current.fmt "<h%d><a name=\"%s\">%s</a></h%d>@\n"
    level name title level

let head =
  "<html>\n\
   <head>\n\
   <meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\">\n\
   <link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\">\n\
   <title>"

let body =
  "</title>\n\
   </head>\n\
   <body>\n"

let foot =
  "</body>\n\
   </html>\n"

let table_of_contents cout heads =
  begin
    output_string cout "<nav>\n" ;
    let fmt = Format.formatter_of_out_channel cout in
    List.iter
      (fun { level ; name ; title } ->
         Format.fprintf fmt "<a class=\"toc%d\" href=\"#%s\">%s</a>@\n"
           level name title
      ) heads ;
    Format.pp_print_flush fmt () ;
    output_string cout "</nav>\n" ;
  end

let close output =
  flush output ;
  let target = output.target in
  let cout = open_out target.file in
  output_string cout head ;
  output_string cout target.htitle ;
  output_string cout body ;
  table_of_contents cout (List.rev target.headers) ;
  Buffer.output_buffer cout target.contents;
  output_string cout foot ;
  close_out cout ;
  Option.iter
    (fun tgt -> output.target <- tgt)
    output.target.forked

let close_all output =
  close output ;
  while output.target.forked <> None do
    close output ;
  done

(* -------------------------------------------------------------------------- *)
