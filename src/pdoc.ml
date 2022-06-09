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

let pp_span cla pp fmt v =
  Format.fprintf fmt "<span class=\"%s\">%a</span>" cla pp v

let pp_html = Why3.Pp.html_string
let to_html = Format.asprintf "%a" pp_html

type header = {
  level: int;
  name: string;
  title: string;
}

type output = {
  file: string;
  htitle: string;
  fmt: Format.formatter;
  buffer: Buffer.t;
  mutable headers: header list; (* in reverse order *)
}

let output ~file ~title =
  let buffer = Buffer.create 80 in
  let fmt = Format.formatter_of_buffer buffer in
  { htitle = title ; file ; buffer ; fmt ; headers = [] }

let printf output msg = Format.fprintf output.fmt msg

let header output ~level ~title =
  let name = Printf.sprintf "hd%d" (succ @@ List.length output.headers) in
  output.headers <- { level ; name ; title } :: output.headers ;
  printf output "<h%d><a name=\"%s\">%s</a></h%d>@\n"
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
  Format.pp_print_flush output.fmt () ;
  let cout = open_out output.file in
  output_string cout head ;
  output_string cout output.htitle ;
  output_string cout body ;
  table_of_contents cout (List.rev output.headers) ;
  Buffer.output_buffer cout output.buffer ;
  output_string cout foot ;
  close_out cout

(* -------------------------------------------------------------------------- *)
