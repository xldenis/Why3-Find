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
(* --- Documentation Lexer                                                --- *)
(* -------------------------------------------------------------------------- *)

{
  open Token

  let buffered parser lexbuf =
    let buffer = Buffer.create 80 in
    Buffer.add_string buffer (Lexing.lexeme lexbuf) ;
    parser buffer lexbuf ;
    Buffer.contents buffer

  let add = Buffer.add_string
  let addc = Buffer.add_char
  let newline = Lexing.new_line
  let style sty lexbuf = Style(sty,Lexing.lexeme lexbuf)

}

(* -------------------------------------------------------------------------- *)

let op_char = [
  '=' '<' '>' '~'
  '+' '-' '*' '/' '%'
  '!' '$' '&' '?' '@' '^' '.' ':' '|' '#'
  '_'
]

let ident = ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']*

let operator =
  op_char+
  | "[]"
  | "[<-]"
  | "[]<-"

let space = [' ' '\t']

(* -------------------------------------------------------------------------- *)
(* --- Source Code Lexer                                                  --- *)
(* -------------------------------------------------------------------------- *)

rule source = parse
  | eof { Eof }
  | '\n' { newline lexbuf ; Newline }
  | "(*)" { Infix "(*)" }
  | "(*" '*'* "*)" { Comment (Lexing.lexeme lexbuf) }
  | "(*" '*'+  space* { OpenDoc }
  | "(*" { Comment (buffered (comment 0) lexbuf) }
  | (ident | operator) as id { Ident id }
  | _ as c { Char c }

and comment level buffer = parse
  | eof { () }
  | "(*" { add buffer "(*" ; comment (succ level) buffer lexbuf }
  | "*)" { add buffer "*)" ; if level > 0 then comment (pred level) buffer lexbuf }
  | '\n' { add buffer "\n" ; newline lexbuf ; comment level buffer lexbuf }
  | _ as c { addc buffer c ; comment level buffer lexbuf }

(* -------------------------------------------------------------------------- *)
(* --- Document Lexer                                                     --- *)
(* -------------------------------------------------------------------------- *)

and document = parse
  | eof { Eof }
  | '\n' { newline lexbuf ; Newline }
  | space+ { Space }
  | space* '*'+ ')' { CloseDoc }
  | '#'+ { style Head lexbuf }
  | '-' { style Ulist lexbuf }
  | ['0'-'9']+ '.' { style Olist lexbuf }
  | '-'+ { style Dash lexbuf }
  | '_' | '*' { style Emph lexbuf }
  | "__" | "**" { style Bold lexbuf }
  | '{' ([^'}']* as rf) '}' { Ref rf }
  | '`' ([^'`']* as tt) '`' { Verb tt }
  | _ as c { Char c }

(* -------------------------------------------------------------------------- *)
(* --- Bootstrap                                                          --- *)
(* -------------------------------------------------------------------------- *)

{
  let () =
    begin
      Token.src_lexer := source ;
      Token.doc_lexer := document ;
    end
}

(* -------------------------------------------------------------------------- *)
