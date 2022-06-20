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
(* --- Documentation Generator Tokens                                     --- *)
(* -------------------------------------------------------------------------- *)

type style =
  | Head
  | Emph
  | Bold
  | Dash
  | Ordered

type token =
  | Eof
  | Newline
  | Char of char
  | Text of string
  | Comment of string
  | Infix of string
  | Ident of string
  | OpenDoc
  | CloseDoc
  | Space
  | Style of style * string
  | Ref of string
  | Verb of string

type context = Src | Doc | End

type input = {
  lexbuf : Lexing.lexbuf;
  channel : in_channel;
  mutable newlines : int; (* number of consecutive \n (space excluded) *)
  mutable tokcount : int; (* number of tokens on the line *)
  mutable context : context;
}

let src_lexer = ref (fun _ -> assert false)
let doc_lexer = ref (fun _ -> assert false)

let input ?(doc=false) file =
  let inc = open_in file in
  let lexbuf = Lexing.from_channel inc in
  Lexing.set_filename lexbuf file ;
  {
    lexbuf ;
    channel = inc ;
    context = if doc then Doc else Src;
    tokcount = 0;
    newlines = 0;
  }

let close input =
  close_in_noerr input.channel ;
  input.context <- End

let error input msg =
  Format.kasprintf
    (fun msg ->
       let p = Lexing.lexeme_start_p input.lexbuf in
       let q = Lexing.lexeme_end_p input.lexbuf in
       let a = p.pos_cnum - p.pos_bol in
       let b = q.pos_cnum - p.pos_bol in
       Format.printf "File \"%s\", line %d, characters %d-%d: %s@."
         p.pos_fname p.pos_lnum a b msg ;
       exit 1
    ) msg

let eof input = input.context = End
let src input = input.context = Src
let doc input = input.context = Doc
let emptyline input = input.newlines > 1
let startline input = input.tokcount <= 1

let indent input =
  let Lexing.{ pos_cnum = c ; pos_bol = b } =
    Lexing.lexeme_start_p input.lexbuf
  in c - b

let position input =
  Lexing.lexeme_start_p input.lexbuf ,
  Lexing.lexeme_end_p input.lexbuf

let line input =
  (Lexing.lexeme_start_p input.lexbuf).pos_lnum

let fetch input =
  match input.context with
  | End -> Eof
  | Src -> !src_lexer input.lexbuf
  | Doc -> !doc_lexer input.lexbuf

let spaces input tk =
  begin
    match tk with
    | Space -> ()
    | Newline ->
      input.tokcount <- 0 ;
      input.newlines <- succ input.newlines ;
    | _ ->
      input.tokcount <- succ input.tokcount ;
      input.newlines <- 0 ;
  end

let context input tk =
  match tk with
  | OpenDoc -> input.context <- Doc
  | CloseDoc -> input.context <- Src
  | Eof -> input.context <- End
  | _ -> ()

let token input =
  let tk = fetch input in
  begin
    spaces input tk ;
    context input tk ;
  end ; tk

(* -------------------------------------------------------------------------- *)
