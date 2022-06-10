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
  | Ulist
  | Olist

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
  lexbuf: Lexing.lexbuf;
  channel: in_channel;
  mutable iscut: bool; (* last token is a space or a newline *)
  mutable wascut: bool; (* there was a space or a newline before *)
  mutable newlines: int; (* number of \n before (space excluded) *)
  mutable waslines: int; (* previous nnumber of \n before *)
  mutable context: context;
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
    newlines = 0;
    waslines = 0;
    iscut = true;
    wascut = true;
  }

let close input =
  close_in_noerr input.channel ;
  input.context <- End

let eof input = input.context = End
let src input = input.context = Src
let doc input = input.context = Doc
let spaced input = input.wascut
let startline input = input.waslines > 0
let emptyline input = input.waslines > 1
let position input =
  Lexing.lexeme_start_p input.lexbuf ,
  Lexing.lexeme_end_p input.lexbuf

let fetch input =
  match input.context with
  | End -> Eof
  | Src -> !src_lexer input.lexbuf
  | Doc -> !doc_lexer input.lexbuf

let spaces input tk =
  begin
    input.wascut <- input.iscut ;
    input.waslines <- input.newlines ;
    match tk with
    | Space ->
      input.iscut <- true ;
    | Newline ->
      input.iscut <- true ;
      input.newlines <- succ input.newlines ;
    | _ ->
      input.iscut <- false ;
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
