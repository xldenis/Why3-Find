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

type kind =
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
  | Word of kind * string
  | Ref of string
  | Verb of string

val src_lexer : (Lexing.lexbuf -> token) ref
val doc_lexer : (Lexing.lexbuf -> token) ref

type input

(** Opens the given file. *)
val input : ?doc:bool -> string -> input

(** Closes the opened chanel. *)
val close : input -> unit

(** Fetch the next token from input. *)
val token : input -> token (** next buffer *)

(** The last returned token is preceeded by a space or a newline. *)
val spaced : input -> bool

(** The last returned token is at the beginning of the line. *)
val startline : input -> bool

(** The last returned token is at then beginning of the line
    {i and} after an empty line. *)
val emptyline : input -> bool

(* -------------------------------------------------------------------------- *)
