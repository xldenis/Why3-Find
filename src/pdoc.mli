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
(* --- HTML Utilities                                                     --- *)
(* -------------------------------------------------------------------------- *)

type 'a fmt = Format.formatter -> 'a -> unit
type 'a printf =  ('a, Format.formatter, unit) format -> 'a

(** Span the printer with a class. *)
val pp_span : ?className:string -> 'a fmt -> 'a fmt

(** Sanitize element NAME to HTML. *)
val pp_name : string fmt

(** Sanitize to HTML. *)
val pp_html : string fmt

(** Sanitize to HTML. *)
val to_html : string -> string

(** Keyword (sanitized) class. *)
val pp_keyword : string fmt

(** Attribute (sanitized) class. *)
val pp_attribute : string fmt

(** Spaces. *)
val pp_spaces : int fmt

(* -------------------------------------------------------------------------- *)
(* --- HTML Buffers                                                       --- *)
(* -------------------------------------------------------------------------- *)

type buffer
val buffer : unit -> buffer
val bprintf : buffer -> 'a printf

val contents : buffer -> string
val pp_buffer : buffer fmt

(* -------------------------------------------------------------------------- *)
(* --- HTML Output                                                        --- *)
(* -------------------------------------------------------------------------- *)

type output

(** Open with (sanitized) title. *)
val output : file:string -> title:string -> output

(** Stack current buffer. *)
val push : output -> buffer

(** Pop previous buffer and flush buffered contents into. *)
val pop : output -> buffer -> unit

(** Extract and clear the current output buffer. *)
val buffered : output -> string

(** Flushes the current output buffer.
    When [~indent:false], space-only trailing contents is {i not} output.
*)
val flush : ?onlyspace:bool -> output -> unit

(** Fork the current output buffer into another file. *)
val fork : output -> file:string -> title:string -> unit

(** Flushes the output on disk. Restore output of previous fork, if any. *)
val close : output -> unit

(** Prints (sanitized) contents. *)
val printf : output -> 'a printf

(** Prints (sanitized) contents. *)
val pp_print_string : output -> string -> unit

(** Prints (sanitized) contents. *)
val pp_print_char : output -> char -> unit

(** Print the data with the pretty printer. *)
val pp : output -> 'a fmt -> 'a -> unit

(** Sanitize then print the string with the optional class. *)
val pp_html_s : output -> ?className:string -> string -> unit

(** Sanitize then print the char. *)
val pp_html_c : output -> char -> unit

(** Prints (sanitized) header and collect it inside TOC.
    Optional [~toc] is an alternative (sanitized) [~title]
    for the TOC entry. *)
val header : output -> level:int -> title:string -> ?toc:string -> unit -> unit

(** Flushes all pending outputs on disk. *)
val close_all : output -> unit

(* -------------------------------------------------------------------------- *)
