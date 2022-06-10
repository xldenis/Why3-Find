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
val pp_span : string -> 'a fmt -> 'a fmt

(** Sanitize element NAME to HTML. *)
val pp_name : string fmt

(** Sanitize to HTML. *)
val pp_html : string fmt

(** Sanitize to HTML. *)
val to_html : string -> string

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

(** Prints (sanitized) contents. *)
val printf : output -> 'a printf

(** Sanitize then print the string with the optional class. *)
val pp_html_s : output -> ?className:string -> string -> unit

(** Sanitize then print the char. *)
val pp_html_c : output -> char -> unit

(** Print the data with the pretty printer. *)
val pp : output -> 'a fmt -> 'a -> unit

(** Prints (sanitized) header and collect it inside TOC.
    Optional [~toc] is an alternative (sanitized) [~title]
    for the TOC entry. *)
val header : output -> level:int -> title:string -> ?toc:string -> unit -> unit

(** Flush the output on disk. *)
val close : output -> unit

(* -------------------------------------------------------------------------- *)
