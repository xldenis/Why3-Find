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

(** Span the printer with a class. *)
val pp_span : string -> 'a fmt -> 'a fmt

(** Sanitize to HTML. *)
val pp_html : string fmt

(** Sanitize to HTML. *)
val to_html : string -> string

type output

(** Open with (sanitized) title. *)
val output : file:string -> title:string -> output

(** Prints (sanitized) contents. *)
val printf : output -> ('a, Format.formatter, unit) format -> 'a

(** Prints (sanitized) header and collect it inside TOC. *)
val header : output -> level:int -> title:string -> unit

(** Flush the output on disk. *)
val close : output -> unit

(* -------------------------------------------------------------------------- *)
