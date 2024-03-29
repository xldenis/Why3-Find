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
(* --- Location & Range utilities                                         --- *)
(* -------------------------------------------------------------------------- *)

type pos = int * int
type range = pos * pos

val ( << ) : 'a * 'b -> 'a * 'b -> bool
val ( >> ) : 'a * 'b -> 'a * 'b -> bool
val ( <<= ) : 'a * 'b -> 'a * 'b -> bool
val ( <<< ) : range -> range -> bool

val compare_pos : pos -> pos -> int
val compare_range : range -> range -> int

val start : pos
val next : pos -> pos
val prev : pos -> pos
val newline : pos -> pos
val after : pos -> char -> pos
val min : pos -> pos -> pos
val max : pos -> pos -> pos

val pp_pos : Format.formatter -> pos -> unit
val pp_range : Format.formatter -> range -> unit
val pp_position : Format.formatter -> file:string -> range -> unit

val is_empty : range -> bool
val inside : pos -> range -> bool
val disjoint : range -> range -> bool
val union : range -> range -> range
val diff : range -> range -> range
