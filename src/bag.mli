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
(* --- Bags                                                               --- *)
(* -------------------------------------------------------------------------- *)

type 'a bag

val empty : 'a bag
val elt : 'a -> 'a bag

val (++) : 'a bag -> 'a bag -> 'a bag
val (+>) : 'a bag -> 'a -> 'a bag
val (@<) : 'a -> 'a bag -> 'a bag
val (+=) : 'a bag ref -> 'a -> unit

val map : ('a -> 'b) -> 'a list -> 'b bag
val merge : ('a -> 'b bag) -> 'a list -> 'b bag

val size : 'a bag -> int
val to_list : 'a bag -> 'a list
val to_array : 'a bag -> 'a array

(* -------------------------------------------------------------------------- *)
