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
(* --- Why3 Find Builtin Commands                                         --- *)
(* -------------------------------------------------------------------------- *)

val mkdirs : string -> unit
val cleanup : string -> unit
val copy : src:string -> tgt:string -> unit
val locate : string list -> (string * string) option
val chdir : string -> unit

val pp_ok : Format.formatter -> unit
val pp_ko : Format.formatter -> unit
val pp_weak : Format.formatter -> unit
val pp_mark : Format.formatter -> bool -> unit
val pp_time : Format.formatter -> float -> unit

val tty : bool
val flush : unit -> unit
val progress : ('a,Format.formatter,unit) format -> 'a

(* -------------------------------------------------------------------------- *)
