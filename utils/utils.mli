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
val rmpath : string -> unit
val copy : src:string -> tgt:string -> unit
val locate : string -> (string * string) option
val chdir : string -> unit
val absolute : string -> string

val load : file:string -> Buffer.t -> unit
val readfile : file:string -> string
val writefile : file:string -> string -> unit

val readdir : (string -> unit) -> string -> unit
val iterpath :
  ?enter:(string -> unit) ->
  ?file:(string -> unit) ->
  ?leave:(string -> unit) ->
  string -> unit

val pp_hex : Format.formatter -> string -> unit
val pp_arg : Format.formatter -> string -> unit
val pp_args : Format.formatter -> string list -> unit

val pp_ok : Format.formatter -> unit
val pp_ko : Format.formatter -> unit
val pp_mark : Format.formatter -> bool -> unit
val pp_time : Format.formatter -> float -> unit
val pp_s : Format.formatter -> int -> unit
val pp_yies : Format.formatter -> int -> unit

val round : float -> float

val tty : bool
val flush : unit -> unit
val log : ('a,Format.formatter,unit) format -> 'a
val progress : ('a,Format.formatter,unit) format -> 'a
val failwith : ('a, Format.formatter, unit, 'b) format4 -> 'a

(* -------------------------------------------------------------------------- *)
