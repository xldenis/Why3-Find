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
(* --- Why3 Environment                                                   --- *)
(* -------------------------------------------------------------------------- *)

val args : (string * Arg.spec * string) list
val get : string -> of_json:(Json.t -> 'a) -> 'a
val set : string -> to_json:('a -> Json.t) -> 'a -> unit
val argv : string list -> string list
val save : unit -> unit

type env = {
  wconfig : Why3.Whyconf.config ;
  wenv : Why3.Env.env ;
}

val init : pkgs:string list -> env
