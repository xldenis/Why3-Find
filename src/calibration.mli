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
(* --- Prover Calibration                                                 --- *)
(* -------------------------------------------------------------------------- *)

type profile

val config : string
val empty : unit -> profile
val default : unit -> profile
val of_json : ?default:profile -> Json.t -> profile
val to_json : profile -> Json.t
val iter : (string -> int -> float -> unit) -> profile -> unit

val round : float -> float
val observed : profile -> Runner.prover -> float
val velocity : Wenv.env -> profile -> Runner.prover -> float Fibers.t
val parallel : bool ref

val calibrate_provers : save:bool -> time:int -> string list -> unit
val velocity_provers : string list -> unit

(* -------------------------------------------------------------------------- *)
