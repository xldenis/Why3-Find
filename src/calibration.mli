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

val create : unit -> profile
val default : unit -> profile
val of_json : ?default:profile -> Json.t -> profile
val to_json : profile -> Json.t

val mem : profile -> string -> bool
val get : profile -> string -> (int * float) option
val set : profile -> string -> int -> float -> unit
val iter : (string -> int -> float -> unit) -> profile -> unit

val init : profile -> string -> bool

val observed : profile -> Runner.prover -> float
val velocity : Wenv.env -> profile -> Runner.prover -> float Fibers.t
val profile : Wenv.env -> profile -> Runner.prover -> (int * float) Fibers.t

val gamma : Wenv.env ->
  src:profile -> tgt:profile -> Runner.prover -> float Fibers.t

val calibrate_provers : saved:bool -> Wenv.env -> Runner.prover list -> unit
val velocity_provers : Wenv.env -> Runner.prover list -> unit

val options : (string * Arg.spec * string) list

(* -------------------------------------------------------------------------- *)
