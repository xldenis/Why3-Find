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
(* --- Proof Certificates                                                 --- *)
(* -------------------------------------------------------------------------- *)

type crc =
  | Stuck
  | Prover of string * float
  | Transf of {
      id : string ;
      children : crc list ;
      stuck : int ;
      proved : int ;
    }

type verdict = [ `Valid of int | `Failed of int | `Partial of int * int ]
val verdict : crc -> verdict
val nverdict : stuck:int -> proved:int -> verdict

val stuck : crc -> int
val proved : crc -> int
val unknown : crc -> bool
val complete : crc -> bool
val pretty : Format.formatter -> crc -> unit
val pp_result : Format.formatter -> stuck:int -> proved:int -> unit
val dump : Format.formatter -> crc -> unit
val shortname : string -> string (* prover short name *)

val apply : string -> crc list -> crc
val merge : crc -> crc -> crc
val of_json : Json.t -> crc
val to_json : crc -> Json.t

(* -------------------------------------------------------------------------- *)
