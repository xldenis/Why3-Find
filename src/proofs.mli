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
(* --- Proof Files                                                        --- *)
(* -------------------------------------------------------------------------- *)

module M = Why3.Wstdlib.Mstr

type profile = Calibration.profile
type theories = Crc.crc M.t M.t

val proofs_file : string -> string
(** Returns the proofs file associated with the given mlw file *)

val load_proofs : ?local:bool -> string -> profile * theories
(**
   Load the content of the proofs file associated with the given mlw file.
   If [~local:true] is specified, the calibration profile also inherits from
   the local environment. Otherwize, it is taken as it is from the file.
*)

type dumper

val create : string -> profile -> dumper

val add : Session.theory -> string -> Crc.crc -> dumper -> unit

val dump : dumper -> unit
