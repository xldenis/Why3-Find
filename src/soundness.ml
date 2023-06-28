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
(* --- Compute Soundness                                                  --- *)
(* -------------------------------------------------------------------------- *)

module Hid = Why3.Ident.Hid
module Thy = Why3.Theory

type instance = { into : Thy.theory ; instance : Docref.instance }
type henv = {
  theories : Docref.theory Hid.t ;
  instances : instance list Hid.t;
}

let init () = {
  theories = Hid.create 0 ;
  instances = Hid.create 0 ;
}

let register henv (thy : Docref.theory) =
  Hid.add henv.theories thy.theory.th_name thy ;
  List.iter (fun ins ->
      let th = Docref.instance ins in
      let inst = { into = thy.theory ; instance = ins } in
      let known = Hid.find_def henv.instances [] th in
      Hid.replace henv.instances th (inst :: known)
    ) thy.instances

(* -------------------------------------------------------------------------- *)
