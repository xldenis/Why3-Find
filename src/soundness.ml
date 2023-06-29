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
(* --- Soundness Registry                                                 --- *)
(* -------------------------------------------------------------------------- *)

module Id = Why3.Ident
module Sid = Why3.Ident.Sid
module Hid = Why3.Ident.Hid
module Thy = Why3.Theory
module Sinst = Set.Make
    (struct
      type t = Docref.instance
      let compare (a : t) (b : t) =
        let cmp = String.compare a.inst_path b.inst_path in
        if cmp <> 0 then cmp else a.inst_order - b.inst_order
    end)

type henv = {
  henv : Axioms.henv ;
  instances : Sinst.t ref Hid.t ;
  (* [ cloned -> instance -> concrete parameters ] *)
}

let init henv = {
  henv ;
  instances = Hid.create 0 ;
}

let add henv (c : Docref.clone) =
  let inst = c.id_instance in
  let key = inst.inst_cloned.th_name in
  try
    let s = Hid.find henv.instances key in
    s := Sinst.add inst !s
  with Not_found ->
    Hid.add henv.instances key (ref (Sinst.singleton inst))

let register henv (s : Docref.source) =
  Docref.Mstr.iter
    (fun _ (thy : Docref.theory) -> List.iter (add henv) thy.clones)
    s.theories

(* -------------------------------------------------------------------------- *)
