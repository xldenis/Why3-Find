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

type instance = {
  path : string ; (* path *)
  rank : int ; (* clone number *)
}

type henv = {
  henv : Axioms.henv ;
  instances : ((instance,Sid.t ref) Hashtbl.t) Hid.t ;
  (* [ cloned -> instance -> concrete parameters ] *)
}

let init henv = {
  henv ;
  instances = Hid.create 0 ;
}

let add henv ~path (c : Docref.clone) =
  let cloned = c.id_section.cloned_theory.th_name in
  let rank = c.id_section.cloned_order in
  let inst = { path ; rank } in
  Format.eprintf "INSTANCE %s#%d: cloned %s@." path rank c.id_source.id_string ;
  let hinst =
    try Hid.find henv.instances cloned with Not_found ->
      let h = Hashtbl.create 1 in
      Hid.add henv.instances cloned h ; h in
  let domain =
    try Hashtbl.find hinst inst with Not_found ->
      let d = ref Sid.empty in
      Hashtbl.add hinst inst d ; d in
  domain := Sid.add c.id_source !domain

let register henv (src : Docref.source) =
  let lib = String.concat "." src.lib in
  Docref.Mstr.iter
    (fun name (thy : Docref.theory) ->
       let path = Printf.sprintf "%s.%s" lib name in
       List.iter (add henv ~path) thy.clones
    ) src.theories

(* -------------------------------------------------------------------------- *)
