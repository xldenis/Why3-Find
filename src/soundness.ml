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

type soundness =
  | Sound of Docref.instance list
  | Unknown of Docref.instance list

type env = {
  theories : (string,Docref.theory) Hashtbl.t ; (* indexed by path *)
  instances : Sinst.t ref Hid.t ;
  soundness : soundness Hid.t ;
}

let init () = {
  theories = Hashtbl.create 0 ;
  instances = Hid.create 0 ;
  soundness = Hid.create 0 ;
}

let add hinst (c : Docref.clone) =
  let inst = c.id_instance in
  let key = inst.inst_cloned.th_name in
  try
    let s = Hid.find hinst key in
    s := Sinst.add inst !s
  with Not_found ->
    Hid.add hinst key (ref (Sinst.singleton inst))

let register henv (src : Docref.source) =
  let hinst = henv.instances in
  Docref.Mstr.iter
    (fun id (thy : Docref.theory) ->
       let path = Printf.sprintf "%s.%s" (Id.cat src.lib) id in
       Hashtbl.add henv.theories path thy ;
       List.iter (add hinst) thy.clones
    ) src.theories

let is_sound = function Sound _ -> true | Unknown _ -> false

let rec compute (env : env) (th : Docref.theory) : soundness =
  let key = th.theory.th_name in
  try Hid.find env.soundness key with Not_found ->
    Hid.add env.soundness key (Unknown []) ;
    let s =
      if Axioms.parameters th.signature = [] then Sound [] else
        try
          let instances = Sinst.elements !(Hid.find env.instances key) in
          let grounds = List.filter (ground_instance env) instances in
          if grounds <> [] then Sound grounds else Unknown instances
        with Not_found -> Unknown []
    in Hid.replace env.soundness key s ; s

and ground_instance env inst =
  try Hashtbl.find env.theories inst.inst_path |> compute env |> is_sound
  with Not_found -> false

(* -------------------------------------------------------------------------- *)
