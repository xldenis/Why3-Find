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
(* --- Compute Axioms                                                     --- *)
(* -------------------------------------------------------------------------- *)

module Sid = Why3.Ident.Sid
module Mid = Why3.Ident.Mid

(* module Thy = Why3.Theory *)
(* module Mod = Why3.Pmodule *)
(* module Hid = Why3.Ident.Hid *)
(* module Extract = Why3.Pdriver *)

(*
type kind = [ `Type | `Logic | `Program | `Axiom ]
type role = { kind : kind ; builtin : bool ; extern : bool }
*)

(* -------------------------------------------------------------------------- *)
(* --- Builtin & Extracted Environments                                   --- *)
(* -------------------------------------------------------------------------- *)

type henv = {
  builtins : Sid.t ;
  extracted : Sid.t ;
}

let ocaml64 =
  Filename.concat Why3.Config.datadir @@
  Filename.concat "drivers" "ocaml64.drv"

let init (wenv : Wenv.env) =
  let provers = Runner.select wenv @@ Wenv.provers () in
  let builtins = List.fold_left
      (fun s Runner.{ driver } ->
         Sid.union s @@ Mid.domain @@ Why3.Driver.syntax_map driver)
      Sid.empty provers
  in
  let drivers = List.concat_map (fun pkg -> pkg.Meta.drivers) wenv.pkgs in
  let pdriver = Why3.Pdriver.load_driver wenv.wenv ocaml64 drivers in
  let extracted = Mid.domain @@ pdriver.drv_syntax in
  { builtins ; extracted }

(*
type axioms = {
  locals : role Mid.t ;
  theories : Sid.t ;
}

let axioms = Hid.create 0

let assumed { builtin ; extern } = not builtin && not extern
*)

(* -------------------------------------------------------------------------- *)
