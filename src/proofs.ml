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

module M = Why3.Wstdlib.Mstr
module Th = Why3.Theory

type profile = Calibration.profile
type theories = Crc.crc M.t M.t

(* -------------------------------------------------------------------------- *)
(* --- Proof Files                                                        --- *)
(* -------------------------------------------------------------------------- *)

let jmap cc js =
  let m = ref M.empty in
  Json.jiter (fun fd js -> m := M.add fd (cc js) !m) js ; !m

let jproofs proofs : Json.t =
  let folder th rs acc =
    (th, `Assoc (List.map (fun (g,r) -> g, Crc.to_json r) rs)) :: acc in
  `Assoc (M.fold folder proofs [])

let proofs_file file =
  Filename.concat (Filename.chop_extension file) "proof.json"

let load_proofs ?(local=false) file : profile * theories =
  let file = proofs_file file in
  let js = if Sys.file_exists file then Json.of_file file else `Null in
  let default = if local then Some (Calibration.default ()) else None in
  let profile = Calibration.of_json ?default @@ Json.jfield "profile" js in
  let strategy = jmap (jmap Crc.of_json) @@ Json.jfield "proofs" js in
  profile , strategy

type dumper = {
  file : string;
  profile : Calibration.profile;
  mutable proofs : (string * Crc.crc) list M.t;
}

let save_proofs { file; profile; proofs } =
  let file = proofs_file file in
  Utils.mkdirs (Filename.dirname file) ;
  Json.to_file file @@ `Assoc [
    "profile", Calibration.to_json profile ;
    "proofs", jproofs proofs ;
  ]

let create file profile =
  { file; profile; proofs = M.empty; }

let dump dumper =
  save_proofs dumper

let add theory goal crc dumper =
  let changer o =
    Some ((goal, crc) :: Option.value ~default:[] o) in
  dumper.proofs <- M.change changer (Session.name theory) dumper.proofs;
  dump dumper
