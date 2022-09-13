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

let stuck = function
  | Stuck -> 1
  | Prover _ -> 0
  | Transf { stuck } -> stuck

let proved = function
  | Stuck -> 0
  | Prover _ -> 1
  | Transf { proved } -> proved

let complete = function
  | Stuck -> false
  | Prover _ -> true
  | Transf { stuck } -> stuck = 0

let unknown = function
  | Stuck -> true
  | Prover _ -> false
  | Transf { stuck ; proved } -> stuck > 0 && proved = 0

let apply id children =
  let stuck = List.fold_left (fun n c -> n + stuck c) 0 children in
  let proved = List.fold_left (fun n c -> n + proved c) 0 children in
  if stuck > 0 && proved = 0 then Stuck else
    Transf { id ; children ; stuck ; proved }

let pretty fmt crc =
  let s = stuck crc in
  let p = proved crc in
  if s = 0 then Format.fprintf fmt "@{<green>Valid@} (%d)" p else
  if p = 0 then Format.fprintf fmt "@{<red>Unknown@}" else
    Format.fprintf fmt "@{<orange>Partial@} (%d/%d)" p (s+p)

(* -------------------------------------------------------------------------- *)
(* --- JSON                                                               --- *)
(* -------------------------------------------------------------------------- *)

let rec to_json (a : crc) : Json.t = match a with
  | Stuck -> `Null
  | Prover(p,t) ->
    `Assoc [ "prover", `String p ; "time", `Float t]
  | Transf { id ; children } ->
    `Assoc [
      "transf", `String id;
      "children", `List (List.map to_json children);
    ]

let rec of_json (js : Json.t) : crc =
  try
    match js with
    | `Assoc fds when List.mem_assoc "prover" fds ->
      let p = List.assoc "prover" fds |> Json.jstring in
      let t = List.assoc "time" fds |> Json.jfloat in
      Prover(p,t)
    | `Assoc fds when List.mem_assoc "transf" fds ->
      let f = List.assoc "transf" fds |> Json.jstring in
      let xs = List.assoc "children" fds |> Json.jlist in
      apply f (List.map of_json xs)
    | _ -> Stuck
  with _ -> Stuck

(* -------------------------------------------------------------------------- *)
(* --- Merging                                                            --- *)
(* -------------------------------------------------------------------------- *)

let rec merge a b =
  match a, b with
  | Prover(p0,t0), Prover(p1,t1)
    when p0 = p1 && t0 *. 0.5 < t1 && t1 < t0 *. 2.0 -> a
  | Transf { id = f ; children = xs } ,
    Transf { id = g ; children = ys }
    when f = g && List.length xs = List.length ys ->
    apply f (List.map2 merge xs ys)
  | _ -> b

(* -------------------------------------------------------------------------- *)
(* --- Dump                                                               --- *)
(* -------------------------------------------------------------------------- *)

let pname = Hashtbl.create 0
let shortname p =
  try Hashtbl.find pname p
  with Not_found ->
    let s = String.lowercase_ascii @@ List.hd @@ String.split_on_char ',' p in
    Hashtbl.add pname p s ; s

let rec dump fmt = function
  | Stuck -> Format.fprintf fmt "@ @{<orange>Unknown@}"
  | Prover(p,t) -> Format.fprintf fmt "@ %s (%a)" (shortname p) Utils.pp_time t
  | Transf { id ; children } ->
    Format.fprintf fmt "@ @[<hov 2>+ %s" id ;
    List.iter (dump fmt) children ;
    Format.fprintf fmt "@]"

(* -------------------------------------------------------------------------- *)
