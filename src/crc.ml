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
      todo : int ;
      size : int ;
    }

let size = function
  | Stuck | Prover _ -> 1
  | Transf { size } -> 1 + size

let todo = function
  | Stuck -> 1
  | Prover _ -> 0
  | Transf { todo } -> todo

let proved = function
  | Stuck -> 0
  | Prover _ -> 1
  | Transf { todo ; size } -> 1 + size - todo

let complete = function
  | Stuck -> false
  | Prover _ -> true
  | Transf { todo } -> todo = 0

let apply id children =
  let todo = List.fold_left (fun n c -> n + todo c) 0 children in
  let size = List.fold_left (fun n c -> n + size c) 1 children in
  Transf { id ; children ; todo ; size }

let pretty fmt crc =
  let n = size crc in
  let p = proved crc in
  if p = 0 then Format.fprintf fmt "@{<red>Unknown@}" else
  if p = n then Format.fprintf fmt "@{<green>Valid@}" else
    Format.fprintf fmt "@{<orange>Partial@} (%d/%d)" p n

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
  match a,b with
  | Prover(p0,t0), Prover(p1,t1)
    when p0 = p1 && t0 *. 0.5 < t1 && t1 < t0 *. 2.0 -> a
  | Transf { id = f ; children = xs } ,
    Transf { id = g ; children = ys }
    when f = g && List.length xs = List.length ys ->
    apply f (List.map2 merge xs ys)
  | _ -> b

(* -------------------------------------------------------------------------- *)
