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
  | Transf of bool * int * string * crc list

let complete = function
  | Stuck -> false
  | Prover _ -> true
  | Transf(b,_,_,_) -> b

let depth = function
  | Stuck -> 0
  | Prover _ -> 1
  | Transf(_,n,_,_) -> n

let apply f cs =
  let b = List.for_all complete cs in
  let n = List.fold_left (fun h c -> max h (depth c)) 0 cs in
  Transf(b,n,f,cs)

let rec update a b =
  match a,b with
  | Prover(p0,t0), Prover(p1,t1)
    when p0 = p1 && t0 *. 0.5 < t1 && t1 < t0 *. 2.0 -> a
  | Transf(_,_,f,xs), Transf(_,_,g,ys)
    when f = g && List.length xs = List.length ys ->
    apply f (List.map2 update xs ys)
  | _ -> b
