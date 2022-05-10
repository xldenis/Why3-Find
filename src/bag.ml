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
(* --- Bags                                                               --- *)
(* -------------------------------------------------------------------------- *)

type 'a bag =
  | Empty
  | Elt of 'a
  | Cat of int * 'a * 'a bag * 'a bag

let empty = Empty
let elt x = Elt x
let size = function Empty -> 0 | Elt _ -> 1 | Cat(s,_,_,_) -> s

let (++) a b =
  match a,b with
  | Empty,c | c,Empty -> c
  | Elt x,_ -> Cat(1 + size b,x,a,b)
  | Cat(s,x,_,_),_ -> Cat(s + size b,x,a,b)

let (+>) a x = let e = Elt x in if a = Empty then e else Cat(1 + size a,x,a,e)
let (@<) x a = let e = Elt x in if a = Empty then e else Cat(1 + size a,x,e,a)
let (+=) acc x = acc := !acc +> x

let map f xs = List.fold_left (fun acc x -> acc +> f x) empty xs
let merge f xs = List.fold_left (fun acc x -> acc ++ f x) empty xs

let rec iter f = function
  | Empty -> ()
  | Elt x -> f x
  | Cat(_,_,a,b) -> iter f a ; iter f b

let rec of_list = function
  | [] -> empty
  | x::xs -> x @< of_list xs

let to_list a =
  let rec walk acc = function
    | Empty -> acc
    | Elt x -> x::acc
    | Cat(_,_,a,b) -> walk (walk acc b) a
  in walk [] a

let to_array = function
  | Empty -> [| |]
  | Elt x -> [| x |]
  | Cat(n,x,a,b) ->
    let m = Array.make n x in
    let rec set m k = function
      | Empty -> k
      | Elt x -> m.(k) <- x ; succ k
      | Cat(_,_,a,b) -> set m (set m k a) b
    in let _ = set m (set m 0 a) b in m

(* -------------------------------------------------------------------------- *)
