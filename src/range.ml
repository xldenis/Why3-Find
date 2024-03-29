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
(* --- Position & Ranges                                                  --- *)
(* -------------------------------------------------------------------------- *)

type pos = int * int

let pp_pos fmt (l,c) = Format.fprintf fmt "line %d, character %d" l (succ c)

let (<<) (l1,c1) (l2,c2) = l1 < l2 || (l1 = l2 && c1 < c2)
let (>>) (l1,c1) (l2,c2) = l1 > l2 || (l1 = l2 && c1 > c2)
let (<<=) (l1,c1) (l2,c2) = l1 < l2 || (l1 = l2 && c1 < c2)

let compare_pos (l1,c1) (l2,c2) =
  if l1 < l2 then (-1) else
  if l1 > l2 then (+1) else
    c1 - c2

let compare_range (a,b) (c,d) =
  let cmp = compare_pos a c in
  if cmp <> 0 then cmp else compare_pos b d

let start = (1,0)
let next (l,c) = (l,succ c)
let prev (l,c) = (l,pred c)
let newline (l,_) = (succ l,0)
let after p = function '\n' -> newline p | _ -> next p

let min p q = if p <<= q then p else q
let max p q = if p <<= q then q else p

type range = pos * pos

let pp_range fmt ((l,c),(l',d)) =
  if l = l' then
    Format.fprintf fmt "line %d, characters %d-%d" l (succ c) (succ d)
  else
    Format.fprintf fmt "lines %d-%d" l l'

let pp_position fmt ~file r =
  Format.fprintf fmt "file %S, %a" file pp_range r

let is_empty (a,b) = b << a

let inside p (a,b) = a <<= p && p <<= b

let disjoint (a,b) (c,d) = b << c || d << a
let (<<<) (_,b) (c,_) = b << c
let union (a,b) (c,d) = min a c, max b d
let diff (a,b) (c,d) = max a d, min b c
