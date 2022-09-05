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
(* --- Prover Calibration                                                 --- *)
(* -------------------------------------------------------------------------- *)

open Why3

let ty = Ty.create_tysymbol (Ident.id_fresh "t") [] Ty.NoDef
let t = Ty.ty_app ty []
let f = Term.create_fsymbol (Ident.id_fresh "f") [t;t] t
let vx = Term.create_vsymbol (Ident.id_fresh "x") t
let vy = Term.create_vsymbol (Ident.id_fresh "y") t
let vz = Term.create_vsymbol (Ident.id_fresh "z") t
let x = Term.t_var vx
let y = Term.t_var vy
let z = Term.t_var vz
let eq = Term.t_equ
let op x y = Term.fs_app f [x;y] t
let forall xs t = Term.t_forall @@ Term.t_close_quant xs [] t
let assoc = Decl.create_prsymbol (Ident.id_fresh "assoc")
let passoc = forall [vx;vy;vz] @@ eq (op x (op y z)) (op (op x y) z)
let hmap = Hashtbl.create 0
let vx i =
  try Hashtbl.find hmap i
  with Not_found ->
    let v = Term.create_vsymbol (Ident.id_fresh "e") t in
    Hashtbl.add hmap i v ; v
let x i = Term.t_var (vx i)
let rec vars xs n = if n > 0 then vars (vx n :: xs) (n-1) else xs
let rec left i n =
  if i < n then op (x i) (left (succ i) n)
  else x i
let rec right i n =
  if i > 1 then op (right (pred i) n) (x i)
  else x i
let goal = Decl.create_prsymbol (Ident.id_fresh "goal")
let pgoal n = forall (vars [] n) (eq (left 1 n) (right n n))

let generate n =
  let open Why3 in
  let task : Task.task ref = ref None in
  let declare d = task := Task.add_decl !task d in
  begin
    declare @@ Decl.create_ty_decl ty ;
    declare @@ Decl.create_param_decl f ;
    declare @@ Decl.create_prop_decl Paxiom assoc passoc ;
    declare @@ Decl.create_prop_decl Plemma goal (pgoal n) ;
  end ;
  !task

(* -------------------------------------------------------------------------- *)
