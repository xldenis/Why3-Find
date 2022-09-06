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
  let task = ref (Task.use_export None Theory.builtin_theory) in
  let declare d = task := Task.add_decl !task d in
  begin
    declare @@ Decl.create_ty_decl ty ;
    declare @@ Decl.create_param_decl f ;
    declare @@ Decl.create_prop_decl Paxiom assoc passoc ;
    declare @@ Decl.create_prop_decl Pgoal goal (pgoal n) ;
  end ;
  !task

(* -------------------------------------------------------------------------- *)
(* --- Velocity Lookup                                                    --- *)
(* -------------------------------------------------------------------------- *)
open Runner
open Fibers.Monad

let velocity env timeout prv n =
  let cancel = Fibers.signal () in
  Runner.prove env cancel (generate n) prv timeout

type range = Guess of int | Range of int * int
let guesslist = [ "alt-ergo", 16 ; "z3", 30 ; "cvc4", 40 ]
let guess prv = Guess (try List.assoc (name prv) guesslist with Not_found -> 20)
let singleton = function Guess _ -> false | Range(p,q) -> q <= p+1
let select = function Guess n -> n | Range(p,q) -> (p+q)/2
let upper s = function Guess _ -> Guess (2*s) | Range(_,q) -> Range(s,q)
let lower s = function Guess _ -> Range(1,s) | Range(p,_) -> Range(p,s)

let rec lookup env time prv rg best =
  if singleton rg then Fibers.return best else
    let n = select rg in
    Format.printf "[%s] %d…\r@?" (name prv) n ;
    let* r = velocity env (time *. 2.0) prv n in
    match r with
    | Valid t ->
      let best = Some (n,t) in
      if t <= time *. 0.75 then lookup env time prv (upper n rg) best else
      if time *. 1.25 <= t then lookup env time prv (lower n rg) best else
        Fibers.return best
    | _ ->
      lookup env time prv (lower n rg) best

(* -------------------------------------------------------------------------- *)
(* --- Calibration Processing                                             --- *)
(* -------------------------------------------------------------------------- *)

let calibrate ~force ~master ~time provers =
  begin
    let env = Wenv.init ~pkgs:[] in
    let provers = Runner.select env provers in
    let timeout = float time *. 1e-3 in
    List.iter
      (fun prv ->
         Format.printf "Calibrating %a@." pp_prover prv ;
         Fibers.await (lookup env timeout prv (guess prv) None)
           begin function
             | None ->
               Format.printf "[%a] no result@." pp_prover prv
             | Some(n,t) ->
               Format.printf "[%a] N=%d %a@." pp_prover prv n pp_time t
           end
      ) provers ;
    Fibers.flush ~polling:20 () ;
    Format.printf "Finished.@." ;
    ignore force ;
    ignore master ;
  end

(* -------------------------------------------------------------------------- *)
