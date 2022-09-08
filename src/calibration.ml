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

type range = Guess of int * int | Range of int * int
let guesses = [ "alt-ergo", 16 ; "z3", 32 ; "cvc4", 40 ]
let guess prv = Guess (1,try List.assoc (name prv) guesses with Not_found -> 20)
let singleton = function Guess _ -> false | Range(p,q) -> q <= p+1
let select = function Guess(_,n) -> n | Range(p,q) -> (p+q)/2
let upper s = function Guess _ -> Guess(s,2*s) | Range(_,q) -> Range(s,q)
let lower s = function Guess(p,_) -> Range(p,s) | Range(p,_) -> Range(p,s)
let choose t x y =
  match x,y with
  | None,z | z,None -> z
  | Some(_,tx), Some(_,ty) ->
    if Float.abs (t -. tx) < Float.abs (t -. ty) then x else y

[@@@ warning "-32"]
let pp_range fmt = function
  | Guess(p,q) -> Format.fprintf fmt "%d…%d?" p q
  | Range(p,q) -> Format.fprintf fmt "%d…%d" p q
[@@@ warning "+32"]

type qenv = {
  env : Wenv.env ;
  cancel : unit Fibers.signal ;
  time : float ;
  time_lo : float ;
  time_up : float ;
  time_out : float ;
}

let qenv env time = {
  env ; time ;
  cancel = Fibers.signal () ;
  time_lo = time *. 0.9 ;
  time_up = time *. 1.1 ;
  time_out = time *. 2.0
}

let rec lookup q prv rg best =
  if singleton rg then Fibers.return best else
    let n = select rg in
    Format.printf "> %s:%d\x1B[K\r@?" (name prv) n ;
    let* result = Runner.prove q.env q.cancel (generate n) prv q.time_out in
    match result with
    | Valid t ->
      let best = choose q.time best (Some (n,t)) in
      if t < q.time_lo then lookup q prv (upper n rg) best else
      if q.time_up < t then lookup q prv (lower n rg) best else
        Fibers.return best
    | _ ->
      lookup q prv (lower n rg) best

(* -------------------------------------------------------------------------- *)
(* --- On-the-fly Calibration                                             --- *)
(* -------------------------------------------------------------------------- *)

let qhash = Hashtbl.create 0

let calibrate env prv : (int * float) option Fibers.t =
  try Hashtbl.find qhash (id prv)
  with Not_found ->
    let q = qenv env 0.5 in
    let p = lookup q prv (guess prv) None in
    Hashtbl.add qhash (id prv) p ; p

(* -------------------------------------------------------------------------- *)
(* --- Profile                                                            --- *)
(* -------------------------------------------------------------------------- *)

type gauge = {
  size : int ;
  time : float ;
  mutable alpha : float ;
}

type profile = (string,gauge) Hashtbl.t

let of_json (js : Json.t) : profile =
  let open Json in
  let p = Hashtbl.create 0 in
  List.iter (fun js ->
      try
        let prv = jfield_exn "prover" js |> jstring in
        let size = jfield_exn "size" js |> jint in
        let time = jfield_exn "time" js |> jfloat in
        Hashtbl.replace p prv { size ; time ; alpha = 0.0 }
      with _ -> ()
    ) (jlist js) ; p

let to_json (p : profile) : Json.t =
  `List (Hashtbl.fold (fun prv { size ; time } js ->
      (`Assoc [
        "prover", `String prv ;
        "size", `Int size ;
        "time", `Float time ;
      ]) :: js
    ) p [])

(* -------------------------------------------------------------------------- *)
(* --- Velocity                                                           --- *)
(* -------------------------------------------------------------------------- *)

let gauge env profile prv : gauge Fibers.t =
  let p = id prv in
  try Fibers.return @@ Hashtbl.find profile p
  with Not_found ->
    let+ r = calibrate env prv in
    match r with
    | Some (size,time) ->
      let g = { time ; size ; alpha = 1.0 } in
      Hashtbl.add profile p g ; g
    | None ->
      Format.eprintf "[Error] can not calibrate prover %s@." p ;
      exit 2

let velocity env profile prv : float Fibers.t =
  let* g = gauge env profile prv in
  if g.alpha > 0.0 then Fibers.return g.alpha else
    let cancel = Fibers.signal () in
    let timeout = 5.0 *. (max 1.0 g.time) in
    Format.printf "> %s:%d\x1B[K\r@?" (name prv) g.size ;
    let* result = Runner.prove env cancel (generate g.size) prv timeout in
    match result with
    | Valid t -> let a = t /. g.time in g.alpha <- a ; Fibers.return a
    | _ ->
      Format.eprintf "[Error] can not calibrate prover %a (N=%d, %a)@."
        Runner.pp_prover prv g.size Runner.pp_result result ;
      exit 2

(* -------------------------------------------------------------------------- *)
(* --- Testing Calibration                                                --- *)
(* -------------------------------------------------------------------------- *)

let calibrate_provers ~time provers =
  Fibers.run @@
  begin
    let env = Wenv.init ~pkgs:[] in
    let time = float time *. 1e-3 in
    let provers = Runner.select env provers in
    let q = qenv env time in
    let* results =
      Fibers.all @@ List.map
        (fun prv ->
           let+ r = lookup q prv (guess prv) None in
           prv, r
        ) provers
    in
    List.iter
      (fun (prv,res) ->
         match res with
         | None ->
           Format.printf "%-16s no result@." (id prv)
         | Some(n,t) ->
           Format.printf "%-16s n=%d %a@." (id prv) n pp_time t
      ) @@
    List.sort (fun (p,_) (q,_) -> String.compare (id p) (id q)) results ;
    Runner.report_stats () ;
    Fibers.return () ;
  end

(* -------------------------------------------------------------------------- *)
