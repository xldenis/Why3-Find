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

(* -------------------------------------------------------------------------- *)
(* --- Calibration Problems                                               --- *)
(* -------------------------------------------------------------------------- *)

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

open Prover
open Runner
open Fibers.Monad

type range = Guess of int * int | Range of int * int
let guesses = [ "alt-ergo", 16 ; "z3", 32 ; "cvc4", 40 ; "cvc5", 40 ]
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

let pp_range fmt = function
  | Guess(p,q) -> Format.fprintf fmt "%d…%d?" p q
  | Range(p,q) -> Format.fprintf fmt "%d…%d" p q
[@@ warning "-32"]

type qenv = {
  env : Wenv.env ;
  time : float ;
  time_lo : float ;
  time_up : float ;
  time_out : float ;
}

let qenv env time = {
  env ; time ;
  time_lo = time *. 0.9 ;
  time_up = time *. 1.1 ;
  time_out = time *. 2.0
}

let rec seqlookup ~progress q prv rg best =
  if singleton rg then Fibers.return best else
    let n = select rg in
    let name =
      if progress then
        ( Utils.progress "%s (%d)" (name prv) n ; None )
      else Some (name prv)
    in
    let* result = Runner.prove q.env ?name prv (generate n) q.time_out in
    match result with
    | Valid t ->
      let best = choose q.time best (Some (n,t)) in
      if t < q.time_lo then seqlookup ~progress q prv (upper n rg) best else
      if q.time_up < t then seqlookup ~progress q prv (lower n rg) best else
        Fibers.return best
    | _ ->
      seqlookup ~progress q prv (lower n rg) best

(* -------------------------------------------------------------------------- *)
(* --- Velocity Parallel Lookup                                           --- *)
(* -------------------------------------------------------------------------- *)

type cenv = {
  qenv : qenv ;
  prover : prover ;
  progress : bool ;
  ca : unit Fibers.signal ;
  cg : unit Fibers.signal ;
  cb : unit Fibers.signal ;
  mutable inf : int ;
  mutable sup : int ;
  mutable guess : int ;
  mutable best : (int * float) option ;
}

let fire cs = List.iter (fun s -> Fibers.emit s ()) cs

let pguess ~progress q prv =
  let g = try List.assoc (name prv) guesses with Not_found -> 20 in
  { progress ; prover = prv ; qenv = q ;
    ca = Fibers.signal () ;
    cg = Fibers.signal () ;
    cb = Fibers.signal () ;
    inf = 1 ; sup = max_int ; guess = g ; best = None }

let ptry c ~cancel ?(upper=[]) ?(lower=[]) n =
  if n = 0 then Fibers.return false
  else
    let prv = c.prover in
    let name =
      if c.progress then
        ( Utils.progress "%s (%d)" (name prv) n ; None )
      else Some (name prv)
    in
    let q = c.qenv in
    let+ r = Runner.prove q.env ?name ~cancel prv (generate n) q.time_out in
    match r with
    | Valid t ->
      c.best <- choose q.time c.best (Some (n,t)) ;
      if t < q.time_lo then (fire lower ; c.inf <- max c.inf n ; false) else
      if q.time_up < t then (fire upper ; c.sup <- min c.sup n ; false) else
        true
    | Timeout t | Unknown t ->
      if q.time_up < t then ( fire upper ; c.sup <- min c.sup n ) ;
      false
    | Failed | NoResult -> false

let middle a b = (a+b)/2

let rec parlookup p =
  let { inf = a ; sup = b ; guess = g } = p in
  match b - a with
  | 0 | 1 -> Fibers.return p.best
  | 2 -> spawn p 0 g 0
  | 3 -> spawn p 0 g (g+1)
  | 4 -> spawn p (g-1) g (g+1)
  | _ ->
    let a = middle a g in
    let b = if b = max_int then g * 2 else (middle g b) in
    spawn p a g b

and spawn p na ng nb =
  let { inf = a0 ; sup = b0 ; ca ; cg ; cb } = p in
  let pa = ptry p ~cancel:ca ~upper:[cg;cb] na in
  let pg = ptry p ~cancel:cg ~lower:[ca] ~upper:[cb] ng in
  let pb = ptry p ~cancel:cb ~lower:[cg;ca] nb in
  let* found = Fibers.all [ pg ; pb ; pa ] in
  if List.exists Fun.id found then
    Fibers.return p.best
  else
    let { inf = a ; sup = b } = p in
    if b-a < b0-a0 then
      begin
        p.guess <- if b < max_int then (a+b)/2 else max a (p.guess * 2) ;
        parlookup p
      end
    else
      Fibers.return p.best

(* -------------------------------------------------------------------------- *)
(* --- Problem Lookup                                                     --- *)
(* -------------------------------------------------------------------------- *)

let reftime = ref 0.5
let sequential = ref false

let lookup ~progress q prv =
  if !sequential then
    seqlookup ~progress q prv (guess prv) None
  else
    parlookup (pguess ~progress q prv)

(* -------------------------------------------------------------------------- *)
(* --- On-the-fly Calibration                                             --- *)
(* -------------------------------------------------------------------------- *)

let qhash = Hashtbl.create 0

let calibrate ?(progress=false) env prv : (int * float) option Fibers.t =
  try Hashtbl.find qhash prv.desc
  with Not_found ->
    let p = Fibers.var () in
    let q = qenv env !reftime in
    Fibers.background ~callback:(Fibers.set p) (lookup ~progress q prv) ;
    Hashtbl.add qhash prv.desc p ; p

(* -------------------------------------------------------------------------- *)
(* --- Profile                                                            --- *)
(* -------------------------------------------------------------------------- *)

type gauge = {
  size : int ;
  time : float ;
  mutable alpha : float Fibers.t option ;
}

type profile = (Prover.prover_desc,gauge Fibers.t) Hashtbl.t

let create () = Hashtbl.create 0

let of_json ?default (js : Json.t) : profile =
  let open Json in
  let p = match default with Some p -> p | None -> create () in
  List.iter (fun js ->
      try
        let prv = jfield_exn "prover" js |> jstring in
        let size = jfield_exn "size" js |> jint in
        let time = jfield_exn "time" js |> jfloat in
        let gauge = Fibers.var ~init:{ size ; time ; alpha = None } () in
        Hashtbl.replace p (Prover.desc_of_string prv) gauge
      with _ -> ()
    ) (jlist js) ; p

let to_json (p : profile) : Json.t =
  `List (Hashtbl.fold (fun prv gv js ->
      match Fibers.get gv with
      | None -> js
      | Some { size ; time } ->
        (`Assoc [
            "prover", `String (Prover.desc_to_string prv) ;
            "size", `Int size ;
            "time", `Float time ;
          ]) :: js
    ) p [])

(* -------------------------------------------------------------------------- *)
(* --- Velocity                                                           --- *)
(* -------------------------------------------------------------------------- *)

let gauge env profile prv : gauge Fibers.t =
  let p = prv.desc in
  try Hashtbl.find profile p
  with Not_found ->
    let gv =
      let+ r = calibrate env prv in
      match r with
      | Some (size,time) -> { time ; size ; alpha = None }
      | None ->
        Log.error "can not calibrate prover %a"
          Prover.pp_desc p ;
        exit 2
    in Hashtbl.replace profile p gv ; gv

let profile env profile prv =
  let+ { size ; time } = gauge env profile prv in (size,time)

let alpha env prv ~size ~time : float Fibers.t =
  let timeout = 5.0 *. (max 1.0 time) in
  let+ result = Runner.prove env
      ~name:(Prover.name prv)
      prv (generate size) timeout in
  match result with
  | Valid t -> t /. time
  | _ ->
    Log.error "can not calibrate prover %a (N=%d, %a)"
      Prover.pp_prover prv size Runner.pp_result result ;
    exit 2

let local = ref false

let velocity env profile prv : float Fibers.t =
  if !local then Fibers.return 1.0 else
    let* g = gauge env profile prv in
    match g.alpha with
    | Some a -> a
    | None ->
      let ga = alpha env prv ~size:g.size ~time:g.time in
      g.alpha <- Some ga ; ga

let observed profile prv =
  try
    match Hashtbl.find profile prv.desc |> Fibers.find with
    | { alpha = Some ga } -> Fibers.find ga
    | _ -> 1.0
  with Not_found -> 1.0

let mem = Hashtbl.mem

let set (profile: profile) prv size time =
  let init = { size ; time ; alpha = None } in
  try
    Fibers.set (Hashtbl.find profile prv) init
  with Not_found ->
    Hashtbl.add profile prv @@ Fibers.var ~init ()

let get (profile: profile) prv =
  try
    let { size ; time } = Fibers.find @@ Hashtbl.find profile prv
    in Some(size,time)
  with Not_found -> None

let lock (profile: profile) prv =
  if not @@ Hashtbl.mem profile prv then
    ( Hashtbl.add profile prv @@ Fibers.var () ; true )
  else false

let iter f profile =
  List.iter (fun (p,n,t) -> f p n t) @@
  List.sort (fun (p,_,_) (q,_,_) -> Prover.compare_desc p q) @@
  Hashtbl.fold (fun p g w ->
      match Fibers.get g with
      | None -> w
      | Some g -> (p,g.size,g.time)::w
    ) profile []

(* -------------------------------------------------------------------------- *)
(* --- Calibration                                                        --- *)
(* -------------------------------------------------------------------------- *)

let default () =
  if !local then create () else
    try Wenv.get "profile" ~of_json:(of_json ?default:None)
    with Not_found -> create ()

let calibrate_provers ~saved env provers =
  Fibers.await @@
  begin
    let q = qenv env !reftime in
    let* results =
      Fibers.all @@ List.map
        (fun prv ->
           let+ r = lookup ~progress:true q prv in prv, r
        ) provers
    in
    Utils.flush () ;
    let profile = create () in
    Format.printf "Calibration:@." ;
    List.iter
      (fun (prv,res) ->
         let name = fullname prv in
         match res with
         | None ->
           Format.printf " - %-16s no result@." name
         | Some(n,t) ->
           Hashtbl.add profile prv.desc
             (Fibers.var ~init:{ size = n ; time = t ; alpha = None } ()) ;
           Format.printf " - %-16s n=%d %a (%s)@." name n Utils.pp_time t
             (if saved then "master" else "locally")
      ) results ;
    Wenv.set "profile" ~to_json profile ;
    Fibers.return () ;
  end

let velocity_provers env provers =
  Fibers.await @@
  begin
    let profile = default () in
    let* results =
      Fibers.all @@ List.map
        (fun prv ->
           try
             let g = Fibers.find @@ Hashtbl.find profile prv.desc in
             let+ a = velocity env profile prv in
             prv , Some(g.size,g.time,a)
           with Not_found ->
             Fibers.return (prv , None)
        ) provers
    in
    Utils.flush () ;
    Format.printf "Velocity:@." ;
    List.iter
      (fun (prv,res) ->
         let name = fullname prv in
         match res with
         | Some(n,t,1.0) ->
           Format.printf " - %-16s n=%d %a (master)@." name n Utils.pp_time t
         | Some(n,tm,a) ->
           let tl = tm *. a in
           Format.printf " - %-16s n=%d %a (velocity %.1f)@."
             name n Utils.pp_time tl a
         | None ->
           Format.printf " - %-16s no calibration (use -m)@." name
      ) results ;
    Fibers.return () ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Options                                                            --- *)
(* -------------------------------------------------------------------------- *)

let options = [
  "--local", Arg.Set local, "no calibration (use local times)";
  "--reftime", Arg.Set_float reftime, "TIME set calibration time (default 0.5s)" ;
  "--sequential", Arg.Set sequential, "use sequential calibration algorithm" ;
]

(* -------------------------------------------------------------------------- *)
