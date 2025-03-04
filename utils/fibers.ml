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
(* --- Fibers                                                             --- *)
(* -------------------------------------------------------------------------- *)

type 'a t = 'a state ref
and 'a state = Done of 'a | Wait of ('a -> unit) list

let var ?init () = ref @@ match init with None -> Wait [] | Some v -> Done v
let defined th = match !th with Done _ -> true | Wait _ -> false
let get th = match !th with Done v -> Some v | Wait _ -> None
let find th = match !th with Done v -> v | Wait _ -> raise Not_found
let return v = ref (Done v)
let forward th f = match !th with Done v -> f v | Wait qs -> th := Wait (f::qs)

let rec notify v = function [] -> () | f::fs -> f v ; notify v fs

let set th v = match !th with Done _ -> () | Wait qs ->
  th := Done v ; notify v (List.rev qs)

let bind ta fb = match !ta with Done v -> fb v | Wait qs ->
  let r = var () in
  let fa v = forward (fb v) (set r) in
  ta := Wait (fa :: qs) ; r

let map f ta = match !ta with Done v -> return (f v) | Wait qs ->
  let r = var () in
  let fa v = set r (f v) in
  ta := Wait (fa :: qs) ; r

let apply f th = map th f

let par a b =
  let joined = var () in
  let x = ref None in
  let y = ref None in
  let join () =
    match !x,!y with
    | Some u,Some v -> set joined (u,v)
    | _ -> ()
  in
  begin
    forward a (fun u -> x := Some u ; join ()) ;
    forward b (fun v -> y := Some v ; join ()) ;
    joined
  end

module Monad =
struct
  let (let*) = bind
  let (let+) = apply
  let (and*) = par
  let (@+) = map
  let (@*) = par
end
open Monad

(* -------------------------------------------------------------------------- *)
(* --- Iterable Queues                                                    --- *)
(* -------------------------------------------------------------------------- *)

module Queue :
sig
  type 'a t
  val create : unit -> 'a t
  val length : 'a t -> int
  val push : 'a t -> 'a -> unit
  val pop : 'a t -> 'a
  val iter : 'a t -> ('a -> unit) -> unit
  val filter : 'a t -> ('a -> bool) -> unit
  val clear : 'a t -> unit
end =
struct

  type 'a t = {
    mutable size : int ;
    mutable head : 'a list ;
    mutable tail : 'a list ;
  }

  let create () = { size = 0 ; head = [] ; tail = [] }

  let length q =
    let n = q.size in
    if 0 <= n then n
    else
      let n = List.length q.head + List.length q.tail in
      q.size <- n ; n

  let push q v =
    if q.size >= 0 then q.size <- succ q.size ;
    q.tail <- v::q.tail

  let pop q =
    let unroll =
      if q.head <> [] then q.head else
        let tl = q.tail in q.tail <- [] ; List.rev tl
    in match unroll with
    | [] -> raise Not_found
    | h::hd ->
      if q.size > 0 then q.size <- pred q.size ;
      q.head <- hd ; h

  let flatten q =
    match q.tail with
    | [] -> q.head
    | tl ->
      q.tail <- [] ;
      let xs = q.head @ List.rev tl in q.head <- xs ; xs

  let filter q f =
    q.size <- (-1) ;
    q.head <- List.filter f @@ flatten q

  let iter q f =
    List.iter f @@ flatten q

  let clear q =
    begin
      q.size <- 0 ;
      q.head <- [] ;
      q.tail <- [] ;
    end

end

(* -------------------------------------------------------------------------- *)
(* --- Signals                                                            --- *)
(* -------------------------------------------------------------------------- *)

type 'a signal = ('a -> unit) Queue.t

let signal = Queue.create
let emit s v = Queue.iter s (fun f -> f v)
let on = Queue.push
let off q k = Queue.filter q (fun k0 -> k0 != k)
let once q k = let rec fn v = k v ; off q fn in on q fn
let clear = Queue.clear
let connected s = Queue.length s > 0
let disconnect = Queue.clear

(* -------------------------------------------------------------------------- *)
(* --- List Combinators                                                   --- *)
(* -------------------------------------------------------------------------- *)

let any ts =
  let r = var () in
  List.iter (fun th -> forward th @@ set r) ts ; r

let all = function
  | [] -> return []
  | ts ->
    let xs = ref [] in
    let n = ref (List.length ts) in
    let rs = var () in
    let recv i v =
      xs := (i,v) :: !xs ;
      decr n ;
      if !n <= 0 then
        let ys = List.sort (fun (i,_) (j,_) -> Stdlib.compare i j) !xs in
        set rs @@ List.map snd ys
    in
    let rec schedule i = function
      | [] -> ()
      | t::ts -> forward t @@ recv i ; schedule (succ i) ts
    in schedule 0 ts ; rs

let rec seq ts =
  match ts with
  | [] -> return []
  | t :: ts ->
    let* r = t in
    let* rs = seq ts in
    return (r::rs)

let first f ts =
  if ts = [] then return None else
    let r = var () in
    let count = ref 0 in
    let update x =
      decr count ;
      match f x with
      | None -> if !count <= 0 then set r None
      | Some _ as m -> set r m
    in
    List.iter
      (fun t ->
         incr count ;
         forward t update
      ) ts ; r

(* -------------------------------------------------------------------------- *)
(* --- Asynchronous Tasks                                                 --- *)
(* -------------------------------------------------------------------------- *)

let queue : (unit -> bool) Queue.t = Queue.create ()

let pending () = Queue.length queue

let async f =
  let r = var () in
  let yd () =
    try
      match f () with
      | None -> true
      | Some v -> set r v ; false
    with exn ->
      Format.eprintf "[Fibers] yield error (%s)@." @@ Printexc.to_string exn ;
      false
  in Queue.push queue yd ; r

let yield =
  let lock = ref false in
  fun () ->
    if not !lock then
      begin
        lock := true ;
        try
          Queue.filter queue (fun yd -> yd ()) ;
          lock := false ;
        with exn ->
          lock := false ;
          raise exn
      end

(* -------------------------------------------------------------------------- *)
(* --- Mutex                                                              --- *)
(* -------------------------------------------------------------------------- *)

type mutex = int ref

let mutex n = ref n
let lock m =
  async
    begin fun () ->
      let n = !m in
      if n > 0 then (decr m ; Some ())
      else None
    end

let sync m f x =
  let* () = lock m in
  try
    let+ r = f x in
    incr m ; r
  with e ->
    incr m ; raise e

(* -------------------------------------------------------------------------- *)
(* --- Sleep                                                              --- *)
(* -------------------------------------------------------------------------- *)

let sleep n =
  let t = Unix.time () +. float n *. 1e-3 in
  async
    begin fun () ->
      if Unix.time () < t then None else Some ()
    end

(* -------------------------------------------------------------------------- *)
(* --- Results & Monitoring                                               --- *)
(* -------------------------------------------------------------------------- *)

let flush ?(polling=10) () =
  let interval = float polling *. 1e-3 in
  while pending () > 0 do yield () ; Unix.sleepf interval done

let background ?callback f = Option.iter (forward f) callback
let finally ?callback f = background ?callback f ; f

let monitor ?signal ?handler fn =
  match signal, handler with
  | Some s, Some h -> on s h ; forward fn (fun _ -> off s h) ; fn
  | _ -> fn

let await ?polling th = flush ?polling () ; find th

(* -------------------------------------------------------------------------- *)
