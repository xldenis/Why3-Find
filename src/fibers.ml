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

type 'a t = ('a -> unit) -> unit

let return v k = k v
let bind ta fb k = ta (fun x -> fb x k)
let apply ta f k = ta (fun x -> k (f x))
let map f ta k = ta (fun x -> k (f x))
let pair ta tb k = ta (fun x -> tb (fun y -> k (x,y)))
let await f k = f k

module Monad =
struct
  let (let*) = bind
  let (let+) = apply
  let (and*) = pair
  let (and+) = pair
end
open Monad

(* -------------------------------------------------------------------------- *)
(* --- Iterable Queues                                                    --- *)
(* -------------------------------------------------------------------------- *)

module Queue :
sig
  type 'a t
  val create : unit -> 'a t
  val size : 'a t -> int
  val push : 'a t -> 'a -> unit
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

  let size q =
    let n = q.size in
    if 0 <= n then n
    else
      let n = List.length q.head + List.length q.tail in
      q.size <- n ; n

  let push q v = q.size <- (-1) ; q.tail <- v::q.tail

  let rec exec f xs ys =
    match xs with
    | [] -> if ys = [] then [] else exec f ys []
    | x::xs ->
      let ok = f x in
      let rs = exec f xs ys in
      if ok then x :: rs else rs

  let filter q f =
    begin
      match q.head, q.tail with
      | [], [] -> ()
      | hd, tl ->
        q.tail <- [] ;
        q.size <- (-1) ;
        q.head <- exec f hd (List.rev tl) ;
    end

  let iter q f = filter q (fun x -> f x ; true)

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
let clear = Queue.clear
let hook s k f x =
  on s k ;
  try
    let+ r = f x in
    off s k ; r
  with exn ->
    off s k ; raise exn

(* -------------------------------------------------------------------------- *)
(* --- Variables                                                          --- *)
(* -------------------------------------------------------------------------- *)

type 'a var = 'a state ref
and 'a state = Done of 'a | Wait of 'a signal

let var () = ref @@ Wait (Queue.create ())
let get v k = match !v with Done r -> k r | Wait q -> Queue.push q k
let set v r = match !v with Done _ -> () | Wait q -> v := Done r ; emit q r

(* -------------------------------------------------------------------------- *)
(* --- List Combinators                                                   --- *)
(* -------------------------------------------------------------------------- *)

let any ts =
  let x = var () in
  List.iter (fun t -> t (set x)) ts ;
  get x

let all ts =
  let xs = ref [] in
  let n = ref (List.length ts) in
  let rs = var () in
  let recv i v =
    xs := (i,v) :: !xs ;
    decr n ;
    if !n <= 0 then
      let ys = List.sort (fun (i,_) (j,_) -> Stdlib.compare i j) !xs in
      set rs (List.map snd ys)
  in
  let rec schedule i = function
    | [] -> ()
    | t::ts -> t (recv i) ; schedule (succ i) ts
  in schedule 0 ts ; get rs

let rec seq ts =
  match ts with
  | [] -> return []
  | t :: ts ->
    let* r = t in
    let* rs = seq ts in
    return (r::rs)

(* -------------------------------------------------------------------------- *)
(* --- Asynchronous Tasks                                                 --- *)
(* -------------------------------------------------------------------------- *)

let queue : (unit -> bool) Queue.t = Queue.create ()

let pending () = Queue.size queue

let async f =
  let x = var () in
  let yd () =
    match f () with
    | None -> true
    | Some v -> set x v ; false
  in Queue.push queue yd ; get x

let yield () = Queue.filter queue (fun yd -> yd ())

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
(* --- Polling                                                            --- *)
(* -------------------------------------------------------------------------- *)

let sleep n =
  let t = Unix.time () +. float n *. 1e-3 in
  async
    begin fun () ->
      if Unix.time () < t then None else Some ()
    end

let flush ?(polling=10) () =
  while pending () > 0 do
    Unix.sleepf (float polling *. 1e-3) ;
    yield () ;
  done

let run ?polling ?(callback=ignore) f = f callback ; flush ?polling ()

(* -------------------------------------------------------------------------- *)
