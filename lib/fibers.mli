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

(** Lightweight and Monadic Fibers Library.

    Freely inspired from the
    {{:https://ocaml.org/p/fiber/3.1.1/doc/index.html}Fiber} dune package.
*)

(** {1 Basic Constructors} *)

(** The time of computations that produces an ['a]. *)
type 'a t

(** [return v] computation that immediately returns [v]. *)
val return : 'a -> 'a t

(** Monadic [bind a f] operator binds result [v] of [a]
    to a new computation [f v]. See {!Monad.let*} operator. *)
val bind : 'a t -> ('a -> 'b t) -> 'b t

(** Monadic [apply a f] operator binds result [v] of [a]
    to computation [return (f v)]. See {!Monad.let+} operator.*)
val apply : 'a t -> ('a -> 'b) -> 'b t

(** Same as {!apply} with reversed arguments. *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** Combines two computations into a parallel computation of both.

    See {!Monad.and*}, and {!Monad.@*} operators. *)
val par : 'a t -> 'b t -> ('a * 'b) t

(** Monadic and Applicative operators.

    You can use them locally with following syntax:

    {[
      let sum ta tb =
        let open Monad in
        let* x = ta in
        let* y = tb in
        return (x+y)
    ]}

    Conforms to OCaml language manual for let-bindings operators
    extension.
*)
module Monad :
sig

  (** Monadic operator, same as {!bind}. *)
  val (let*) : 'a t -> ('a -> 'b t) -> 'b t

  (** Applicative map operator, same as {!apply}. *)
  val (let+) : 'a t -> ('a -> 'b) -> 'b t

  (** Monoidal product operator, same as {!par}. *)
  val (and*) : 'a t -> 'b t -> ('a * 'b) t

  (** Map operator, same as {!map}. *)
  val (@+) : ('a -> 'b) -> 'a t -> 'b t

  (** Parallel product operator, same as {!par}. *)
  val (@*) : 'a t -> 'b t -> ('a * 'b) t

end

(** Iterable Queues.

    This data structure is very similar to {!Queue} from
    the standard library, but offers efficient filtering.

    It is designed for internal use, although it might be usefull
    for other purposes.
*)
module Queue :
sig
  type 'a t
  val create : unit -> 'a t
  val length : 'a t -> int
  val pop : 'a t -> 'a
  val push : 'a t -> 'a -> unit
  val iter : 'a t -> ('a -> unit) -> unit
  val filter : 'a t -> ('a -> bool) -> unit
  val clear : 'a t -> unit
end

(** {1 List Combinators} *)

(** [any ks] synchronizes on the first computation in [ks] that terminates. *)
val any : 'a t list -> 'a t

(** [seq ks] waits all continuations in {i sequence} and returns their result. *)
val seq : 'a t list -> 'a list t

(** [all ks] waits all continuations in {i parallel} and returns their result one
    they _all_ have terminated. *)
val all : 'a t list -> 'a list t

(** [find f ks] runs all continuations in {i parallel} and returns
    the _first_ result that satisfies the filter [f], if any. *)
val first : ('a -> 'b option) -> 'a t list -> 'b option t

(** {1 Signals} *)

(** Inter tasks communication. *)
type 'a signal

(** Creates a new channel for signaling. *)
val signal : unit -> 'a signal

(** Register a hook on the signal. *)
val on : 'a signal -> ('a -> unit) -> unit

(** Un-register a hook on the signal. *)
val off : 'a signal -> ('a -> unit) -> unit

(** Register a hook that automatically disconnect itself on first emit. *)
val once : 'a signal -> ('a -> unit) -> unit

(** At most one hook is connected to the signal. *)
val connected : 'a signal -> bool

(** Remove all hooks on the signal. *)
val disconnect : 'a signal -> unit

(** Remove all registered hooks. *)
val clear : 'a signal -> unit

(** Broadcast a value on all registered hooks. *)
val emit : 'a signal -> 'a -> unit

(** {1 Variables} *)

(** Condition variables to synchronize tasks. *)
type 'a var

(** Creates a variable. *)
val var : ?init:'a -> unit -> 'a var

(** [get x] waits on the variable [x] until it is set.
    If the variable [x] is already initialized, its value is immediately
    returned. *)
val get : 'a var -> 'a t

(** [set x v] stores value [v] into variable [x] and signals all computations
    that are waiting on [x].

    Successive calls to [set x _] will be ignored. *)
val set : 'a var -> 'a -> unit

(** [peek x] returns the value assigned to variable [x], if any. *)
val peek : 'a var -> 'a option

(** [find x] returns the value assigned to variable [x] or raise [Not_found].
    @raise Not_found *)
val find : 'a var -> 'a

(** [defined x] returns [true] is the variable [x] has been assigned. *)
val defined : 'a var -> bool

(** {1 Mutual Exclusion} *)

(** A limited set of resources to be shared on different tasks. *)
type mutex

(** [mutex n] creates a pool of [n] resources. *)
val mutex : int -> mutex

(** [sync m f x] synchronize task [f x] as soon as there is one
    available resource in mutex [m]. The resource is released when
    the computation terminates. *)
val sync : mutex -> ('a -> 'b t) -> 'a -> 'b t

(** {1 Asynchronous Tasks}

    The [Fibers] library maintains a global pool of asynchronous tasks.
    Cooperative threads must repeatedly invoke {!yield} to ping pending
    tasks. Eventually, the list of pending tasks can be wait to terminate
    by polling with {!flush}. *)

(** [async f] register a new asynchronous task. Each time the global queue
    is yield, [f ()] is invoked until it returns [Some v] which
    terminates the continuation with value [v]. *)
val async : (unit -> 'a option) -> 'a t

(** Ping all pending asynchronous tasks for termination. *)
val yield : unit -> unit

(** Number of asynchronous tasks still waiting. *)
val pending : unit -> int

(** Waits for all pending tasks to terminate.
    The default [~polling] interval is [10] milliseconds. *)
val flush : ?polling:int -> unit -> unit

(** [sleep ms] asynchronously waits for (at least) [ms] milliseconds. *)
val sleep : int -> unit t

(** {1 Computation Results} *)

(** [result f] starts computation [f] and immediately returns a variable
    that will eventually capture the result of [f]. *)
val result : 'a t -> 'a var

(** [finally ~callback f] starts computation [f], eventually passing
    its result to [callback], while immediately returning [f] for chaining.

    Default [~callback] is [ignore]. *)
val finally : ?callback:('a -> unit) -> 'a t -> 'a t

(** [background f k] starts computation [f] and immediately returns.
    Result of computation will be eventually passed to continuation [k].

    Default [~callback] is [ignore].
    This is the same as [ignore @@ finally ?callback f].
*)
val background : ?callback:('a -> unit) -> 'a t -> unit

(** [monitor ~signal ~handler f] starts computation [f] and returns [f]
    immediately for chaining,
    while connecting [handler] to [signal] until termination when specified.
*)
val monitor : ?signal:'a signal -> ?handler:('a -> unit) -> 'b t -> 'b t

(** [await f] waits until the end of computation [f],
    yielding periodically until termination.

    Default [~polling] interval is [10] milliseconds. *)
val await : ?polling:int -> 'a t -> 'a

(**************************************************************************)
