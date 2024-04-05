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

open Why3

(* -------------------------------------------------------------------------- *)
(* --- Provers                                                            --- *)
(* -------------------------------------------------------------------------- *)

type prover_desc = {
  name : string ;
  version : string ;
}

exception InvalidPattern of string
exception InvalidProverDescription of string

let split_pattern s =
  match String.split_on_char '@' s with
  | [] -> assert false
  | [ name ] -> String.lowercase_ascii name, None
  | [ name ; version ] -> String.lowercase_ascii name, Some version
  | _  -> raise (InvalidPattern s)

let desc_to_string p =
  Format.sprintf "%s@%s" p.name p.version

let desc_of_string s =
  try
    let (name, version) = split_pattern s in
    { name ; version = Option.get version }
  with
  | Invalid_argument _ | InvalidPattern _->
    raise (InvalidProverDescription s)

let desc_name p = p.name
let desc_version p = p.version

let pp_desc fmt p = Format.fprintf fmt "%s@%s" p.name p.version

type prover = {
  desc : prover_desc ;
  config : Whyconf.config_prover ;
  driver : Driver.driver ;
}

type sem = V of int | S of string
let sem s = try V (int_of_string s) with Failure _ -> S s
let cmp x y =
  match x,y with
  | V a,V b -> b - a
  | V _,S _ -> (-1)
  | S _,V _ -> (+1)
  | S a,S b -> String.compare a b
let cmp x y = cmp (sem x) (sem y)

let compare_version p q =
  List.compare cmp (String.split_on_char '.' p) (String.split_on_char '.' q)

let compare_desc p q =
  let c = String.compare p.name q.name in
  if c <> 0 then c else
    compare_version p.version q.version

let compare_prover (p : prover) (q : prover) =
  compare_desc p.desc q.desc

let why3_desc prv = Whyconf.prover_parseable_format prv.config.prover
let name prv = prv.desc.name
let version prv = prv.desc.version
let fullname p = Format.sprintf "%s@%s" (name p) (version p)
let infoname p = Format.sprintf "%s(%s)" (name p) (version p)
let pp_prover fmt p = Format.fprintf fmt "%s@%s" (name p) (version p)

let pmatch ~pattern d =
  let (name, version) = split_pattern pattern in
  name = d.name && (version = None || Option.get version = d.version)

let pattern_name s =
  let (name, _) = split_pattern s in
  name

let pattern_version s =
  let (_, version) = split_pattern s in
  version

let desc_of_config config = {
  name = String.lowercase_ascii config.Whyconf.prover.prover_name ;
  version = config.Whyconf.prover.prover_version ;
}

let load (env : Wenv.env) (config : Whyconf.config_prover) =
  try
    let main = Whyconf.get_main env.wconfig in {
      desc = desc_of_config config ;
      config ;
      driver = Driver.load_driver_for_prover main env.wenv config ;
    }
  with _ ->
    Log.error "failed to load driver for %s"
      (Whyconf.prover_parseable_format config.prover) ;
    exit 2

let all env =
  let aux c =
    if c.Whyconf.prover.prover_altern = ""
    then Some (load env c)
    else None in
  let configs = Whyconf.(Mprover.values @@ get_provers env.Wenv.wconfig) in
  List.sort compare_prover @@ List.filter_map aux @@ configs

let filter_prover ~name ?version p =
  p.desc.name = name
  && (version = None || p.desc.version = Option.get version)

let take_one = function
  | [] -> raise Not_found
  | [ p ] -> p
  | _ :: _ -> assert false

let take_best = function
  | [] -> raise Not_found
  | prv :: _ -> prv

let prover (env : Wenv.env) desc =
  let filter = filter_prover ~name:desc.name ~version:desc.version in
  take_one @@ List.filter filter @@ all env

let find_exn env ~pattern =
  let (name, version) = split_pattern pattern in
  let filter = filter_prover ~name ?version in
  take_best @@ List.filter filter @@ all env

let find_default env name =
  try [ find_exn env ~pattern:name ] with Not_found -> []

let default env =
  find_default env "alt-ergo" @
  find_default env "z3" @
  find_default env "cvc4" @
  find_default env "cvc5"

let select env ~patterns =
  let find pattern =
    try Some (find_exn env ~pattern) with
    | InvalidPattern s -> Log.warning "invalid prover pattern %s" s; None
    | Not_found -> Log.warning "prover %s not found (why3)" pattern; None in
  List.filter_map find patterns
