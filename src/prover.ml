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

let desc_to_string p =
  Format.sprintf "%s@%s" p.name p.version

let desc_of_string s =
  match String.split_on_char '@' s with
  | [ name; version ] -> { name = String.lowercase_ascii name ; version }
  | _ -> invalid_arg "desc_of_string"

let desc_name p = p.name
let desc_version p = p.version

let pp_desc fmt p = Format.fprintf fmt "%s@%s" p.name p.version

type prover = {
  desc : prover_desc ;
  config : Whyconf.config_prover ;
  driver : Driver.driver ;
}

type sem = V of int | S of string
let sem s = try V (int_of_string s) with _ -> S s
let cmp x y =
  match x,y with
  | V a,V b -> b - a
  | V _,S _ -> (-1)
  | S _,V _ -> (+1)
  | S a,S b -> String.compare a b
let rec cmps xs ys =
  match xs,ys with
  | [],[] -> 0
  | [],_ -> (-1)
  | _,[] -> (+1)
  | x::rxs,y::rys -> let c = cmp x y in if c <> 0 then c else cmps rxs rys
let tosem p = List.map sem (String.split_on_char 'c' p)

let compare_desc p q =
  let c = String.compare p.name q.name in
  if c <> 0 then c else
    cmps (tosem p.version) (tosem q.version)

let compare_wprover (p : Whyconf.prover) (q : Whyconf.prover) =
  let c = String.compare
      (String.lowercase_ascii p.prover_name)
      (String.lowercase_ascii q.prover_name) in
  if c <> 0 then c else
    let c = cmps (tosem p.prover_version) (tosem q.prover_version) in
    if c <> 0 then c else
      String.compare p.prover_altern q.prover_altern

let compare_config (p : Whyconf.config_prover) (q : Whyconf.config_prover) =
  compare_wprover p.prover q.prover

let compare_prover (p : prover) (q : prover) =
  compare_config p.config q.config

let why3_desc prv = Whyconf.prover_parseable_format prv.config.prover
let name prv = prv.desc.name
let version prv = prv.desc.version
let fullname p = Format.sprintf "%s@%s" (name p) (version p)
let infoname p = Format.sprintf "%s(%s)" (name p) (version p)
let pp_prover fmt p = Format.fprintf fmt "%s@%s" (name p) (version p)

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
    Format.eprintf "Error: failed to load driver for %s@."
      (Whyconf.prover_parseable_format config.prover) ;
    exit 2

let prover (env : Wenv.env) desc =
  load env @@ Whyconf.(
      let all = get_provers env.wconfig in
      snd @@ Mprover.choose @@ Mprover.filter (fun _ c ->
          String.lowercase_ascii c.prover.prover_name = desc.name
          && c.prover.prover_version = desc.version
          && c.prover.prover_altern = ""
        ) all
    )

let find_shortcut (env : Wenv.env) name =
  Whyconf.Mprover.find
    (Wstdlib.Mstr.find name @@ Whyconf.get_prover_shortcuts env.wconfig)
    (Whyconf.get_provers env.wconfig)

let find_filter (env : Wenv.env) ~name ?(version="") () =
  let mpr = Whyconf.get_provers env.wconfig in
  Whyconf.Mprover.fold
    (fun key cfg acc ->
       if key.prover_altern = "" &&
          (version = "" || key.prover_version = version) &&
          String.lowercase_ascii key.prover_name = name
       then cfg :: acc else acc
    ) mpr []

let take_one = function [cfg] -> cfg | _ -> raise Not_found
let take_best ~name = function
  | [] ->
    Format.eprintf "Warning: prover %s not found@." name ;
    raise Not_found
  | [prv] -> prv
  | others -> List.hd @@ List.sort compare_config others

let take_default = function
  | [] -> []
  | [cfg] -> [cfg]
  | others -> [List.hd @@ List.sort compare_config others]

let find env ~pattern =
  try
    let prover =
      load env @@
      match String.split_on_char '@' pattern with
      | [name] ->
        begin
          try find_shortcut env name with Not_found ->
            take_best ~name @@ find_filter env ~name ()
        end
      | [name;version] ->
        begin
          try take_one @@ find_filter env ~name ~version () with Not_found ->
            Format.eprintf
              "Warning: prover %s@%s not found@." name version ;
            raise Not_found
        end
      | _ ->
        Format.eprintf "Invalid prover pattern '%s'@." pattern ;
        exit 2
    in Some prover
  with Not_found -> None

let find_default env name =
  List.map (load env) @@
  try [find_shortcut env name] with Not_found ->
    take_default @@ find_filter env ~name ()

let default env =
  find_default env "alt-ergo" @
  find_default env "z3" @
  find_default env "cvc4" @
  find_default env "cvc5"

let select env ~patterns =
  List.filter_map (fun pattern -> find env ~pattern) patterns

let all (env : Wenv.env) =
  List.sort compare_prover @@
  Whyconf.Mprover.fold
    (fun _id config prvs ->
       if config.Whyconf.prover.prover_altern = "" then
         (load env config) :: prvs
       else prvs
    ) (Whyconf.get_provers env.wconfig) []
