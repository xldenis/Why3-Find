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
(* --- Why3 Find Builtin Commands                                         --- *)
(* -------------------------------------------------------------------------- *)

let commands = ref []

let iter f = List.iter (fun (cmd,(args,_)) -> f cmd args) (List.rev !commands)

let wrap argv : string array =
  let open Bag in
  let pkgs = ref empty in
  let args = ref empty in
  let p = ref 0 in
  while !p < Array.length argv do
    begin
      match argv.(!p) with
      | "-p" | "--package" ->
          incr p ;
          if !p < Array.length argv then
            pkgs += argv.(!p)
          else
            failwith "missing PKG name"
      | arg ->
          args += arg
    end ; incr p
  done ;
  let pkgs = Meta.find_all (Bag.to_list !pkgs) in
  let load = List.fold_left
      (fun acc (pkg : Meta.pkg) -> acc +> "-L" +> pkg.path)
      empty pkgs in
  to_array @@ "-L" @< "." @< load ++ !args

let exec cmd args =
  match List.assoc cmd !commands with
  | exception Not_found -> Unix.execv ("why3-" ^ cmd) (wrap args)
  | _,process -> process args

let register ~name ?(args="") process =
  assert (not @@ List.mem_assoc name !commands) ;
  commands := (name,(args,process)) :: !commands

(* -------------------------------------------------------------------------- *)
(* --- why3find where                                                     --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"where"
    begin fun _argv ->
      List.iter
        (fun pkg -> Format.printf "%s@\n" pkg)
        Global.Sites.packages
    end

(* -------------------------------------------------------------------------- *)
(* --- why3find query                                                     --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"query" ~args:"[PKG...]"
    begin fun argv ->
      let path = ref false in
      let load = ref false in
      let deps = ref false in
      let query = ref [] in
      Arg.parse_argv argv
        [ "-p", Arg.Set path, "print package paths only"
        ; "-L", Arg.Set load, "prints -L <path> for all dependencies"
        ; "-r", Arg.Set deps, "recursive mode, query also dependencies" ]
        (fun pkg -> query := pkg :: !query)
        "USAGE:\n\
         \n  why3find query [PKG...]\n\n\
         DESCRIPTION:\n\
         \n  Query why3 package location.\n\n\
         OPTIONS:\n" ;
      let pkgs = List.rev !query in
      let pkgs =
        if !deps || !load then
          Meta.find_all pkgs
        else List.map Meta.find pkgs in
      if !path then
        List.iter
          (fun (p : Meta.pkg) -> Format.printf "%s@\n" p.path)
          pkgs
      else if !load then
        List.iter
          (fun (p : Meta.pkg) -> Format.printf "-L %s@\n" p.path)
          pkgs
      else
        List.iter
          (fun (p : Meta.pkg) ->
             Format.printf
               "Package %s:@\n\
                 - path: %s@\n\
                 - depends: %s@\n"
               p.name p.path (String.concat ", " p.depends)
          ) pkgs
    end

(* -------------------------------------------------------------------------- *)
(* --- why3 wrapper                                                       --- *)
(* -------------------------------------------------------------------------- *)



(* -------------------------------------------------------------------------- *)
