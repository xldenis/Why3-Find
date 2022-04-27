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

let exec cmd args =
  match List.assoc cmd !commands with
  | exception Not_found -> Unix.execv ("why3-" ^ cmd) args
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

let find pkg =
  let rec lookup pkg = function
    | [] -> raise Not_found
    | d::ds ->
        let d = Filename.concat d pkg in
        if Sys.file_exists d then d else lookup pkg ds
  in lookup pkg Global.Sites.packages

let () = register ~name:"query" ~args:"[PKG...]"
    begin fun argv ->
      let quiet = ref false in
      Arg.parse_argv argv
        [ "-q", Arg.Set quiet, "quiet mode, print location or exit(1)" ]
        begin fun pkg ->
           if !quiet then
             try
               let d = find pkg in
               Format.printf "%s@." d
             with Not_found -> exit 1
           else
             let r = try find pkg with Not_found -> "not found" in
             Format.printf "%s: %s@." pkg r
        end
        "USAGE:\n\
         \n  why3find query [PKG...]\n\n\
         DESCRIPTION:\n\
         \n  Query why3 package location.\n\n\
         OPTIONS:\n"
    end

(* -------------------------------------------------------------------------- *)
(* --- why3find query                                                     --- *)
(* -------------------------------------------------------------------------- *)

(* -------------------------------------------------------------------------- *)
