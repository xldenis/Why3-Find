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
let iter f = List.iter (fun (cmd,_) -> f cmd) (List.rev !commands)
let exec cmd args =
  match List.assoc cmd !commands with
  | process -> process args
  | exception Not_found -> Unix.execv ("why3-" ^ cmd) args

let register ~name process =
  assert (not @@ List.mem_assoc name !commands) ;
  commands := (name,process) :: !commands

(* -------------------------------------------------------------------------- *)
(* --- why3find where                                                     --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"where"
    begin fun _args ->
      List.iter
        (fun pkg -> Format.printf "%s@\n" pkg)
        Global.Sites.packages
    end

(* -------------------------------------------------------------------------- *)
