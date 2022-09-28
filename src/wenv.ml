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
(* --- Why3Find Environment                                               --- *)
(* -------------------------------------------------------------------------- *)

let config = "why3find.json"
let prefix = ref ""
let sections = Hashtbl.create 0
let loaded = ref false
let modified = ref false
let chdir = ref ""

let load () =
  if not !loaded then
    begin
      loaded := true ;
      if !chdir <> "" then
        Utils.chdir !chdir
      else
        begin
          match Utils.locate [config;"Makefile";".git"] with
          | Some(dir,path) -> Utils.chdir dir ; prefix := path
          | None -> ()
        end ;
      if Sys.file_exists config then
        Json.of_file config |> Json.jiter (Hashtbl.add sections)
    end

let get fd ~of_json =
  load () ; of_json @@ Hashtbl.find sections fd

let set fd ~to_json value =
  load () ;
  Hashtbl.replace sections fd (to_json value) ;
  modified := true

let save () =
  if !modified then
    let fields =
      List.sort (fun a b -> String.compare (fst a) (fst b)) @@
      Hashtbl.fold
        (fun fd js fds -> (fd,js) :: fds)
        sections []
    in
    begin
      Json.to_file config (`Assoc fields) ;
      Format.printf "Configuration saved to %s@."
        (Filename.concat (Sys.getcwd ()) config) ;
    end

(* -------------------------------------------------------------------------- *)
(* --- Command Line Arguments                                             --- *)
(* -------------------------------------------------------------------------- *)

let args = [
  "--root", Arg.Set_string chdir, "DIR change to directory";
]

let argv files =
  load () ;
  List.map (Filename.concat !prefix) files

(* -------------------------------------------------------------------------- *)
(* --- Why3 Environment                                                   --- *)
(* -------------------------------------------------------------------------- *)

type env = {
  wconfig : Why3.Whyconf.config ;
  wenv : Why3.Env.env ;
}

let init ~pkgs =
  let open Why3 in
  begin
    let pkgs = Meta.find_all pkgs in
    let pkg_path = List.map (fun m -> m.Meta.path) pkgs in
    let wconfig = Whyconf.init_config None in
    let wmain = Whyconf.get_main wconfig in
    let wpath = Whyconf.loadpath wmain in
    let wenv = Why3.Env.create_env ("." :: pkg_path @ wpath) in
    { wconfig ; wenv }
  end

(* -------------------------------------------------------------------------- *)
