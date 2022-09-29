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
      Hashtbl.clear sections ;
      if !chdir <> "" then
        Utils.chdir !chdir
      else
        begin
          match Utils.locate config with
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

(* -------------------------------------------------------------------------- *)
(* --- Packages                                                           --- *)
(* -------------------------------------------------------------------------- *)

let pkgs = ref []
let prvs = ref []
let trfs = ref []

(* -------------------------------------------------------------------------- *)
(* --- Command Line Arguments                                             --- *)
(* -------------------------------------------------------------------------- *)

let removal = ref false

let add r a =
  modified := true ;
  r := a :: !r

let gets fd ?(prefix=false) ?(default=[]) r =
  load () ;
  let cfg =
    try get fd ~of_json:(Json.(jmap jstring))
    with Not_found -> default
  in
  if !removal then
    let filter =
      if prefix then
        fun x -> not @@ List.exists (String.starts_with ~prefix:x) !r
      else
        fun x -> not @@ List.mem x !r
    in List.filter filter cfg
  else
    cfg @ List.rev !r

let options = [
  "--root", Arg.Set_string chdir, "DIR change to directory";
  "--package", Arg.String (add pkgs), "PKG add package dependency";
  "--prover", Arg.String (add prvs), "PRV add automated prover";
  "--transf", Arg.String (add trfs), "TRANS add transformation ";
  "--remove", Arg.Set removal, "remove all specified packages, provers\
                                and transformations";
  "-p", Arg.String (add pkgs), " same as --package";
  "-P", Arg.String (add prvs), " same as --prover";
  "-T", Arg.String (add trfs), " same as --transf";
]

let pkg_options () =
  List.filter
    (fun (opt,_,_) -> match opt with
       | "--rot" | "-p" | "--package" -> true
       | _ -> false
    ) options

let packages () = gets "packages" pkgs
let provers () = gets "provers" ~prefix:true prvs
let transfs () = gets "transfs" ~default:["split_vc";"inline_goal" ] trfs

let sets fd xs =
  set fd ~to_json:Fun.id (`List (List.map (fun x -> `String x) xs))

let set_packages = sets "packages"
let set_provers = sets "provers"
let set_transfs = sets "transfs"

let arg0 file =
  if Filename.is_relative file then Filename.concat !prefix file else file

let arg1 file = load () ; arg0 file
let argv files = load () ; List.map arg0 files

(* -------------------------------------------------------------------------- *)
(* --- Saving Config                                                      --- *)
(* -------------------------------------------------------------------------- *)

let is_modified () = !modified

let save () =
  if !modified then
    begin
      let sections =
        List.sort (fun a b -> String.compare (fst a) (fst b)) @@
        Hashtbl.fold
          (fun fd js fds -> (fd,js) :: fds)
          sections []
      in Json.to_file config (`Assoc sections) ;
      Format.printf "Configuration saved to %s@."
        (Filename.concat (Sys.getcwd ()) config) ;
      loaded := false ;
    end

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
