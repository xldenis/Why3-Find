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
let loadcfg = ref true
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
      if !loadcfg && Sys.file_exists config then
        Json.of_file config |> Json.jiter (Hashtbl.add sections)
    end

let get fd ~of_json =
  load () ; of_json @@ Hashtbl.find sections fd

let set fd ~to_json value =
  load () ;
  Hashtbl.replace sections fd (to_json value) ;
  modified := true

(* -------------------------------------------------------------------------- *)
(* --- Command Line Options                                               --- *)
(* -------------------------------------------------------------------------- *)

let time = ref None
let depth = ref None
let cfgs = ref []
let drvs = ref []
let pkgs = ref []
let prvs = ref []
let tacs = ref []

(* -------------------------------------------------------------------------- *)
(* --- Argument Processing                                                --- *)
(* -------------------------------------------------------------------------- *)

type item =
  | Set of string
  | Add of string
  | Sub of string
  | Insert of int * string

let parse_item s =
  let n = String.length s in
  if n > 1 then
    match s.[0] with
    | '=' -> Set (String.sub s 1 (n-1))
    | '+' -> Add (String.sub s 1 (n-1))
    | '-' -> Sub (String.sub s 1 (n-1))
    | _ ->
      let rec digits k =
        if k < n then
          match s.[k] with
          | ':' -> k
          | '0'..'9' -> digits (succ k)
          | _ -> 0
        else 0 in
      let d = digits 0 in
      if d > 0 then
        let at = int_of_string (String.sub s 0 d) in
        Insert(at,String.sub s d (n-d))
      else Add s
  else Add s

let _parse_args s =
  List.map parse_item @@ String.split_on_char ',' s

(* -------------------------------------------------------------------------- *)
(* --- Command Line Arguments                                             --- *)
(* -------------------------------------------------------------------------- *)

let setv r v =
  modified := true ;
  r := Some v

let getv fd ~of_json ~default r =
  load () ;
  match !r with
  | Some v -> v
  | None ->
    load () ;
    try get fd ~of_json
    with Not_found -> default

let add r a =
  modified := true ;
  r := a :: !r

let filter ~prefix a =
  if prefix then
    fun x -> not @@ List.exists (String.starts_with ~prefix:x) a
  else
    fun x -> not @@ List.mem x a

let gets fd ?(prefix=false) ?(default=[]) r =
  load () ;
  let cfg =
    try get fd ~of_json:(Json.(jmap jstring))
    with Not_found -> default
  in
  cfg @ List.filter (filter ~prefix cfg) @@ List.rev !r

type opt = [
  | `All
  | `Package
  | `Prover
  | `Driver
  | `Config
]

let alloptions : (opt * string * Arg.spec * string) list = [
  `All, "--root", Arg.Set_string chdir, "DIR change to directory";
  `All, "--extra-config", Arg.String (add cfgs), "CFG extra why3 config";
  `Package, "--package", Arg.String (add pkgs), "PKG add package dependency";
  `Prover,  "--time", Arg.Float (setv time), "TIME median proof time";
  `Prover,  "--depth", Arg.Int (setv depth), "DEPTH proof search limit";
  `Prover,  "--prover", Arg.String (add prvs), "PRV add automated prover";
  `Prover,  "--tactic", Arg.String (add tacs), "TAC add proof tactic";
  `Driver,  "--driver", Arg.String (add drvs), "DRV add extraction driver";
  `Config, "--reset", Arg.Clear loadcfg, "reset configuration";
  `Package, "-p", Arg.String (add pkgs), " same as --package";
  `Prover,  "-t", Arg.Float (setv time), " same as --time";
  `Prover,  "-d", Arg.Float (setv time), " same as --depth";
  `Prover,  "-P", Arg.String (add prvs), " same as --prover";
  `Prover,  "-T", Arg.String (add tacs), " same as --tactic";
  `Driver,  "-D", Arg.String (add drvs), " same as --driver";
]

let options ?(packages=false) ?(provers=false) ?(drivers=false) () =
  let default = not packages && not provers && not drivers in
  List.filter_map
    (fun (opt,name,spec,descr) ->
       if default ||
          match opt with
          | `All -> true
          | `Package -> packages
          | `Prover -> provers
          | `Driver -> drivers
          | `Config -> packages && provers && drivers
       then Some(name,spec,descr)
       else None
    ) alloptions

let add_config = add cfgs
let add_driver = add drvs

let time () = getv "time" ~of_json:Json.jfloat ~default:1.0 time
let depth () = getv "depth" ~of_json:Json.jint ~default:6 depth
let configs () = gets "configs" cfgs
let packages () = gets "packages" pkgs
let provers () = gets "provers" ~prefix:true prvs
let tactics () = gets "tactics" ~default:["split_vc";"inline_goal" ] tacs
let drivers () = gets "drivers" drvs

let sets fd xs =
  set fd ~to_json:Fun.id (`List (List.map (fun x -> `String x) xs))

let set_time = set "time" ~to_json:(fun v -> `Float v)
let set_depth = set "depth" ~to_json:(fun n -> `Int n)
let set_configs = sets "configs"
let set_packages = sets "packages"
let set_provers = sets "provers"
let set_tactics = sets "tactics"
let set_drivers = sets "drivers"

let arg0 file =
  if Filename.is_relative file then Filename.concat !prefix file else file

let arg1 file = load () ; arg0 file
let argv files = load () ; List.map arg1 files

let filter ~exts p = List.mem (Filename.extension p) exts

let allfiles ~exts f path =
  if not (Sys.file_exists path) then
    Utils.failwith "Unknown file or directory %S" path ;
  if not (filter ~exts path || Sys.is_directory path) then
    Utils.failwith "File %S is neither a Why3 file nor a directory" path ;
  Utils.iterpath
    ~file:(fun p -> if filter ~exts p then f p)
    path

let argfiles ~exts files = load () ;
  let paths = ref [] in
  List.iter
    (fun f -> allfiles ~exts (fun p -> paths := p :: !paths) (arg1 f))
    files ;
  List.rev !paths

(* -------------------------------------------------------------------------- *)
(* --- Saving Project Config                                              --- *)
(* -------------------------------------------------------------------------- *)

let is_modified () = !modified
let set_modified () = modified := true

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
  pkgs : Meta.pkg list ;
}

let init () =
  let open Why3 in
  begin
    let pkgs = Meta.find_all @@ packages () in
    let pkg_path = List.map (fun m -> m.Meta.path) pkgs in
    let extra_config = configs () in
    let wconfig = Whyconf.init_config ~extra_config None in
    let wmain = Whyconf.get_main wconfig in
    let wpath = Whyconf.loadpath wmain in
    let wenv = Why3.Env.create_env ("." :: pkg_path @ wpath) in
    { wconfig ; wenv ; pkgs }
  end

(* -------------------------------------------------------------------------- *)
