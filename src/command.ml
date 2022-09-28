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
(* --- Command Line Parsing                                               --- *)
(* -------------------------------------------------------------------------- *)

let commands = ref []

let usage argv msg =
  Array.iter (function
      | "-h" | "-help" | "--help" -> raise @@ Stdlib.Arg.Help msg
      | _ -> ()
    ) argv

let allargs argv xs =
  let all =
    Array.exists (function "-a" | "--all" -> true | _ -> false) argv in
  match xs with
  | x::_ when not all -> [x]
  | _ -> xs

let iter f = List.iter (fun (cmd,(args,_)) -> f cmd args) (List.rev !commands)

let get argv k msg =
  if k < Array.length argv then argv.(k) else failwith msg

let get_opt argv k =
  if k < Array.length argv then Some argv.(k) else None

let generate file gen =
  let out = open_out file in
  let fmt = Format.formatter_of_out_channel out in
  begin
    gen fmt ;
    Format.pp_print_flush fmt () ;
    close_out out ;
  end

let var = Str.regexp "%{\\([a-zA-Z]+\\)}"

let template ~subst ~src ~tgt =
  Utils.mkdirs (Filename.dirname tgt) ;
  let inc = open_in src in
  let out = open_out tgt in
  let apply subst text =
    try List.assoc (Str.matched_group 1 text) subst
    with Not_found -> Str.matched_group 0 text
  in
  let rec walk () =
    match Stdlib.input_line inc with
    | exception End_of_file -> ()
    | line ->
      let line' = Str.global_substitute var (apply subst) line in
      Stdlib.output_string out line' ;
      Stdlib.output_string out "\n" ;
      walk ()
  in walk () ;
  close_in inc ;
  close_out out ;
  Format.printf "Initialized %s@." tgt

(* -------------------------------------------------------------------------- *)
(* --- Wrapper Command                                                    --- *)
(* -------------------------------------------------------------------------- *)

let verbose = ref false

let exec
    ?(cmd="why3")
    ?(auto=false)
    ?(configs=true)
    ?(drivers=false)
    ?(prefix=[])
    ?(pkgs=[])
    ?(skip=1)
    argv =
  let open Bag in
  let args = ref empty in
  let pkgs = ref (Bag.of_list pkgs) in
  let drivers = ref drivers in
  let configs = ref configs in
  let p = ref skip in
  while !p < Array.length argv do
    begin
      match argv.(!p) with
      | "-p" | "--package" ->
        incr p ;
        if !p < Array.length argv then
          pkgs += argv.(!p)
        else
          failwith "missing PKG name"
      | "--drivers" when auto -> drivers := true
      | "--configs" when auto -> configs := true
      | "-v" | "--verbose" -> verbose := true
      | arg ->
        args += arg
    end ; incr p
  done ;
  let pkgs = Meta.find_all (Bag.to_list !pkgs) in
  let cfg =
    if !configs then
      Bag.merge
        (fun (pkg : Meta.pkg) ->
           Bag.map
             (Printf.sprintf "--extra-config=%s/%s" pkg.path)
             pkg.configs
        ) pkgs
    else Bag.empty in
  let drv =
    if !drivers then
      Bag.merge
        (fun (pkg : Meta.pkg) ->
           Bag.map
             (Printf.sprintf "--driver=%s/%s" pkg.path)
             pkg.drivers
        ) pkgs
    else Bag.empty in
  let load =
    Bag.map
      (fun (pkg : Meta.pkg) -> Printf.sprintf "--library=%s" pkg.path)
      pkgs in
  let argv =
    to_array @@
    Bag.of_list (cmd::prefix) ++ cfg +> "-L" +> "." ++ load ++ drv ++ !args
  in
  if !verbose then
    begin
      Format.printf "%s" cmd ;
      for i = 1 to Array.length argv - 1 do
        Format.printf " %s" argv.(i)
      done ;
      Format.printf "@." ;
    end ;
  Unix.execvp cmd argv

let process cmd argv : unit =
  match List.assoc cmd !commands with
  | exception Not_found ->
    usage argv
      "USAGE:\n\
       \n  why3find CMD [ARGS...]\n\n\
       DESCRIPTION:\n\
       \n  Execute command \"CMD\" with wrapped arguments.\n\n\
       OPTIONS:\n\
       \n  --p|--package PKG : pass --library=<path> for the package\
       \n  --configs : pass also --extra-config-file=<CFG> options\
       \n  --drivers : pass also --driver=<DRV> options\
       \n" ;
    exec ~cmd ~auto:true argv
  | _,process -> process argv

let register ~name ?(args="") process =
  if List.mem_assoc name !commands then
    Utils.failwith "Duplicate command '%s'" name ;
  commands := (name,(args,process)) :: !commands

(* -------------------------------------------------------------------------- *)
(* --- why3find where                                                     --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"where"
    begin fun argv ->
      usage argv
        "USAGE:\n\
         \n  why3find where [-a|--all]\n\n\
         DESCRIPTION:\n\
         \n  Prints installation site(s).\
         \n" ;
      List.iter
        (fun site -> Format.printf "%s@\n" site)
        (allargs argv Global.Sites.packages)
    end

(* -------------------------------------------------------------------------- *)
(* --- why3find shared                                                    --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"shared"
    begin fun argv ->
      usage argv
        "USAGE:\n\
         \n  why3find shared [-a|--all]\n\n\
         DESCRIPTION:\n\
         \n  Prints shared resources site(s).\
         \n" ;
      List.iter
        (fun site -> Format.printf "%s@\n" site)
        (allargs argv Global.Sites.resources)
    end

(* -------------------------------------------------------------------------- *)
(* --- why3find init                                                      --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"init"
    begin fun argv ->
      usage argv
        "USAGE:\n\
         \n  why3find init PKG [DIR]\n\n\
         DESCRIPTION:\n\
         \n  Create templates for dune and make for package PKG.\
         \n  Files are created in directory DIR (default ./PKG).\
         \n\
         \n  See 'why3find makefile -h' for instructions.\
         \n" ;
      let pkg = get argv 1 "missing PKG name" in
      let dir =
        match get_opt argv 2 with
        | None -> pkg
        | Some dir -> dir in
      let subst = ["pkg",pkg] in
      template
        ~subst
        ~src:(Meta.shared "git.template")
        ~tgt:(Filename.concat dir ".gitignore") ;
      template
        ~subst
        ~src:(Meta.shared "dune.template")
        ~tgt:(Filename.concat dir "dune-project") ;
      template
        ~subst
        ~src:(Meta.shared "make.template")
        ~tgt:(Filename.concat dir "Makefile") ;
    end

(* -------------------------------------------------------------------------- *)
(* --- why3find makefile                                                  --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"makefile"
    begin fun argv ->
      usage argv
        "USAGE:\n\
         \n  why3find makefile\n\n\
         DESCRIPTION:\n\
         \n  Prints shared makefile location.\n\n\
         MAKEFILE USAGE:\n\
         \n  WHY3_PACKAGE=PKG       package name\
         \n  WHY3_DEPENDS=PKG...    package dependencies\
         \n  WHY3_OPTIONS=OPTION... general why3find or why3 options\
         \n  WHY3_HAMMERS=OPTION... hammer options\
         \n  WHY3_CONFIGS=**/*.cfg  extra configuration files \
         \n  WHY3_DRIVERS=**/*.drv  extraction drivers \
         \n  WHY3_EXTRACT=MODULE... extracted modules\
         \n  WHY3_LIBRARIES=PKG...  extra OCaml package dependencies\
         \n\
         \n  include $(shell why3find makefile)
         \n\
         MAKEFILE TARGETS:\n\
         \n  make all        build (default, extensible)\
         \n  make build      generate dune file(s) (extensible)\
         \n  make install    install the why3 package (extensible)\
         \n  make uninstall  remove the why3 package (extensible)\
         \n\
         \n  make compile | file.cc      compile file(s)\
         \n  make prove   | file.prove   hammer file(s)\
         \n  make ide     | file.ide     open ide\
         \n  make fix     | file.fix     hammer file(s) and open ide if needed\
         \n  make check   | file.check   replay session (obsolete only)\
         \n  make replay  | file.replay  replay session\
         \n" ;
      Format.printf "%s@\n" @@ Meta.shared "makefile"
    end

let () = register ~name:"make"
    begin fun argv ->
      usage argv
        "USAGE:\n\
         \n  why3find make [ARGS...]\n\n\
         DESCRIPTION:\n\
         \n  run make -C DIR ARGS... from the closest\
         \n  directory DIR that contains a Makefile.\
         \n" ;
      let dir =
        match Utils.locate ["Makefile"] with
        | None -> failwith "Makefile not found"
        | Some(dir,_) -> Utils.chdir dir ; dir in
      let args = Array.sub argv 1 (Array.length argv - 1) in
      let argv = Array.append [| "make" ; "-C" ; dir |] args in
      Unix.execvp "make" argv
    end

(* -------------------------------------------------------------------------- *)
(* --- why3find list                                                      --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"list"
    begin fun argv ->
      usage argv
        "USAGE:\n\
         \n  why3find list\n\n\
         DESCRIPTION:\n\
         \n  Prints all installed packages.\
         \n" ;
      List.iter
        (fun site ->
           if Sys.file_exists site && Sys.is_directory site then
             Array.iter
               (fun pkg -> Format.printf "%s/%s@\n" site pkg)
               (Sys.readdir site)
        ) Global.Sites.packages
    end

(* -------------------------------------------------------------------------- *)
(* --- why3find query                                                     --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"query" ~args:"[PKG...]"
    begin fun argv ->
      let libs = ref false in
      let path = ref false in
      let load = ref false in
      let deps = ref false in
      let query = ref [] in
      Arg.parse_argv argv
        [ "-p", Arg.Set path, "print package paths only"
        ; "-l", Arg.Set libs, "print extracted ocaml libraries"
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
        if !deps || !load
        then Meta.find_all pkgs
        else List.map Meta.find pkgs in
      if !path then
        List.iter
          (fun (p : Meta.pkg) -> Format.printf "%s@\n" p.path)
          pkgs
      else if !load then
        List.iter
          (fun (p : Meta.pkg) -> Format.printf "-L %s@\n" p.path)
          pkgs
      else if !libs then
        List.iter
          (fun (p : Meta.pkg) ->
             if p.library then Format.printf "%s@\n" p.name)
          pkgs
      else
        let pp_opt name ps =
          Format.printf "  @[<hov 2>%s: %s@]@\n" name
            (if ps = [] then "-" else String.concat ", " ps)
        in
        List.iter
          (fun (p : Meta.pkg) ->
             Format.printf "Package %s:@\n" p.name ;
             Format.printf "  path: %s@\n" p.path ;
             Format.printf "  library: %s@\n"
               (if p.library then "-" else p.name) ;
             pp_opt "depends" p.depends ;
             pp_opt "configs" p.configs ;
             pp_opt "drivers" p.drivers ;
          ) pkgs
    end

(* -------------------------------------------------------------------------- *)
(* --- Install/Remove commands                                            --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"uninstall" ~args:"[PKG...]"
    begin fun argv ->
      usage argv
        "USAGE:\n\
         \n  why3find uninstall [PKG...]\n\n\
         DESCRIPTION:\n\
         \n  Remove all specified packages from topmost installation site.\
         \n" ;
      for i = 1 to Array.length argv - 1 do
        let pkg = argv.(i) in
        let path = Meta.path pkg in
        if Sys.file_exists path then
          begin
            Format.printf "remove %s@." pkg ;
            Utils.cleanup path ;
          end
      done
    end

let () = register ~name:"install" ~args:"PKG [ARG...]"
    begin fun argv ->
      let rs = ref Bag.empty in
      let dune = ref false in
      Arg.parse_argv argv
        [ "--local", Arg.Set dune , "DIR generate dune files in DIR instead" ]
        (fun a -> rs := Bag.(!rs +> a))
        "USAGE:\n\
         \n  why3find install [OPTIONS] PKG [ARG...]\n\n\
         DESCRIPTION:\n\
         \n  Install the package PKG at the topmost installation site.\
         \n  Contents of the installed package is specified by ARG extension:\
         \n
         \n    PKG' register package PKG' as a dependency of PKG\
         \n    PKG/**/*.mlw a why3 source file in PKG's scope\
         \n    *.cfg extra why3 configuration file\
         \n    *.drv ocaml extraction driver for PKG clients\
         \n\n\
         OPTIONS:\n" ;
      let argv = Bag.to_array !rs in
      let pkg = get argv 0 "Missing PKG name" in
      let dune = !dune in
      let path = if dune then "." else Meta.path pkg in
      if not dune && Sys.file_exists path then
        begin
          Format.printf "remove %s@." pkg ;
          Utils.cleanup path ;
        end ;
      if not dune then Utils.mkdirs path ;
      let sources = ref ["META.json"] in
      let depends = ref [] in
      let drivers = ref [] in
      let configs = ref [] in
      let prefix = Filename.concat pkg "" in
      let install ~src =
        if dune
        then sources := src :: !sources
        else Utils.copy ~src ~tgt:(Filename.concat path src)
      in
      for i = 1 to Array.length argv - 1 do
        let src = argv.(i) in
        match Filename.extension src with
        | "" -> Format.printf "depend %s@." src ; depends := src :: !depends
        | ".mlw" ->
          if not (String.starts_with ~prefix src) then
            Utils.failwith "can not install %S from outside of %S" src prefix ;
          Format.printf "install (source) %s@." src ;
          install ~src ;
        | ".cfg" ->
          Format.printf "install (config) %s@." src ;
          install ~src ;
          configs := src :: !configs ;
        | ".drv" ->
          Format.printf "install (driver) %s@." src ;
          install ~src ;
          drivers := src :: !drivers ;
        | _ -> Utils.failwith "don't know what to do with %S" src
      done ;
      Format.printf "install (meta)   META.json@." ;
      Meta.install {
        name = pkg ; path ;
        library = Sys.file_exists "lib" ;
        depends = List.rev !depends ;
        drivers = List.rev !drivers ;
        configs = List.rev !configs ;
      } ;
      if dune then
        begin
          generate "dune"
            begin fun out ->
              Format.fprintf out "; generated by why3find install@\n" ;
              Format.fprintf out "(install@\n" ;
              Format.fprintf out "  (package %s)@\n" pkg ;
              Format.fprintf out "  (section (site (why3find packages)))@\n" ;
              Format.fprintf out "  (files@\n" ;
              List.iter
                (fun src ->
                   Format.fprintf out "    (%s as %s/%s)@\n"
                     src pkg src
                ) (List.rev !sources) ;
              Format.fprintf out "    ))@." ;
            end ;
          Format.printf "install (dune)   dune@." ;
        end
    end

(* -------------------------------------------------------------------------- *)
(* --- compile                                                            --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"compile" ~args:"[-p PKG] FILE"
    begin fun argv ->
      usage argv
        "USAGE:\n\
         \n  why3find compile [OPTIONS] FILE\n\n\
         DESCRIPTION:\n\
         \n  Compile the given file(s) using why3 prove command.\n\n\
         OPTIONS:\n\
         \n  -v|--verbose print why3 command\
         \n  -p|--package PKG package dependency\
         \n  --extra-config FILE additional configuration file\
         \n";
      exec ~prefix:["prove";"--type-only"] ~configs:true argv
    end

(* -------------------------------------------------------------------------- *)
(* --- IDE                                                                --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"ide" ~args:"[-p PKG] FILE"
    begin fun argv ->
      usage argv
        "USAGE:\n\
         \n  why3find ide [OPTIONS] FILE\n\n\
         DESCRIPTION:\n\
         \n  Run why3 ide on the given file.\n\
         \n  Also loads the « hammer » strategy.\n\
         \n\
         OPTIONS:\n\
         \n  -v|--verbose print why3 command\
         \n  -p|--package PKG package dependency\
         \n  --extra-config FILE additional configuration file\
         \n";
      let hammer = Meta.shared "hammer.cfg" in
      exec ~prefix:["ide";"--extra-config";hammer] ~configs:true argv
    end

(* -------------------------------------------------------------------------- *)
(* --- REPLAY                                                             --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"replay" ~args:"[-p PKG] FILE"
    begin fun argv ->
      usage argv
        "USAGE:\n\
         \n  why3find extract [OPTIONS] MODULE...\n\n\
         DESCRIPTION:\n\
         \n  Executes why3 replay with the specified arguments.\n\n\
         OPTIONS:\n\
         \n  -v|--verbose print why3 command\
         \n  -p|--package PKG package dependency\
         \n  --extra-config FILE additional configuration file\
         \n";
      exec ~prefix:["replay"] ~configs:true argv
    end

(* -------------------------------------------------------------------------- *)
(* --- EXTRACT                                                            --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"extract" ~args:"[-p PKG] MODULE..."
    begin fun argv ->
      usage argv
        "USAGE:\n\
         \n  why3find extract [OPTIONS] MODULE...\n\n\
         DESCRIPTION:\n\
         \n  Executes why3 extract with the specified arguments.\n\n\
         OPTIONS:\n\
         \n  -v|--verbose print why3 command\
         \n  -p|--package PKG package dependency\
         \n  -D|--driver NAME|FILE additional extraction driver\
         \n  --extra-config FILE additional configuration file\
         \n";
      exec ~prefix:["extract"] ~configs:true ~drivers:true argv
    end

(* -------------------------------------------------------------------------- *)
(* --- CALIBRATE                                                          --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"calibrate" ~args:"[OPTIONS] PROVERS"
    begin fun argv ->
      let save = ref false in
      let velocity = ref false in
      let time = ref 500 in
      let prvs = ref [] in
      let prover p = prvs := p :: !prvs in
      Arg.parse_argv argv
        [
          "-j", Arg.Set_int Runner.jobs, "JOBS max parallel provers";
          "-c", Arg.Clear Runner.cache, "force cache update";
          "-q", Arg.Clear Calibration.parallel, "sequential calibration";
          "-t", Arg.Set_int time, "MS calibration time (default 500ms)";
          "-P", Arg.String prover, "PRV prover to calibrate";
          "-m", Arg.Set save, "save calibration profile (master)";
          "-v", Arg.Set velocity, "evaluate prover velocity (local)";
        ]
        prover
        "USAGE:\n\
         \n  why3find calibrate [OPTIONS] PROVERS\n\n\
         DESCRIPTION:\n\
         \n  Calibrate your machine.\
         \n  By default, report local velocity with respect to\
         \n  the master calibration profile, if available (same as -v).\
         \n  Otherwize, compute the local calibration profile (without -m).\
         \n\n\
         OPTIONS:\n" ;
      if !velocity then
        Calibration.velocity_provers (List.rev !prvs)
      else
        Calibration.calibrate_provers ~save:!save ~time:!time (List.rev !prvs)
    end

(* -------------------------------------------------------------------------- *)
(* --- PROVE                                                              --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"prove" ~args:"[OPTIONS] FILES"
    begin fun argv ->
      let pkgs = ref [] in
      let prvs = ref [] in
      let trfs = ref [] in
      let time = ref 5 in
      let files = ref [] in
      let ide = ref false in
      let session = ref false in
      let log = ref `Default in
      let mode = ref `Update in
      let set m v () = m := v in
      let add r p = r := p :: !r in
      Arg.parse_argv argv
        begin
          Wenv.args @ [
            "--local", Arg.Set Hammer.local, "no calibration (use this machine only)";
            "-p", Arg.String (add pkgs), "PKG package dependency";
            "-c", Arg.Clear Runner.cache, "force cache update";
            "-q", Arg.Clear Calibration.parallel, "sequential calibration";
            "-a", Arg.Unit (set mode `All), "rebuild all proofs";
            "-u", Arg.Unit (set mode `Update), "update proofs (default)";
            "-r", Arg.Unit (set mode `Replay), "replay proofs (no update)";
            "-i", Arg.Set ide, "run why-3 IDE on error (implies -s)";
            "-s", Arg.Set session, "save why3 session";
            "-j", Arg.Set_int Runner.jobs, "JOBS max running provers";
            "-t", Arg.Set_int time, "TIME acceptable prover timeout (default 5s)";
            "-P", Arg.String (add prvs), "PRV use prover";
            "-T", Arg.String (add trfs), "TRANS use transformation ";
            "--modules",  Arg.Unit (set log `Modules), "list results by module";
            "--theories", Arg.Unit (set log `Theories), "list results by theory";
            "--goals", Arg.Unit (set log `Goals), "list results by goals";
            "--proofs",   Arg.Unit (set log `Proofs), "list proofs by goals";
          ]
        end
        (add files)
        "USAGE:\n\
         \n  why3find prove [OPTIONS] FILES\n\n\
         DESCRIPTION:\n\
         \n  Prove why3 files.\n\n\
         OPTIONS:\n" ;
      let pkgs = List.rev !pkgs in
      let provers = List.rev !prvs in
      let transfs = List.rev !trfs in
      let session = !session || !ide in
      let files = Wenv.argv @@ List.rev !files in
      let tofix = Prove.command
          ~mode:!mode ~session ~log:!log
          ~time:!time ~provers ~transfs ~pkgs ~files
      in match tofix with
      | [] -> ()
      | f::_ ->
        if not !ide then
          exit 1
        else
          begin
            Format.printf "proof failed: running why3 ide %s@." f ;
            let hammer = Meta.shared "hammer.cfg" in
            exec ~prefix:["ide";"--extra-config";hammer] ~pkgs ~skip:0 [| f |]
          end
    end

(* -------------------------------------------------------------------------- *)
(* --- DOC                                                                --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"doc" ~args:"[-p PKG] FILE..."
    begin fun argv ->
      let pkgs = ref [] in
      let files = ref [] in
      let out = ref "html" in
      let add r p = r := p :: !r in
      Arg.parse_argv argv
        [
          "-o", Arg.Set_string out,
          "destination directory (default \"html\")" ;
          "-p", Arg.String (add pkgs), "package dependency"
        ]
        (add files)
        "USAGE:\n\
         \n  why3find query [PKG...]\n\n\
         DESCRIPTION:\n\
         \n  Query why3 package location.\n\n\
         OPTIONS:\n" ;
      let pkgs = List.rev !pkgs in
      let files = List.rev !files in
      let out = !out in
      Docgen.main ~pkgs ~files ~out
    end

(* -------------------------------------------------------------------------- *)
