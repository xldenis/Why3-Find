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
(* --- WHERE                                                              --- *)
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
(* --- SHARED                                                             --- *)
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
(* --- INIT                                                               --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"init"
    begin fun argv ->
      usage argv
        "USAGE:\n\
         \n  why3find init PKG [DIR]\n\n\
         DESCRIPTION:\n\
         \n  Create templates for dune and git for package PKG.\
         \n  Files are created in directory DIR (default ./PKG).\
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
    end

(* -------------------------------------------------------------------------- *)
(* --- LIST                                                               --- *)
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
(* --- QUERY                                                              --- *)
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
      else
        let pp_opt name ps =
          Format.printf "  @[<hov 2>%s: %s@]@\n" name
            (if ps = [] then "-" else String.concat ", " ps)
        in
        List.iter
          (fun (p : Meta.pkg) ->
             Format.printf "Package %s:@\n" p.name ;
             Format.printf "  path: %s@\n" p.path ;
             pp_opt "depends" p.depends ;
             pp_opt "configs" p.configs ;
             pp_opt "drivers" p.drivers ;
          ) pkgs
    end

(* -------------------------------------------------------------------------- *)
(* --- COMPILE                                                            --- *)
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
(* --- CONFIGURATION                                                      --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"config" ~args:"[OPTIONS] PROVERS"
    begin fun argv ->
      let list = ref false in
      let save = ref false in
      let strict = ref false in
      let relax = ref false in
      let calibrate = ref false in
      let velocity = ref false in
      Arg.parse_argv argv
        begin
          Wenv.options () @
          Runner.options @
          [
            "-m", Arg.Set calibrate, "calibrate provers (master)";
            "-v", Arg.Set velocity, "evaluate prover velocity (local)";
            "-l", Arg.Set list, "list final configuration";
            "-s", Arg.Set save, "save project configuration";
            "--relax", Arg.Set relax, "relax prover version constraints";
            "--strict", Arg.Set strict, "save strict prover versions";
          ]
        end
        (Utils.failwith "don't known what to do with %S")
        "USAGE:\n\
         \n  why3find config [OPTIONS]\n\n\
         DESCRIPTION:\n\
         \n  Configuration of the local package.\
         \n  By default, report on the current configuration.\
         \n\n\
         OPTIONS:\n" ;
      let env = Wenv.init () in
      Wenv.load () ;
      (* --- Configs ---- *)
      let cfgs = Wenv.configs () in
      if !list && cfgs <> [] then
        begin
          Format.printf "Extra Why-3 Configuration:@." ;
          List.iter (Format.printf " - %s@.") cfgs ;
        end ;
      (* --- Packages ---- *)
      let pkgs = Wenv.packages () in
      if !list && pkgs <> [] then
        begin
          Format.printf "Package Dependencies:@." ;
          List.iter (Format.printf " - %s@.") pkgs ;
        end ;
      (* --- Provers ----- *)
      let pconfig = Wenv.provers () in
      let list_provers = !list || !relax || !strict || pconfig = [] in
      if list_provers then Format.printf "Provers Configuration:@." ;
      let pconfig =
        if !relax then List.map Runner.relax pconfig else pconfig in
      let provers = Runner.select env @@ pconfig in
      if !calibrate then
        Calibration.calibrate_provers ~saved:!save env provers
      else
      if !velocity then
        Calibration.velocity_provers env provers ;
      let provers =
        if !strict then List.map Runner.id provers else
        if pconfig = [] then List.map Runner.name provers else
          pconfig in
      if list_provers && not !velocity && not !calibrate then
        List.iter (Format.printf " - %s@.") provers ;
      if provers = [] then
        Format.printf "  (no provers, use -P or why3 config detect)@." ;
      (* --- Transformations ----- *)
      let transfs = Wenv.transfs () in
      if !list && transfs <> [] then
        begin
          Format.printf "Proof Transformations:@." ;
          List.iter (Format.printf " - %s@.") transfs ;
        end ;
      (* --- Drivers ----- *)
      let drivers = Wenv.drivers () in
      if !list && drivers <> [] then
        begin
          Format.printf "Extraction Drivers:@." ;
          List.iter (Format.printf " - %s@.") drivers ;
        end ;
      (* --- Updating -------------- *)
      if !save then
        begin
          if Runner.is_modified () then
            Runner.save_config env ;
          if Wenv.is_modified () then
            begin
              Wenv.set_configs cfgs ;
              Wenv.set_packages pkgs ;
              Wenv.set_provers provers ;
              Wenv.set_transfs transfs ;
              Wenv.set_drivers drivers ;
              Wenv.save () ;
            end ;
        end
      else if !strict || !relax ||
              Wenv.is_modified () || Runner.is_modified () then
        Format.printf "Use '-s' to save project configuration.@." ;
    end

(* -------------------------------------------------------------------------- *)
(* --- PROVE                                                              --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"prove" ~args:"[OPTIONS] FILES"
    begin fun argv ->
      let files = ref [] in
      let ide = ref false in
      let session = ref false in
      let log = ref `Default in
      let mode = ref `Update in
      let time = ref 1.0 in
      let set m v () = m := v in
      let add r p = r := p :: !r in
      Arg.parse_argv argv
        begin
          Wenv.options () @
          Runner.options @
          [
            "-t", Arg.Set_float time, "TIME prover time (default 1.0s)";
            "-f", Arg.Unit (set mode `Force), "force rebuild proofs";
            "-u", Arg.Unit (set mode `Update), "update proofs (default)";
            "-r", Arg.Unit (set mode `Replay), "check proofs (no update)";
            "-m", Arg.Unit (set mode `Minimize), "minimize proofs (or update)";
            "-i", Arg.Set ide, "run why-3 IDE on error (implies -s)";
            "-s", Arg.Set session, "save why3 session";
            "--modules",  Arg.Unit (set log `Modules), "list results by module";
            "--theories", Arg.Unit (set log `Theories), "list results by theory";
            "--goals", Arg.Unit (set log `Goals), "list results by goals";
            "--proofs",   Arg.Unit (set log `Proofs), "list proofs by goals";
            "--local", Arg.Set Hammer.local, "no calibration (local times)";
          ]
        end
        (add files)
        "USAGE:\n\
         \n  why3find prove [OPTIONS] FILES\n\n\
         DESCRIPTION:\n\
         \n  Prove why3 files.\n\n\
         OPTIONS:\n" ;
      let session = !session || !ide in
      let files = Wenv.argv @@ List.rev !files in
      let tofix = Prove.prove_files
          ~mode:!mode ~session ~log:!log
          ~time:1.0 ~files
      in match tofix with
      | [] -> ()
      | f::_ ->
        if not !ide then
          exit 1
        else
          begin
            let pkgs = Wenv.packages () in
            Format.printf "proof failed: running why3 ide %s@." f ;
            let hammer = Meta.shared "hammer.cfg" in
            exec ~prefix:["ide";"--extra-config";hammer] ~pkgs ~skip:0 [| f |]
          end
    end

(* -------------------------------------------------------------------------- *)
(* --- DOC                                                                --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"doc" ~args:"[OPTIONS] FILE..."
    begin fun argv ->
      let files = ref [] in
      let out = ref "" in
      Arg.parse_argv argv
        begin
          Wenv.options ~packages:true ~drivers:true () @ [
            "-o", Arg.Set_string out,
            "destination directory (default \"html\")" ;
          ]
        end
        (fun f -> files := f :: !files)
        "USAGE:\n\
         \n  why3find query [PKG...]\n\n\
         DESCRIPTION:\n\
         \n  Query why3 package location.\n\n\
         OPTIONS:\n" ;
      let files = Wenv.argv @@ List.rev !files in
      let out = if !out = "" then "html" else Wenv.arg1 !out in
      Docgen.generate ~out ~files
    end

(* -------------------------------------------------------------------------- *)
(* --- INSTALL                                                            --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"install" ~args:"PKG [ARG...]"
    begin fun argv ->
      let args = ref [] in
      let dune = ref false in
      let html = ref true in
      Arg.parse_argv argv [
        "--dune", Arg.Set dune,"Generate dune installer" ;
        "--no-doc", Arg.Clear html,"Do not install documentation";
      ] (fun f -> args := f :: !args)
        "USAGE:\n\
         \n  why3find install [OPTIONS] PKG [FILE...]\n\n\
         DESCRIPTION:\n\
         \n  Install the package PKG at the topmost installation site.\
         \n\
         \n  Package dependencies and configuration are taken from the\
         \n  local project, or from the command line:\
         \n\
         \n    **/*.cfg extra why3 configuration\
         \n    **/*.drv OCaml extraction drivers\
         \n    PKG/**/*.mlw why3 source files\
         \n\
         \n  If no source file is given, all why3 source files\
         \n  in directory PKG will be installed.\
         \n\
         \n  Unless --no-doc is specified, documentation in './html'\
         \n  directory is also installed.
         \n\n\
         OPTIONS:\n" ;
      let pkg = get argv 0 "Missing PKG name" in
      let dune = !dune in
      let path = if dune then "." else Meta.path pkg in
      if not dune && Sys.file_exists path then
        begin
          Format.printf "remove %s@." pkg ;
          Utils.rmpath path ;
        end ;
      if not dune then Utils.mkdirs path ;
      let dunefiles = ref [] in
      let files = Wenv.argv @@ List.rev !args in
      let log ~kind src = Format.printf "install %-10s %s@." kind src in
      let install ~kind src =
        log ~kind src ;
        if dune
        then dunefiles := src :: !dunefiles
        else Utils.copy ~src ~tgt:(Filename.concat path src)
      in
      let prefix = Filename.concat pkg "" in
      let allsrc = ref true in
      List.iter
        begin fun src ->
          match Filename.extension src with
          | ".mlw" ->
            if not (String.starts_with ~prefix src) then
              Utils.failwith "Can not install %S from outside of %S"
                src prefix ;
            allsrc := false ;
            install ~kind:"(source)" src ;
          | ".cfg" -> Wenv.add_config src
          | ".drv" -> Wenv.add_driver src
          | _ ->
            Utils.failwith "don't know what to do with %S" src
        end files ;
      let depends = Wenv.packages () in
      let drivers = Wenv.drivers () in
      let configs = Wenv.configs () in
      if !allsrc then
        Utils.iterpath
          ~file:(fun f ->
              if Filename.extension f = ".mlw" then
                install ~kind:"(source)" f
            ) pkg ;
      List.iter (install ~kind:"(config)") configs ;
      List.iter (install ~kind:"(driver)") drivers ;
      if !html then
        Utils.iterpath ~file:(install ~kind:"(doc)") "html" ;
      log ~kind:"(meta)" "META.json" ;
      Meta.install { name = pkg ; path ; depends ; drivers ; configs } ;
      if dune then
        begin
          generate "dune"
            begin fun out ->
              Format.fprintf out ";; generated by why3find@\n" ;
              Format.fprintf out "(install@\n" ;
              Format.fprintf out "  (package %s)@\n" pkg ;
              Format.fprintf out "  (section (site (why3find packages)))@\n" ;
              Format.fprintf out "  (files@\n" ;
              List.iter
                (fun src -> Format.fprintf out "    (%s as %s)@\n" src src)
                ("META.json" :: List.rev !dunefiles) ;
              Format.fprintf out "    ))@." ;
            end ;
          Format.printf "install (dune)   dune@." ;
        end
    end

(* -------------------------------------------------------------------------- *)
(* --- UNINSTALL                                                          --- *)
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
            Utils.rmpath path ;
          end
      done
    end

(* -------------------------------------------------------------------------- *)
