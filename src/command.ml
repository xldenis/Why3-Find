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
  Format.printf "Generated %s@." (Utils.absolute tgt)

(* -------------------------------------------------------------------------- *)
(* --- Wrapper Command                                                    --- *)
(* -------------------------------------------------------------------------- *)

let verbose = ref false

let exec
    ?(cmd="why3")
    ?(auto=false)
    ?(configs)
    ?(drivers)
    ?(prefix=[])
    ?(pkgs=[])
    ?(skip=0)
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
      | "--drivers" when auto && !drivers = None -> drivers := Some []
      | "--configs" when auto && !configs = None -> configs := Some []
      | "-v" | "--verbose" -> verbose := true
      | arg ->
        args += arg
    end ; incr p
  done ;
  let pkgs = Meta.find_all (Bag.to_list !pkgs) in
  let cfg =
    match !configs with
    | None -> Bag.empty
    | Some cs ->
      Bag.map (Printf.sprintf "--extra-config=%s") cs ++
      Bag.merge
        (fun (pkg : Meta.pkg) ->
           Bag.map
             (Printf.sprintf "--extra-config=%s/%s" pkg.path)
             pkg.configs
        ) pkgs in
  let drv =
    match !drivers with
    | None -> Bag.empty
    | Some ds ->
      Bag.map (Printf.sprintf "--driver=%s") ds ++
      Bag.merge
        (fun (pkg : Meta.pkg) ->
           Bag.map
             (Printf.sprintf "--driver=%s/%s" pkg.path)
             pkg.drivers
        ) pkgs in
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
    exec ~cmd ~auto:true ~skip:1 argv
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
         \n  Create templates for dune-project and git-ignore for package PKG.\
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
             Utils.readdir
               (fun pkg -> Format.printf "%s/%s@\n" site pkg)
               site
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
          if ps <> [] then
            begin
              Format.printf "  @[<hov 2>%s:" name ;
              List.iter (Format.printf "@ %s") ps ;
              Format.printf "@]@\n" ;
            end in
        let pp_yes name fg =
          if fg then Format.printf "  %s: yes@\n" name in
        List.iter
          (fun (p : Meta.pkg) ->
             Format.printf "Package %s:@\n" p.name ;
             Format.printf "  path: %s@\n" p.path ;
             pp_opt "depends" p.depends ;
             pp_opt "configs" p.configs ;
             pp_opt "drivers" p.drivers ;
             pp_yes "extracted" p.extracted ;
             pp_yes "symbols" (p.extracted && p.symbols) ;
          ) pkgs
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
      let detect = ref false in
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
            "--detect", Arg.Set detect, "update why3 config detect";
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
      (* --- Why3 Config --- *)
      if !detect then ignore @@ Sys.command "why3 config detect" ;
      let env = Wenv.init () in
      Wenv.load () ;
      (* --- Extra Configs ---- *)
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
      let list_provers = !list || !relax || !strict in
      if list_provers then
        begin
          let j = Runner.maxjobs env in
          Format.printf "Provers Configuration:@." ;
          Format.printf " - proof time: %a@." Utils.pp_time (Wenv.time ()) ;
          Format.printf " - proof depth: %d@." (Wenv.depth ()) ;
          Format.printf " - parallel provers: %d@." j ;
        end ;
      let pconfig = if !relax then List.map Runner.relax pconfig else pconfig in
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
      if provers = [] then
        Format.printf "  (no provers, use -P or why3 config detect)@."
      else
      if list_provers && not !velocity && not !calibrate then
        if List.for_all Runner.relaxed provers then
          Format.printf " - provers: %s@." (String.concat ", " provers)
        else
          List.iter (Format.printf " - %s@.") provers ;
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
      else
        let project = !strict || !relax || Wenv.is_modified () in
        let local = Runner.is_modified () in
        let target =
          match project, local with
          | false,false -> None
          | true,false -> Some "project configuration"
          | false,true -> Some "local configuration"
          | true,true -> Some "project and local configurations"
        in Option.iter (Format.printf "Use '-s' to save %s@.") target
    end

(* -------------------------------------------------------------------------- *)
(* --- PROVE                                                              --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"prove" ~args:"[OPTIONS] PATH..."
    begin fun argv ->
      let files = ref [] in
      let ide = ref false in
      let session = ref false in
      let log = ref `Default in
      let mode = ref `Update in
      let axioms = ref false in
      let set m v () = m := v in
      let add r p = r := p :: !r in
      Arg.parse_argv argv
        begin
          Wenv.options () @
          Runner.options @
          [
            "-f", Arg.Unit (set mode `Force), "force rebuild proofs";
            "-u", Arg.Unit (set mode `Update), "update proofs (default)";
            "-r", Arg.Unit (set mode `Replay), "replay proofs (no update)";
            "-m", Arg.Unit (set mode `Minimize), "minimize proofs (or update)";
            "-i", Arg.Set ide, "run why-3 IDE on error(s) (implies -s)";
            "-s", Arg.Set session, "save why3 session";
            "-h", Arg.Set axioms, "report hypotheses and axioms";
            "--local", Arg.Set Hammer.local, "no calibration (local times)";
            "--modules",  Arg.Unit (set log `Modules), "list results by module";
            "--theories", Arg.Unit (set log `Theories), "list results by theory";
            "--goals", Arg.Unit (set log `Goals), "list results by goals";
            "--proofs", Arg.Unit (set log `Proofs), "list proofs by goals";
            "--stdlib", Arg.Set Prove.stdlib, "report hypotheses from stdlib";
            "--extern", Arg.Set Prove.externals, "report assumed external symbols";
            "--builtin", Arg.Set Prove.builtins, "report assumed builtin symbols";
          ]
        end
        (add files)
        "USAGE:\n\
         \n  why3find prove [OPTIONS] PATH...\n\n\
         DESCRIPTION:\n\
         \n  Prove all why3 files and directories accessible from PATH.\n\n\
         OPTIONS:\n" ;
      let session = !session || !ide in
      let files = Wenv.argfiles ~exts:[".mlw"] @@ List.rev !files in
      let tofix = Prove.prove_files
          ~mode:!mode ~session ~log:!log ~axioms:!axioms ~files
      in
      match tofix with
      | [] -> ()
      | f::_ ->
        if not !ide then
          exit 1
        else
          begin
            let pkgs = Wenv.packages () in
            Format.printf "proof failed: running why3 ide %s@." f ;
            let hammer = Meta.shared "hammer.cfg" in
            exec ~prefix:["ide";"--extra-config";hammer] ~pkgs [| f |]
          end
    end

(* -------------------------------------------------------------------------- *)
(* --- DOC                                                                --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"doc" ~args:"[OPTIONS] PATH..."
    begin fun argv ->
      let files = ref [] in
      let title = ref "" in
      let out = ref "" in
      Arg.parse_argv argv
        begin
          Wenv.options ~packages:true ~drivers:true () @ [
            "-t", Arg.Set_string title, "document title (default none)" ;
            "-o", Arg.Set_string out,
            "destination directory (default \"html\")" ;
          ]
        end
        (fun f -> files := f :: !files)
        "USAGE:\n\
         \n  why3find doc [OPTIONS] PATH...\n\n\
         DESCRIPTION:\n\
         \n  Generate HTML documentation.\
         \n\
         \n  Includes all why3 sources and markdown pages\
         \n  accessible from PATH.\n\n\
         \n\
         OPTIONS:\n" ;
      let title = !title in
      let files = Wenv.argfiles ~exts:[".md";".mlw"] @@ List.rev !files in
      let out = if !out = "" then "html" else Wenv.arg1 !out in
      Docgen.generate ~out ~title ~files
    end

(* -------------------------------------------------------------------------- *)
(* --- EXTRACT                                                            --- *)
(* -------------------------------------------------------------------------- *)

let re_module = Str.regexp "\\([a-zA-Z0x-9_]+.\\)+[A-Z][a-zA-Z0-9_]*"

let () = register ~name:"extract" ~args:"[OPTIONS] MODULE..."
    begin fun argv ->
      let pkg = ref "" in
      let modules = ref [] in
      let libraries = ref [] in
      let symbols = ref false in
      let out = ref "lib" in
      Arg.parse_argv argv
        begin
          Wenv.options ~packages:true ~drivers:true () @ [
            "-l", Arg.String (fun d -> libraries := d :: !libraries),
            "PKG Additional OCaml library dependency";
            "-o", Arg.Set_string out,
            "destination directory (default \"lib\")" ;
            "-s", Arg.Set symbols,
            "generate symbol maps for ppx_why3find" ;
            "-v", Arg.Set verbose,
            "print why3 extract command" ;
          ]
        end
        (fun m ->
           if not @@ Str.string_match re_module m 0 then
             Utils.failwith "%S is not a module name" m ;
           let mp = List.hd @@ String.split_on_char '.' m in
           if !pkg = "" then
             pkg := mp
           else
           if !pkg <> mp then
             Utils.failwith "%S is not in package %S" m !pkg ;
           modules := m :: !modules)
        "USAGE:\n\
         \n  why3find extract [OPTIONS] PKG MODULE...\n\n\
         DESCRIPTION:\n\
         \n  Extract OCaml and generate Dune file.\
         \n\n\
         OPTIONS:\n" ;
      if !pkg = "" then failwith "Nothing to extract" ;
      let pkgs = Wenv.packages () in
      let configs = Wenv.configs () in
      let drivers = Wenv.drivers () in
      let libraries =
        List.rev !libraries @
        List.filter (fun pkg -> (Meta.find pkg).extracted) pkgs in
      Utils.rmpath !out ;
      Utils.mkdirs !out ;
      let dune = Filename.concat !out "dune" in
      let cdune = open_out (Filename.concat !out "dune") in
      let fdune = Format.formatter_of_out_channel cdune in
      Format.fprintf fdune "(* Generated by why3find extract *)@\n" ;
      Format.fprintf fdune
        "(library@\n  (name %s)@\n  (public_name %s)"
        !pkg !pkg ;
      if libraries <> [] then
        begin
          Format.fprintf fdune "@\n  (@[<hov 2>libraries" ;
          List.iter (Format.fprintf fdune "@ %s") @@ libraries ;
          Format.fprintf fdune "@])" ;
        end ;
      Format.fprintf fdune ")@." ;
      close_out cdune ;
      Format.printf "Generated %s@." (Utils.absolute dune) ;
      let prefix =
        List.append
          ["extract";"-D";"ocaml64";"-o";!out;"--modular"]
          (if !symbols then ["-symbols"] else []) in
      let argv = Array.of_list @@ List.rev !modules in
      exec ~prefix ~pkgs ~configs ~drivers argv
    end

(* -------------------------------------------------------------------------- *)
(* --- SERVER                                                             --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"server" ~args:"OPTIONS"
    begin fun argv ->
      let age = ref 0 in
      let database = ref "why3server" in
      let frontend = ref "tcp://*:5555" in
      let backend = ref "tcp://*:5556" in
      let hangup = ref 10 in
      let url kind value =
        Printf.sprintf "URL %s connection address (default %S)" kind !value
      in
      Arg.parse_argv argv [
        "--prune",Arg.Set_int age,"AGE Prune old cache generations@.";
        "--database",Arg.Set_string database, "DIR Database (default \"why3server\")";
        "--frontend",Arg.Set_string frontend, url "Client" frontend;
        "--backend", Arg.Set_string backend,  url "Worker" backend;
        "--hangup",  Arg.Set_int hangup,
        "Connection timeout (in minutes, default 10')";
      ] failwith
        "USAGE:\n\
         \n  why3find server [OPTIONS]\n\n\
         DESCRIPTION:\n\
         \n  Establishes a proof server.\
         \n\n\
         OPTIONS:\n" ;
      if !age > 0 then Server.prune_database !database !age ;
      Server.establish
        ~database:!database
        ~frontend:!frontend
        ~backend:!backend
        ~hangup:!hangup
    end

(* -------------------------------------------------------------------------- *)
(* --- INSTALL                                                            --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"install" ~args:"PKG PATH..."
    begin fun argv ->
      let args = ref [] in
      let dune = ref true in
      let lib = ref "lib" in
      let html = ref "html" in
      let nodoc () = html := "" in
      Arg.parse_argv argv [
        "--dune", Arg.Set dune,"Generate dune installer (default)" ;
        "--global", Arg.Clear dune,"Install in global repository (why3find where)" ;
        "--lib", Arg.Set_string lib,"DIR extraction directory (why3find extract -o DIR)";
        "--doc", Arg.Set_string html,"DIR doc output directory (why3find doc -o DIR)";
        "--no-doc", Arg.Unit nodoc,"Do not install documentation";
      ] (fun f -> args := f :: !args)
        "USAGE:\n\
         \n  why3find install [OPTIONS] PKG PATH...\n\n\
         DESCRIPTION:\n\n\
         \n  Install the package PKG at the topmost installation site.\
         \n\
         \n  Package dependencies and configuration are taken from the\
         \n  local project, or from the command line:\
         \n\
         \n    **/*.cfg extra why3 configuration\
         \n    **/*.drv OCaml extraction drivers\
         \n    DIR all why3 source files in DIR\
         \n    PKG/**/*.mlw why3 source files\
         \n\
         \n  If no source file is given, all why3 source files\
         \n  in directory PKG will be installed.\
         \n\
         \n  Unless --no-doc is specified, documentation in './html'\
         \n  directory is also installed.\
         \n\n\
         OPTIONS:\n" ;
      let pkg,paths =
        match List.rev !args with
        | pkg::paths -> pkg, Wenv.argv paths
        | [] -> failwith "Missing PKG name" in
      let dune = !dune in
      let path = if dune then "." else Meta.path pkg in
      if not dune && Sys.file_exists path then
        Utils.rmpath path ;
      if not dune then Utils.mkdirs path ;
      let dunefiles = ref [] in
      let log ~kind src = Format.printf "install %-10s %s@." kind src in
      let install ~kind ?tgt src =
        let tgt = match tgt with Some p -> p | None -> src in
        log ~kind tgt ;
        if dune
        then dunefiles := (src,tgt) :: !dunefiles
        else Utils.copy ~src ~tgt:(Filename.concat path tgt)
      in
      let allsrc = ref true in
      let pkg_prefix = String.starts_with ~prefix:(Filename.concat pkg "") in
      let install_src ?(check=true) src =
        begin
          if check && not (pkg_prefix src) then
            Utils.failwith "Can not install %S from outside of %s directory"
              src pkg ;
          allsrc := false ;
          Wenv.allfiles ~exts:[".mlw"] (install ~kind:"(source)") src ;
          let proofs = Filename.(concat (chop_extension src) "proof.json") in
          if Sys.file_exists proofs then
            install ~kind:"(proof)" proofs
        end in
      List.iter
        begin fun src ->
          if not @@ Sys.file_exists src then
            Utils.failwith "unknown file or directory %S" src ;
          if Sys.is_directory src then
            Wenv.allfiles ~exts:[".mlw"] install_src src
          else
            match Filename.extension src with
            | ".mlw" -> install_src src
            | ".cfg" -> Wenv.add_config src
            | ".drv" -> Wenv.add_driver src
            | _ ->
              Utils.failwith "don't know what to do with %S" src
        end paths ;
      let depends = Wenv.packages () in
      let drivers = Wenv.drivers () in
      let configs = Wenv.configs () in
      if !allsrc then Wenv.allfiles ~exts:[".mlw"]
          (install_src ~check:false) pkg ;
      List.iter (install ~kind:"(config)") configs ;
      List.iter (install ~kind:"(driver)") drivers ;
      let doc = !html in
      if doc <> "" then
        begin
          let rec install_doc src tgt =
            if Sys.file_exists src then
              if Sys.is_directory src then
                Utils.readdir (fun d ->
                    let src = Filename.concat src d in
                    let tgt = Filename.concat tgt d in
                    install_doc src tgt
                  ) src
              else
                install ~kind:"(html)" ~tgt src
          in install_doc doc "html" ;
        end ;
      let lib = !lib in
      let haslib = ref false in
      let hasppx = ref false in
      if Sys.file_exists lib && Sys.is_directory lib then
        begin
          Utils.readdir (fun d ->
              let src = Filename.concat lib d in
              if Filename.check_suffix src ".json" then
                begin
                  let tgt = Filename.concat "lib" d in
                  install ~kind:"(ppx)" ~tgt src ;
                  hasppx := true ;
                  haslib := true ;
                end
              else if Filename.check_suffix src ".ml" then
                haslib := true ;
            ) lib
        end ;
      let extracted = !haslib in
      let symbols = !hasppx in
      if dune && extracted then
        log ~kind:"(dune)" "extracted code" ;
      log ~kind:"(meta)" "META.json" ;
      Meta.install {
        name = pkg ; path ; depends ; drivers ; configs ; extracted ; symbols ;
      } ;
      if dune then
        begin
          let meta = "META.json" in
          Format.printf "Generated %s@." (Utils.absolute meta);
          generate "dune"
            begin fun out ->
              Format.fprintf out ";; generated by why3find@\n" ;
              Format.fprintf out "(install@\n" ;
              Format.fprintf out "  (package %s)@\n" pkg ;
              Format.fprintf out "  (section (site (why3find packages)))@\n" ;
              Format.fprintf out "  (files@\n" ;
              List.iter
                (fun (src,tgt) ->
                   Format.fprintf out "    (%s as %s/%s)@\n" src pkg tgt)
                ((meta,meta) :: List.rev !dunefiles) ;
              Format.fprintf out "    ))@." ;
            end ;
          Format.printf "Generated %s@." (Utils.absolute "dune");
        end
      else
        begin
          Format.printf "Installed %s@." (Utils.absolute path);
          if extracted then
            Format.eprintf "Warning: extracted code not installed (use dune)@." ;
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
            Format.printf "remove %s@." path ;
            Utils.rmpath path ;
          end
        else
          Format.printf "Warning: package %s not found@." pkg
      done
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
      exec ~prefix:["prove";"--type-only"] ~configs:[] ~skip:1 argv
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
      exec ~prefix:["ide";"--extra-config";hammer] ~configs:[] ~skip:1 argv
    end

(* -------------------------------------------------------------------------- *)
(* --- REPLAY                                                             --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"replay" ~args:"[-p PKG] FILE"
    begin fun argv ->
      usage argv
        "USAGE:\n\
         \n  why3find replay [OPTIONS] MODULE...\n\n\
         DESCRIPTION:\n\
         \n  Executes why3 replay with the specified arguments.\n\n\
         OPTIONS:\n\
         \n  -v|--verbose print why3 command\
         \n  -p|--package PKG package dependency\
         \n  --extra-config FILE additional configuration file\
         \n";
      exec ~prefix:["replay"] ~configs:[] ~skip:1 argv
    end

(* -------------------------------------------------------------------------- *)
