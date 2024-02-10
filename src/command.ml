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

let generate_format file gen =
  let out = open_out file in
  let fmt = Format.formatter_of_out_channel out in
  begin
    gen fmt ;
    Format.pp_print_flush fmt () ;
    close_out out ;
  end

let generate_or_suggest file gen =
  match open_out_gen [Open_wronly; Open_creat; Open_excl] 0o666 file with
  | exception Sys_error _ ->
     Format.printf "#### Suggested %s:@." (Filename.basename file) ;
     gen stdout ;
     Format.printf "####@."
  | out ->
     gen out ;
     close_out out ;
     Format.printf "Generated %s@." (Utils.absolute file)

let var = Str.regexp "%{\\([a-zA-Z]+\\)}"

let template ~subst ~inc ~out =
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
  in walk ()

let template ~subst ~src ~tgt =
  Utils.mkdirs (Filename.dirname tgt) ;
  let inc = open_in src in
  generate_or_suggest tgt (fun out -> template ~subst ~inc ~out) ;
  close_in inc

let contains ~pattern ~text =
  let regexp = Str.regexp_string pattern in
  match Str.search_forward regexp text 0 with
  | exception Not_found -> false
  | _ -> true

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
  let args = ref [] in
  let pkgs = ref pkgs in
  let drivers = ref drivers in
  let configs = ref configs in
  let p = ref skip in
  while !p < Array.length argv do
    begin
      match argv.(!p) with
      | "-p" | "--package" ->
        incr p ;
        if !p < Array.length argv then
          pkgs := argv.(!p) :: !pkgs
        else
          failwith "missing PKG name"
      | "--drivers" when auto && !drivers = None -> drivers := Some []
      | "--configs" when auto && !configs = None -> configs := Some []
      | "-v" | "--verbose" -> verbose := true
      | arg -> args := arg :: !args
    end ; incr p
  done ;
  let pkgs = Meta.find_all (List.rev !pkgs) in
  let cfg =
    match !configs with
    | None -> []
    | Some cs ->
      List.map (Printf.sprintf "--extra-config=%s") cs @
      List.concat @@ List.map
        (fun (pkg : Meta.pkg) ->
           List.map
             (Printf.sprintf "--extra-config=%s/%s" pkg.path)
             pkg.configs
        ) pkgs in
  let drv =
    match !drivers with
    | None -> []
    | Some ds ->
      List.map (Printf.sprintf "--driver=%s") ds @
      List.concat @@ List.map
        (fun (pkg : Meta.pkg) ->
           List.map
             (Printf.sprintf "--driver=%s/%s" pkg.path)
             pkg.drivers
        ) pkgs in
  let load =
    List.map
      (fun (pkg : Meta.pkg) -> Printf.sprintf "--library=%s" pkg.path)
      pkgs in
  let argv =
    Array.of_list @@ List.concat [
      cmd::prefix ; cfg ; [ "-L" ; "." ] ; load ; drv ; List.rev !args
    ] in
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

let noargv = Utils.failwith "Don't know what to do with %S"

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
        ~src:(Meta.shared "gitignore.template")
        ~tgt:(Filename.concat dir ".gitignore") ;
      template
        ~subst
        ~src:(Meta.shared "dune-project.template")
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
          ) pkgs
    end

(* -------------------------------------------------------------------------- *)
(* --- CONFIGURATION                                                      --- *)
(* -------------------------------------------------------------------------- *)

let pp_list =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
    Format.pp_print_string

let rec configuration configs provers =
  match configs , provers with
  | [] , ps -> List.map Runner.infoname ps
  | cs , [] -> List.map (Printf.sprintf "(?%s)") cs
  | c::cs , p::ps ->
    let cn = Wenv.name c in
    let pn = Runner.name p in
    if cn <> pn then Printf.sprintf "(?%s)" c :: configuration cs provers
    else
      let q = Runner.fullname p in
      if c = q then q :: configuration cs ps else
        Runner.infoname p :: configuration cs ps

let () = register ~name:"config" ~args:"[OPTIONS] PROVERS"
    begin fun argv ->
      let list = ref true in
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
            "--quiet", Arg.Clear list, "do not list final configuration";
            "--detect", Arg.Set detect, "detect and update why3 config";
          ]
        end
        noargv
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
          Format.printf "Extra Why3 Configuration:@." ;
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
      let time = Wenv.time () in
      let patterns = Wenv.provers () in
      let provers = Runner.select env ~patterns in
      let pnames = configuration patterns provers in
      if !calibrate then
        Calibration.calibrate_provers ~saved:true env provers
      else
      if !velocity then
        Calibration.velocity_provers env provers ;
      (* --- Transformations ----- *)
      let depth = Wenv.depth () in
      let tactics = Wenv.tactics () in
      (* --- Drivers ----- *)
      let drivers = Wenv.drivers () in
      (* --- Printing -------------- *)
      if !list then
        begin
          let jobs = Runner.maxjobs env in
          let time = Wenv.time () in
          Format.printf "Configuration:@." ;
          Format.printf " - runner: %d jobs, %a@."
            jobs Utils.pp_time time ;
          if provers <> [] then
            Format.printf " - @[<hov 2>provers: %a@]@." pp_list pnames ;
        end ;
      if !list && tactics <> [] then
        Format.printf " - @[<hov 2>tactics: %a@ (depth %d)@]@."
          pp_list tactics (Wenv.depth ());
      if !list && drivers <> [] then
        Format.printf " - @[<hov 2>drivers: %a@]@." pp_list drivers ;
      (* --- Updating -------------- *)
      if Runner.is_modified () then
        Runner.save_config env ;
      if Wenv.is_modified () then
        begin
          Wenv.set_time time ;
          Wenv.set_depth depth ;
          Wenv.set_configs cfgs ;
          Wenv.set_packages pkgs ;
          Wenv.set_provers patterns ;
          Wenv.set_tactics tactics ;
          Wenv.set_drivers drivers ;
        end ;
      Wenv.save () ;
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
            "-i", Arg.Set ide, "run Why3 IDE on error(s) (implies -s)";
            "-s", Arg.Set session, "save why3 session";
            "-a", Arg.Set axioms, "report axioms and parameters";
          ] @
          Client.options @ [
            "--modules",  Arg.Unit (set log `Modules), "list results by module";
            "--theories", Arg.Unit (set log `Theories), "list results by theory";
            "--goals", Arg.Unit (set log `Goals), "list results by goals";
            "--proofs", Arg.Unit (set log `Proofs), "list proofs by goals";
            "--stdlib", Arg.Set Prove.stdlib, "report hypotheses from stdlib";
            "--extern", Arg.Set Prove.externals, "report also external symbols";
            "--builtin", Arg.Set Prove.builtins, "report also builtin symbols";
          ]
        end
        (add files)
        "USAGE:\n\
         \n  why3find prove [OPTIONS] PATH...\n\n\
         DESCRIPTION:\n\
         \n  Prove all why3 files and directories accessible from PATH.\n\n\
         OPTIONS:\n" ;
      let session = !session || !ide in
      let files = if !files = [] then ["."] else !files in
      let files = Wenv.argfiles ~exts:[".mlw"] @@ List.rev files in
      let result = Prove.prove_files
          ~mode:!mode ~session ~log:!log ~axioms:!axioms ~files in
      let n = List.length result.unfixed in
      if n > 0 then
        begin
          Format.printf "Error: %d unproved file(s)@." n ;
          if not !ide then exit 1 ;
          let file = List.hd result.unfixed in
          let pkgs = Wenv.packages () in
          let Prove.{ provers ; time ; mem ; tactics } = result in
          let cfg = Hammer.config ~tactics ~provers ~time ~mem in
          Format.printf "Fixing %s@." file ;
          exec ~prefix:["ide"] ~pkgs [| "--extra-config" ; cfg ; file |]
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
      let url = ref false in
      Arg.parse_argv argv
        begin
          Wenv.options ~packages:true ~drivers:true () @ [
            "-t", Arg.Set_string title, "TITLE document title (default none)" ;
            "-o", Arg.Set_string out,
            "DIR destination directory (default \"html\")" ;
            "-u", Arg.Set url, "output generated URI" ;
            "--url", Arg.String Id.set_package_url ,
            "URL prefix URL for external packages."
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
      let files = if !files = [] then ["."] else !files in
      let files = Wenv.argfiles ~exts:[".md";".mlw"] @@ List.rev files in
      let out = if !out = "" then "html" else Wenv.arg1 !out in
      Docgen.generate ~out ~title ~files ~url:!url
    end

(* -------------------------------------------------------------------------- *)
(* --- EXTRACT                                                            --- *)
(* -------------------------------------------------------------------------- *)

let re_module = Str.regexp "\\([a-zA-Z0x-9_]+.\\)+[A-Z][a-zA-Z0-9_]*"

let () = register ~name:"extract" ~args:"[OPTIONS] MODULE..."
    begin fun argv ->
      let lib = ref false in
      let pkg = ref "" in
      let modules = ref [] in
      let libraries = ref [] in
      let out = ref "lib" in
      Arg.parse_argv argv
        begin
          Wenv.options ~packages:true ~drivers:true () @ [
            "--lib", Arg.Set lib,
            "Generate PKG.lib library instead of PKG";
            "-l", Arg.String (fun d -> libraries := d :: !libraries),
            "PKG Additional OCaml library dependency";
            "-o", Arg.Set_string out,
            "destination directory (default \"lib\")" ;
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
         \n  why3find extract [OPTIONS] MODULE...\n\n\
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
      Format.fprintf fdune ";; Generated by why3find extract@\n(library" ;
      let name = if !lib then !pkg ^ "__lib" else !pkg in
      let public = if !lib then !pkg ^ ".lib" else !pkg in
      Format.fprintf fdune
        "@\n  (name %s)@\n  (public_name %s)@\n  (wrapped false)"
        name public ;
      if libraries <> [] then
        begin
          Format.fprintf fdune "@\n  (@[<hov 2>libraries" ;
          List.iter (Format.fprintf fdune "@ %s") @@ libraries ;
          Format.fprintf fdune "@])" ;
        end ;
      Format.fprintf fdune ")@\n" ;
      Format.pp_print_flush fdune () ;
      close_out cdune ;
      Format.printf "Generated %s@." (Utils.absolute dune) ;
      let prefix = ["extract";"-D";"ocaml64";"-o";!out;"--modular"] in
      let argv = Array.of_list @@ List.rev !modules in
      exec ~prefix ~pkgs ~configs ~drivers argv
    end

(* -------------------------------------------------------------------------- *)
(* --- SERVER                                                             --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"server" ~args:"OPTIONS"
    begin fun argv ->
      let stats = ref false in
      let prune = ref 0 in
      let database = ref "why3server" in
      let address = ref "tcp://*:5555" in
      let polling = ref 1.0 in
      Arg.parse_argv argv [
        "--stats",Arg.Set stats,"Print cache disk usage";
        "--prune",Arg.Set_int prune,
        "AGE Prune cache generations older than AGE";
        "--address",Arg.Set_string address,
        "URL server address (default \"tcp://*:5555\")";
        "--database",Arg.Set_string database,
        "DIR Database (default \"why3server\")";
        "--polling",Arg.Set_float polling,
        "TIME server polling interval (default 1.0s)";
        "--trace",Arg.Set Server.trace,"Trace server protocol";
      ] noargv
        "USAGE:\n\
         \n  why3find server [OPTIONS]\n\n\
         DESCRIPTION:\n\
         \n  Establishes a proof server.\
         \n\n\
         OPTIONS:\n" ;
      Utils.flush () ;
      let prune = !prune in
      let address = !address in
      let database = !database in
      let polling = !polling in
      Format.printf "Address  %s@." address ;
      Format.printf "Database %s@." (Utils.absolute database) ;
      if prune > 0 then Server.prune ~database ~age:prune ;
      if !stats then
        begin
          Format.printf "Disk usage:@." ;
          if Sys.file_exists database then
            ignore @@ Sys.command ("du -h -d 1 " ^ database)
          else
            Format.printf "  (empty)@."
        end ;
      Server.establish ~address ~database ~polling ;
    end

(* -------------------------------------------------------------------------- *)
(* --- WORKER                                                             --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"worker" ~args:"OPTIONS"
    begin fun argv ->
      let server = ref "tcp://localhost:5555" in
      let polling = ref 1.0 in
      Arg.parse_argv argv
        begin
          Runner.options @ [
            "--server",Arg.Set_string server,
            "URL proof server address (default \"tcp://localhost:5555\")";
            "--polling",Arg.Set_float polling,
            "TIME server polling interval (default 1.0s)";
            "--trace",Arg.Set Worker.trace,"Trace server protocol";
          ] @
          Calibration.options
        end
        noargv
        "USAGE:\n\
         \n  why3find worker [OPTIONS]\n\n\
         DESCRIPTION:\n\
         \n  Provides a worker for the specified proof server.\
         \n\n\
         OPTIONS:\n" ;
      Worker.connect ~server:!server ~polling:!polling ;
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
         \n    PKG/**         all why3 source files\
         \n    PKG/**/*.mlw   specified why3 source files\
         \n    **/*.cfg       extra why3 configuration\
         \n    **/*.drv       OCaml extraction drivers\
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
            install ~kind:"(proof)" proofs ;
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
      if Sys.file_exists lib && Sys.is_directory lib then
        begin
          Utils.readdir (fun d ->
              let src = Filename.concat lib d in
              if Filename.check_suffix src ".ml" then haslib := true ;
            ) lib
        end ;
      let extracted = !haslib in
      if dune && extracted then log ~kind:"(dune)" "extracted code" ;
      log ~kind:"(meta)" "META.json" ;
      Meta.install {
        name = pkg ; path ; depends ; drivers ; configs ; extracted ;
      } ;
      if dune then
        begin
          let meta = "META.json" in
          Format.printf "Generated %s@." (Utils.absolute meta);
          generate_format "dune.why3find"
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
          Format.printf "Generated %s@." (Utils.absolute "dune.why3find");
          let include_stanza = "(include dune.why3find)" in
          let already_included =
            try
              let dune = Utils.readfile ~file:"dune" in
              contains ~pattern:include_stanza ~text:dune
            with Sys_error _ -> false in
          if not already_included then
            generate_or_suggest "dune"
              (fun out -> output_string out (include_stanza ^ "\n"));
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
         \n\
         OPTIONS:\n\
         \n  -v|--verbose print why3 command\
         \n  -p|--package PKG package dependency\
         \n  --extra-config FILE additional configuration file\
         \n";
      exec ~prefix:["ide"] ~configs:[] ~skip:1 argv
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
