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

let usage argv msg =
  Array.iter (function
      | "-h" | "-help" | "--help" -> raise @@ Stdlib.Arg.Help msg
      | _ -> ()
    ) argv

let iter f = List.iter (fun (cmd,(args,_)) -> f cmd args) (List.rev !commands)

(* -------------------------------------------------------------------------- *)
(* --- Why3 Wrapper Command                                               --- *)
(* -------------------------------------------------------------------------- *)

let wrap
    ?(auto=false)
    ?(configs=true)
    ?(drivers=false)
    ?(prefix=[])
    ?(pkgs=[])
    argv : string array =
  let open Bag in
  let args = ref empty in
  let pkgs = ref (Bag.of_list pkgs) in
  let drivers = ref drivers in
  let configs = ref configs in
  let p = ref 1 in
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
  to_array @@ Bag.of_list prefix ++ cfg +> "-L" +> "." ++ load ++ drv ++ !args

let exec cmd argv =
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
    let args = wrap ~auto:true argv in
    Unix.execv cmd args ;
  | _,process -> process argv

let register ~name ?(args="") process =
  assert (not @@ List.mem_assoc name !commands) ;
  commands := (name,(args,process)) :: !commands

(* -------------------------------------------------------------------------- *)
(* --- why3find where                                                     --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"where"
    begin fun argv ->
      usage argv
        "USAGE:\n\
         \n  why3find where\n\n\
         DESCRIPTION:\n\
         \n  Prints all installation sites" ;
      List.iter
        (fun site -> Format.printf "%s@\n" site)
        Global.Sites.packages
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
         \n  Prints all installed packages" ;
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
        let pp_opt name ps =
          if ps <> [] then
            Format.printf "  @[<hov 2>%s %s@]@\n" name
              (String.concat ", " ps) in
        List.iter
          (fun (p : Meta.pkg) ->
             Format.printf "Package %s:@\n" p.name ;
             Format.printf "  path %s@\n" p.path ;
             pp_opt "depends" p.depends ;
             pp_opt "configs" p.configs ;
             pp_opt "drivers" p.drivers ;
          ) pkgs
    end

(* -------------------------------------------------------------------------- *)
(* --- Install/Remove commands                                            --- *)
(* -------------------------------------------------------------------------- *)

let rec cleanup path =
  if Sys.file_exists path then
    if Sys.is_directory path then
      begin
        Array.iter
          (fun d -> cleanup (Filename.concat path d))
          (Sys.readdir path) ;
        Sys.rmdir path
      end
    else
      Sys.remove path

let rec mkdirs = function
  | "/" | "." -> ()
  | path ->
    if not (Sys.file_exists path) then
      begin
        mkdirs (Filename.dirname path) ;
        Sys.mkdir path 0o755 ;
      end

let copy buffer ~src ~tgt =
  mkdirs (Filename.dirname tgt) ;
  let inc = open_in src in
  let out = open_out tgt in
  let rec walk () =
    let n = Stdlib.input inc buffer 0 (Bytes.length buffer) in
    if n > 0 then
      ( Stdlib.output out buffer 0 n ; walk () )
  in walk () ; close_in inc ; close_out out

let () = register ~name:"uninstall" ~args:"[PKG...]"
    begin fun argv ->
      usage argv
        "USAGE:\n\
         \n  why3find uninstall [PKG...]\n\n\
         DESCRIPTION:\n\
         \n  Remove all specified packages from topmost installation site." ;
      for i = 1 to Array.length argv - 1 do
        let pkg = argv.(i) in
        let path = Meta.path pkg in
        if Sys.file_exists path then
          begin
            Format.printf "remove %s@." pkg ;
            cleanup path ;
          end
      done
    end

let () = register ~name:"install" ~args:"PKG [ARG...]"
    begin fun argv ->
      usage argv
        "USAGE:\n\
         \n  why3find install PKG [ARG...]\n\n\
         DESCRIPTION:\n\
         \n  Install the package PKG at the topmost installation site.\
         \n  Contents of the installed package is specified by ARG extension:\
         \n    - PKG_NAME install PKG_NAME as a package dependency of PKG\
         \n    - PKG/*.mlw a why3 source file in PKG's scope\
         \n    - *.cfg an extract why3 configuration file\
         \n    - *.drv an ocaml extraction driver for PKG clients" ;
      let pkg = argv.(1) in
      let path = Meta.path pkg in
      if Sys.file_exists path then
        begin
          Format.printf "remove %s@." pkg ;
          cleanup path ;
        end ;
      mkdirs path ;
      let depends = ref [] in
      let drivers = ref [] in
      let configs = ref [] in
      let prefix = Filename.concat pkg "" in
      let buffer = Bytes.create 2048 in
      for i = 2 to Array.length argv - 1 do
        let a = argv.(i) in
        match Filename.extension a with
        | "" -> Format.printf "depend %s@." a ; depends := a :: !depends
        | ".mlw" ->
          if not (String.starts_with ~prefix a) then
            failwith
              (Printf.sprintf "can not install %S out of %S" a prefix) ;
          Format.printf "install %s@." a ;
          copy buffer ~src:a ~tgt:(Filename.concat path a) ;
        | ".cfg" ->
          Format.printf "config %s@." a ;
          copy buffer ~src:a ~tgt:(Filename.concat path a) ;
          configs := a :: !configs ;
        | ".drv" ->
          Format.printf "driver %s@." a ;
          copy buffer ~src:a ~tgt:(Filename.concat path a) ;
          drivers := a :: !drivers ;
        | _ -> failwith (Printf.sprintf "don't know what to do with %S" a)
      done ;
      Meta.install {
        name = pkg ;
        path ;
        depends = List.rev !depends ;
        drivers = List.rev !drivers ;
        configs = List.rev !configs ;
      }
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
         \n  -p|--package PKG package dependency\
         \n  --extra-config FILE additional configuration file\
         \n";
      let args = wrap ~prefix:["prove";"--type-only"] ~configs:true argv in
      Unix.execv "why3" args
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
         \n  Run why3 ide on the given file.\n\n\
         OPTIONS:\n\
         \n  -p|--package PKG package dependency\
         \n  --extra-config FILE additional configuration file\
         \n";
      let args = wrap ~prefix:["ide"] ~configs:true argv in
      Unix.execv "why3" args
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
         \n  -p|--package PKG package dependency\
         \n  --extra-config FILE additional configuration file\
         \n";
      let args = wrap ~prefix:["replay"] ~configs:true argv in
      Unix.execv "why3" args
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
         \n  Executes why3 ide with the specified arguments.\n\n\
         OPTIONS:\n\
         \n  -p|--package PKG package dependency\
         \n  -D|--driver NAME|FILE additional extraction driver\
         \n  --extra-config FILE additional configuration file\
         \n";
      let args = wrap ~prefix:["extract"] ~configs:true ~drivers:true argv in
      Unix.execv "why3" args
    end

(* -------------------------------------------------------------------------- *)
(* --- PROVE                                                              --- *)
(* -------------------------------------------------------------------------- *)

let () = register ~name:"prove" ~args:"[-p PKG] FILE"
    begin fun argv ->
      usage argv
        "USAGE:\n\
         \n  why3find ide [OPTIONS] FILE\n\n\
         DESCRIPTION:\n\
         \n  Run why3 ide on the given file.\n\n\
         OPTIONS:\n\
         \n  -p|--package PKG package dependency\
         \n  --extra-config FILE additional configuration file\
         \n";
      let args = wrap ~configs:true argv in
      Unix.execv "hammer" args
    end

(* -------------------------------------------------------------------------- *)
