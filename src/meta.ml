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
(* --- META Package Infos                                                 --- *)
(* -------------------------------------------------------------------------- *)

type pkg = {
  name: string ;
  path: string ;
  depends: string list ;
  configs: string list ;
  drivers: string list ;
  extracted: bool ;
}

(* -------------------------------------------------------------------------- *)
(* --- Package Lookup                                                     --- *)
(* -------------------------------------------------------------------------- *)

let path pkg =
  match Global.Sites.packages with
  | local::_ -> Filename.concat local pkg
  | [] -> failwith "Installation site not found"

let find pkg =
  let rec lookup pkg = function
    | [] -> Utils.failwith "Package '%s' not found" pkg
    | d::ds ->
      let path = Filename.concat d pkg in
      if not @@ Sys.file_exists path then lookup pkg ds else
        let meta = Filename.concat path "META.json" in
        if Sys.file_exists meta then
          let open Json in
          let js = of_file meta in
          let depends = jfield "depends" js |> jstringlist in
          let configs = jfield "configs" js |> jstringlist in
          let drivers = jfield "drivers" js |> jstringlist in
          let extracted = jfield "extracted" js |> jbool in
          { name = pkg ; path ; depends ; configs ; drivers ; extracted }
        else
          { name = pkg ; path ; depends = [] ; configs = [] ; drivers = [] ;
            extracted = false }
  in lookup pkg Global.Sites.packages

let find_all pkgs =
  let m = Hashtbl.create 32 in
  let pool = ref [] in
  let rec walk pkg =
    if not (Hashtbl.mem m pkg) then
      begin
        Hashtbl.add m pkg () ;
        let m = find pkg in
        List.iter walk m.depends ;
        pool := m :: !pool ;
      end
  in List.iter walk pkgs ; List.rev !pool

let install pkg =
  let meta = Filename.concat pkg.path "META.json" in
  let list xs = `List (List.map (fun s -> `String s) xs) in
  let nonempty (_,js) =
    match js with
    | `Bool b -> b
    | `Null -> false
    | `List [] -> false
    | _  -> true
  in
  Json.to_file meta @@ `Assoc (
    List.filter nonempty [
      "depends", list pkg.depends ;
      "configs", list pkg.configs ;
      "drivers", list pkg.drivers ;
      "extracted", `Bool pkg.extracted ;
    ])

(* -------------------------------------------------------------------------- *)
(* ---                                                                    --- *)
(* -------------------------------------------------------------------------- *)

let shared file =
  let rec lookup = function
    | [] -> Utils.failwith "Resource '%s' not found" file
    | d::ds ->
      let path = Filename.concat d file in
      if Sys.file_exists path then path else
        lookup ds
  in lookup Global.Sites.resources

(* -------------------------------------------------------------------------- *)
