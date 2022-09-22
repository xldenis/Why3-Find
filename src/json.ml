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
(* --- JSON Utilities                                                     --- *)
(* -------------------------------------------------------------------------- *)

type t = Yojson.t

let of_file f : t = (Yojson.Basic.from_file f :> t)
let to_file f js =
  let out = open_out f in
  Yojson.pretty_to_channel ~std:true out js ;
  close_out out

let jbool = function
  | `Bool b -> b
  | _ -> false

let jint = function
  | `Int n -> n
  | `Float a -> int_of_float (a +. 0.5)
  | _ -> 0

let jfloat = function
  | `Float a -> a
  | `Int n -> float n
  | _ -> 0.0

let jstring = function
  | `String a -> a
  | _ -> ""

let jlist = function
  | `List xs -> xs
  | _ -> []

let jstringlist js = jlist js |> List.map jstring

let jmem fd = function
  | `Assoc fds -> List.mem_assoc fd fds
  | _ -> false

let jfield fd = function
  | `Assoc fds -> (try List.assoc fd fds with Not_found -> `Null)
  | _ -> `Null

let jfield_exn fd = function
  | `Assoc fds -> List.assoc fd fds
  | _ -> raise Not_found

let jiter f = function
  | `Assoc fds -> List.iter (fun (fd,js) -> f fd js) fds
  | _ -> ()

(* -------------------------------------------------------------------------- *)
