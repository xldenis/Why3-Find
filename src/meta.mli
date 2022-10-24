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
  extracted: bool;
}


(** [shared f] returns the path of file [f] installed in shared directory. *)
val shared : string -> string

(** [path pkg] returns the topmost installation path of package [pkg]. *)
val path : string -> string

(** [find pkg] returns the installed package META data, if installed. *)
val find : string -> pkg

(** [find_all pkgs] returns all listed packages and their dependencies,
    in dependency order. *)
val find_all : string list -> pkg list

(** [install pkg] install the package META data. *)
val install : pkg -> unit

(* -------------------------------------------------------------------------- *)
