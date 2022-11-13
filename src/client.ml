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
(* --- Proof Client                                                       --- *)
(* -------------------------------------------------------------------------- *)

let server = ref ""
let polling = ref 1.0
let trace = ref false

let options = [
  "--server",Arg.Set_string server,
  "URL proof server address (default none)";
  "--polling",Arg.Set_float polling,
  "TIME server polling interval (default 1.0s)";
  "--trace",Arg.Set trace,"Trace server protocol";
]

type client = unit

let connect () = None

let prove client ~alpha ~cancel task prover timeout =
  ignore (client,cancel,alpha,task,prover,timeout) ;
  Fibers.return Runner.NoResult

(* -------------------------------------------------------------------------- *)
