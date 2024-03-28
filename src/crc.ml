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
(* --- Proof Certificates                                                 --- *)
(* -------------------------------------------------------------------------- *)

type state =
  | Stuck
  | Prover of Prover.prover_desc * float
  | Tactic of {
      id : string ;
      children : crc list ;
      stuck : int ;
      proved : int ;
    }

and crc = {
  goal : Session.goal option ; (* not saved in JSON *)
  state : state ;
}

let stuck ?goal () = { goal ; state = Stuck }
let prover ?goal p f = { goal ; state = Prover(p,f) }

let get_stuck { state } = match state with
  | Stuck -> 1
  | Prover _ -> 0
  | Tactic { stuck } -> stuck

let get_proved { state } = match state with
  | Stuck -> 0
  | Prover _ -> 1
  | Tactic { proved } -> proved

let size { state } = match state with
  | Stuck | Prover _ -> 1
  | Tactic { stuck ; proved } -> stuck + proved

let is_stuck { state } = match state with
  | Stuck -> true
  | Prover _ | Tactic _ -> false

let is_complete { state } = match state with
  | Stuck -> false
  | Prover _ -> true
  | Tactic { stuck } -> stuck = 0

let is_unknown { state } = match state with
  | Stuck -> true
  | Prover _ -> false
  | Tactic { stuck ; proved } -> stuck > 0 && proved = 0

let tactic ?goal id children =
  let stuck = List.fold_left (fun n c -> n + get_stuck c) 0 children in
  let proved = List.fold_left (fun n c -> n + get_proved c) 0 children in
  if stuck > 0 && proved = 0 then { goal ; state = Stuck } else
    { goal; state = Tactic { id ; children ; stuck ; proved } }

type verdict = [ `Valid of int | `Failed of int | `Partial of int * int ]

let nverdict ~stuck:s ~proved:p =
  if s = 0 then `Valid p else
  if p = 0 then `Failed s else
    `Partial(p,s+p)

let verdict crc = nverdict ~stuck:(get_stuck crc) ~proved:(get_proved crc)

let pp_result fmt ~stuck:s ~proved:p =
  if s = 0 then
    if p > 0
    then Format.fprintf fmt "%t (%d)" Utils.pp_ok p
    else Format.fprintf fmt "%t (-)" Utils.pp_ok
  else
  if p = 0 then Utils.pp_ko fmt
  else Format.fprintf fmt "%t (%d/%d)" Utils.pp_ko p (s+p)

let pretty fmt crc =
  let s = get_stuck crc in
  let p = get_proved crc in
  pp_result fmt ~stuck:s ~proved:p


(* -------------------------------------------------------------------------- *)
(* --- JSON                                                               --- *)
(* -------------------------------------------------------------------------- *)

let rec to_json (a : crc) : Json.t = match a.state with
  | Stuck -> `Null
  | Prover(p,t) ->
    `Assoc [ "prover", `String (Prover.desc_to_string p) ; "time", `Float t]
  | Tactic { id ; children } ->
    `Assoc [
      "tactic", `String id;
      "children", `List (List.map to_json children);
    ]

let rec of_json (js : Json.t) : crc =
  try
    match js with
    | `Assoc fds when List.mem_assoc "prover" fds ->
      let p = List.assoc "prover" fds |> Json.jstring in
      let t = List.assoc "time" fds |> Json.jfloat in
      prover (Prover.desc_of_string p) t
    | `Assoc fds when List.mem_assoc "tactic" fds ->
      let f = List.assoc "tactic" fds |> Json.jstring in
      let xs = List.assoc "children" fds |> Json.jlist in
      tactic f (List.map of_json xs)
    | `Assoc fds when List.mem_assoc "transf" fds ->
      (* Deprecated *)
      let f = List.assoc "transf" fds |> Json.jstring in
      let xs = List.assoc "children" fds |> Json.jlist in
      tactic f (List.map of_json xs)
    | _ -> stuck ()
  with _ -> stuck ()

(* -------------------------------------------------------------------------- *)
(* --- Merging                                                            --- *)
(* -------------------------------------------------------------------------- *)

let window t0 t1 =
  let w0 = max t0 0.01 in
  let w1 = max t1 0.01 in
  w0 *. 0.5 < w1 && w1 < w0 *. 2.0

let rec merge a b =
  let goal = match a.goal with None -> b.goal | g -> g in
  match a.state, b.state with
  | Prover(p0,t0), Prover(p1,t1) when p0 = p1 && window t0 t1 -> a
  | Tactic { id = f ; children = xs } ,
    Tactic { id = g ; children = ys }
    when f = g && List.length xs = List.length ys ->
    tactic ?goal f (List.map2 merge xs ys)
  | _ -> b

let fixed = ref 0
let broken = ref 0
let updated = ref 0
let minimized = ref 0
let unchanged = ref 0
let nproved = ref 0
let nstuck = ref 0

let (+=) r n = r := !r + n

let rec stats a b =
  begin
    match a.state, b.state with

    | Stuck, Stuck ->
      incr unchanged ;
      incr nstuck ;

    | Stuck, _ ->
      (if is_complete b then fixed else updated) += size b ;
      nstuck += get_stuck b ;
      nproved += get_proved b ;

    | _, Stuck ->
      incr (if is_complete a then broken else updated) ;
      incr nstuck ;

    | Prover _, Prover _ ->
      incr (if a = b then unchanged else updated) ;
      incr nproved ;

    | Tactic _ , Prover _ ->
      incr (if is_complete a then minimized else fixed) ;
      incr nproved ;

    | Tactic { id = f0 ; children = cs0 } ,
      Tactic { id = f1 ; children = cs1 }
      when f0 = f1 && List.length cs0 = List.length cs1 ->
      List.iter2 stats cs0 cs1

    | _ ->
      let r =
        match is_complete a, is_complete b with
        | true, true | false, false -> updated
        | true, false -> broken
        | false, true -> fixed
      in
      r += size b ;
      nstuck += get_stuck b ;
      nproved += get_proved b ;

  end

let print_stats () =
  begin
    Utils.flush () ;
    Format.printf "Proofs %t"
      (pp_result ~proved:!nproved ~stuck:!nstuck) ;
    if !fixed + !broken + !minimized + !updated = 0 then
      if !nproved >0 then
        Format.printf " (unchanged)@."
      else
        Format.printf " (none)@."
    else
      begin
        Format.print_newline () ;
        if !unchanged > 0 then
          Format.printf " - unchanged: %d@\n" !unchanged ;
        if !fixed > 0 then
          Format.printf " - fixed: @{<green>%d@}@\n" !fixed ;
        if !minimized > 0 then
          Format.printf " - minimized: @{<green>%d@}@\n" !minimized ;
        if !updated > 0 then
          Format.printf " - updated: @{<orange>%d@}@\n" !updated ;
        if !broken > 0 then
          Format.printf " - broken: @{<red>%d@}@\n" !broken ;
        Format.print_flush () ;
      end
  end

(* -------------------------------------------------------------------------- *)
(* --- iter                                                               --- *)
(* -------------------------------------------------------------------------- *)

let rec iter (f : Session.goal -> state -> unit) c =
  Option.iter (fun g -> f g c.state) c.goal ;
  match c.state with
  | Stuck | Prover _ -> ()
  | Tactic { children } -> List.iter (iter f) children

(* -------------------------------------------------------------------------- *)
(* --- Dump                                                               --- *)
(* -------------------------------------------------------------------------- *)

let rec dump fmt { state } = match state with
  | Stuck -> Format.fprintf fmt "@{<red>Unknown@}"
  | Prover(p,t) -> Format.fprintf fmt "%s (%a)" (Prover.desc_name p) Utils.pp_time t
  | Tactic { id ; children } ->
    Format.fprintf fmt "@[<hv 2>%s" id ;
    List.iter (dumpchild fmt) children ;
    Format.fprintf fmt "@]"

and dumpchild fmt c = Format.pp_print_space fmt () ; dump fmt c

(* -------------------------------------------------------------------------- *)
