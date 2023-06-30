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
(* --- HTML Documentation Generator                                       --- *)
(* -------------------------------------------------------------------------- *)

module T = Token
module L = Lexer
module P = Pdoc
module Sid = Why3.Ident.Sid

(* -------------------------------------------------------------------------- *)
(* --- HTML Mode                                                          --- *)
(* -------------------------------------------------------------------------- *)

type mode =
  | Body
  | Pre
  | Div
  | Par
  | Emph
  | Bold
  | Head of Pdoc.buffer * int
  | List of int (* indent *)
  | Item of int (* indent *)

let pp_mode fmt = function
  | Body -> Format.pp_print_string fmt "BODY"
  | Pre -> Format.pp_print_string fmt "PRE"
  | Div -> Format.pp_print_string fmt "DIV"
  | Par -> Format.pp_print_string fmt "PAR"
  | Emph -> Format.pp_print_string fmt "EMPH"
  | Bold -> Format.pp_print_string fmt "BOLD"
  | Head(_,n) -> Format.fprintf fmt "HEAD%d" n
  | List n -> Format.fprintf fmt "LIST%d" n
  | Item n -> Format.fprintf fmt "ITEM%d" n
[@@ warning "-32"]

let is_opening = function
  | "scope" | "match" | "try" | "begin" -> true
  | _ -> false

let is_closing = function
  | "end" -> true
  | _ -> false

(* -------------------------------------------------------------------------- *)
(* --- Environment                                                        --- *)
(* -------------------------------------------------------------------------- *)

type block = Raw | Doc | Src
type indent = Code | Head of int | Lines of int * int

let pp_block fmt = function
  | Raw -> Format.pp_print_string fmt "RAW"
  | Doc -> Format.pp_print_string fmt "DOC"
  | Src -> Format.pp_print_string fmt "SRC"
[@@ warning "-32"]

type env = {
  dir : string ; (* destination directory *)
  src : Docref.source ; (* source file infos *)
  input : Token.input ; (* input lexer *)
  out : Pdoc.output ; (* output buffer *)
  crc : Pdoc.output ; (* proofs buffer *)
  wenv : Why3.Env.env ; (* why3 env. *)
  cenv : Docref.cenv ; (* cloning order *)
  henv : Axioms.henv ; (* axioms *)
  senv : Soundness.env ; (* instances *)
  mutable proof_href : int ; (* proof href *)
  mutable clone_decl : int ; (* clone decl indent (when > 0) *)
  mutable clone_inst : bool ; (* clone instance found *)
  mutable declared : Sid.t ; (* locally declared *)
  mutable scope : string option ; (* current module name *)
  mutable theory : Docref.theory option ; (* current module theory *)
  mutable space : bool ; (* leading space in Par mode *)
  mutable indent : indent ; (* leading space in Pre mode *)
  mutable block: block ; (* required block for text *)
  mutable mode : mode ; (* current output mode *)
  mutable file : mode ; (* current file mode *)
  mutable stack : mode list ; (* currently opened modes *)
  mutable opened : int ; (* number of pending 'end' *)
  mutable section : int ; (* code foldable section depth *)
}

let bsync ?wanted env =
  let wanted =
    match wanted with Some b -> b | None ->
    match env.mode with
    | Body -> Raw
    | Pre -> Src
    | Head _ | Div | Par | Emph | Bold | List _ | Item _ -> Doc
  in
  if env.block <> wanted then
    begin
      (* Need for closing *)
      begin match env.block with
        | Raw -> ()
        | Doc -> Pdoc.pp_print_string env.out "</div>\n"
        | Src ->
          match env.indent with
          | Code -> Pdoc.pp_print_string env.out "\n</pre>\n"
          | Lines _ -> Pdoc.pp_print_string env.out "</pre>\n"
          | Head _ -> ()
      end ;
      Pdoc.flush env.out ;
      env.indent <- Code ;
      env.block <- wanted ;
      (* Need for opening *)
      begin match wanted with
        | Raw -> ()
        | Doc -> Pdoc.pp_print_string env.out "<div class=\"doc\">\n"
        | Src -> env.indent <- Head 0
      end ;
    end

let clear env =
  env.space <- false

let space env =
  if env.space then
    begin
      Pdoc.pp_print_char env.out ' ' ;
      clear env ;
    end

let push env m =
  env.stack <- env.mode :: env.stack ;
  env.mode <- m ;
  match m with
  | Body | Head _ | Div -> env.indent <- Code
  | Pre -> env.indent <- Head 0
  | Par -> bsync env ; Pdoc.pp_print_string env.out "<p>"
  | List _ -> bsync env ; Pdoc.pp_print_string env.out "<ul>\n"
  | Item _ -> bsync env ; Pdoc.pp_print_string env.out "<li>"
  | Emph -> bsync env ; space env ; Pdoc.pp_print_string env.out "<em>"
  | Bold -> bsync env ; space env ; Pdoc.pp_print_string env.out "<strong>"

let pop env =
  let m = env.mode in
  begin match env.stack with [] -> () | old :: stk ->
    env.mode <- old ;
    env.stack <- stk ;
  end ;
  match m with
  | Body | Head _ -> ()
  | Pre -> clear env
  | Div -> clear env
  | Par -> Pdoc.pp_print_string env.out "</p>\n" ; clear env
  | List _ -> Pdoc.pp_print_string env.out "</ul>\n" ; clear env
  | Item _ -> Pdoc.pp_print_string env.out "</li>\n" ; clear env
  | Emph -> Pdoc.pp_print_string env.out "</em>" (* keep space *)
  | Bold -> Pdoc.pp_print_string env.out "</strong>" (* keep space *)

let text env =
  match env.mode with
  | Body -> push env env.file ; bsync env
  | Div -> push env Par ; bsync env
  | List n -> push env (Item n) ; bsync env
  | Par | Item _ | Head _ | Emph | Bold -> space env ; bsync env
  | Pre ->
    match env.indent with
    | Code -> ()
    | Head n ->
      bsync env ;
      Pdoc.pp_print_string env.out "<pre class=\"src\">\n" ;
      Pdoc.pp_print_string env.out (String.make n ' ') ;
      env.indent <- Code
    | Lines(l,n) ->
      Pdoc.pp_print_string env.out (String.make l '\n') ;
      Pdoc.pp_print_string env.out (String.make n ' ') ;
      env.indent <- Code

let head env buffer level =
  begin
    let title = Pdoc.buffered env.out in
    Pdoc.pop env.out buffer ;
    Pdoc.header env.out ~level ~title () ;
  end

let rec close ?(parblock=false) env =
  match env.mode with
  | Body -> ()
  | Div | Pre when parblock -> ()
  | Div | Pre | Par | List _ | Item _ -> pop env ; close ~parblock env
  | Emph -> Token.error env.input "unclosed emphasis style"
  | Bold -> Token.error env.input "unclosed bold style"
  | Head(buffer,level) -> head env buffer level ; pop env ; close ~parblock env

(* -------------------------------------------------------------------------- *)
(* --- Icons                                                              --- *)
(* -------------------------------------------------------------------------- *)

let icon_nogoal = "icon remark icofont-check"
let icon_valid = "icon valid icofont-check"
let icon_partial = "icon warning icofont-exclamation-tringle"
let icon_failed = "icon failed icofont-exclamation-circle"
let icon_params = "icon remark icofont-question-circle"
let icon_externals = "icon remark icofont-exclamation-circle"
let icon_unsound = "icon warning icofont-question-circle"
let icon_sound = "icon valid icofont-question-circle"

let pp_mark ?href ~title ~cla fmt =
  match href with
  | None ->
    Format.fprintf fmt "<span title=\"%s\" class=\"%s\"></span>"
      title cla
  | Some href ->
    Format.fprintf fmt "<a href=\"%t\" title=\"%s\" class=\"%s\"></a>"
      href title cla

(* -------------------------------------------------------------------------- *)
(* --- Proof Report                                                       --- *)
(* -------------------------------------------------------------------------- *)

let pp_vlink ?href fmt r =
  match r with
  | `Valid n ->
    let title = match n with
      | 0 -> "Valid (no goals)"
      | 1 -> "Valid (one goal)"
      | _ -> Printf.sprintf "Valid (%d goals)" n in
    let cla = if n > 0 then icon_valid else icon_nogoal in
    pp_mark ?href ~title ~cla fmt
  | `Partial(p,n) ->
    let title = Printf.sprintf "Partial proof (%d/%d goals)" p n in
    pp_mark ?href ~title ~cla:icon_partial fmt
  | `Failed _ ->
    let title = "Failed (no proof)" in
    pp_mark ?href ~title ~cla:icon_failed fmt

let pp_verdict = pp_vlink ?href:None

let process_proofs_summary env ?(crc=true) id = function
  | None -> ()
  | Some Docref.{ proofs } ->
    let stuck,proved =
      Docref.Mstr.fold
        (fun _g c (s,p) -> s + Crc.stuck c , p + Crc.proved c)
        proofs (0,0) in
    let href fmt = Format.fprintf fmt "%s.proof.html#%s" env.src.urlbase id in
    let r = Crc.nverdict ~stuck ~proved in
    Pdoc.pp env.out (pp_vlink ~href) r ;
    if crc then Pdoc.pp env.crc pp_verdict r

let rec child n fmt crc =
  Format.fprintf fmt "@\n%a" Pdoc.pp_spaces n ;
  match crc with
  | Crc.Stuck ->
    Format.fprintf fmt "stuck<span class=\"%s\"></span>" icon_failed
  | Crc.Prover(p,t) ->
    Format.fprintf fmt "%s %a" (Crc.shortname p) Utils.pp_time t
  | Crc.Tactic { id ; children ; stuck ; proved } ->
    Format.fprintf fmt "%s%a" id pp_verdict (Crc.nverdict ~stuck ~proved) ;
    List.iter (child (n+2) fmt) children

let process_certif env id crc =
  Pdoc.printf env.crc
    "<pre class=\"src\"> %a <a id=\"%a\" href=\"%a\">%a</a>%a%t</pre>"
    Pdoc.pp_keyword "goal"
    Id.pp_proof_aname id
    (Id.pp_ahref ~scope:None) id
    Id.pp_local id
    pp_verdict (Crc.verdict crc)
    begin fun fmt -> match crc with
      | Crc.Stuck -> ()
      | Crc.Prover _ -> child 4 fmt crc
      | Crc.Tactic _ -> child 4 fmt crc
    end

let process_proof env id = function
  | None -> ()
  | Some crc ->
    let href fmt = Id.pp_proof_ahref fmt id in
    Pdoc.pp env.out (pp_vlink ~href) (Crc.verdict crc) ;
    process_certif env id crc

(* -------------------------------------------------------------------------- *)
(* --- Axioms & Parameter                                                 --- *)
(* -------------------------------------------------------------------------- *)

let provers ops = String.concat "," (List.map (fun (p,_) -> Runner.name p) ops)

let process_axioms env (id : Id.id) =
  match env.theory with
  | None -> ()
  | Some theory ->
    match Axioms.parameter theory.signature id.self with
    | None -> ()
    | Some { param ; builtin ; extern } ->
      match builtin, extern with
      | [],None ->
        let icon_sound () =
          if Soundness.is_sound @@ Soundness.compute env.senv theory
          then icon_sound else icon_unsound in
        let cla,title =
          match param with
          | Type _ | Logic _ | Param _ -> icon_params, "Parameter"
          | Value _ -> icon_sound (), "Value Parameter"
          | Axiom _ -> icon_sound (), "Hypothesis"
        in
        Pdoc.ppt env.out (pp_mark ~cla ~title)
      | [], Some ext ->
        let title = Printf.sprintf "External OCaml symbol (%s)" ext in
        Pdoc.ppt env.out (pp_mark ~cla:icon_externals ~title)
      | ops, None ->
        let title = Printf.sprintf "Built-in symbol (%s)" (provers ops) in
        Pdoc.ppt env.out (pp_mark ~cla:icon_externals ~title)
      | ops, Some ext ->
        let title = Printf.sprintf
            "Built-in symbol (%s), extracted to OCaml (%s)" (provers ops) ext
        in
        Pdoc.ppt env.out (pp_mark ~cla:icon_externals ~title)

let process_abstract env Axioms.{ param } =
  try
    let id = Id.resolve ~lib:env.src.lib @@ Axioms.ident param in
    let key = match param with
      | Type _ -> "type"
      | Logic _ -> "logic"
      | Param _ -> "param"
      | Value _ -> "value"
      | Axiom _ -> "axiom"
    in
    Pdoc.printf env.crc
      "<pre class=\"src\"> %a <a id=\"%a\" href=\"%a\">%a</a>%t</pre>"
      Pdoc.pp_keyword key
      Id.pp_proof_aname id
      (Id.pp_ahref ~scope:None) id
      Id.pp_local id
      (fun fmt -> pp_mark ~title:"Abstract Parameter" ~cla:icon_unsound fmt)
  with Not_found -> ()

let process_module_axioms env =
  match env.theory with
  | None -> ()
  | Some { theory } ->
    Axioms.iter env.henv
      (fun (p : Axioms.parameter) ->
         if Axioms.is_abstract p then
           process_abstract env p)
      [theory]

type axioms = {
  ext : int ;
  prm : int ;
  hyp :  int ;
  pvs : int ;
  sound : Soundness.soundness ;
}

let free = {
  ext = 0 ; prm = 0 ; hyp = 0 ; pvs = 0 ;
  sound = Soundness.Unknown [] ;
}

let add_axiom hs (p : Axioms.parameter) =
  if not @@ Axioms.is_abstract p then
    { hs with ext = succ hs.ext }
  else
    match p.param with
    | Axiom _ -> { hs with hyp = succ hs.hyp }
    | Value _ -> { hs with pvs = succ hs.pvs }
    | Type _ | Logic _ | Param _ -> { hs with prm = succ hs.prm }

let pp_axioms fmt { ext ; prm ; hyp ; pvs ; sound } =
  if ext+prm+hyp+pvs > 0 then
    let ok = Soundness.is_sound sound in
    let cla =
      if hyp + pvs > 0 then if ok then icon_sound else icon_unsound else
      if prm > 0 then icon_params else icon_externals in
    let title =
      let plural k single msg =
        if k = 0 then [] else if k = 1 then [single] else [msg k] in
      String.concat ", " @@ List.concat [
        plural pvs "1 value" @@ Printf.sprintf "%d values" ;
        plural prm "1 parameter" @@ Printf.sprintf "%d parameters" ;
        plural hyp "1 hypothesis" @@ Printf.sprintf "%d hypotheses" ;
        plural ext "1 external symbol" @@ Printf.sprintf "%d external symbols" ;
      ] in
    pp_mark ~cla ~title fmt

let process_axioms_summary env = function
  | None -> ()
  | Some (thy : Docref.theory) ->
    let hs = List.fold_left add_axiom free @@ Axioms.parameters thy.signature in
    let hs = { hs with sound = Soundness.compute env.senv thy } in
    Pdoc.pp env.out pp_axioms hs

(* -------------------------------------------------------------------------- *)
(* --- References                                                         --- *)
(* -------------------------------------------------------------------------- *)

let rec fetch_module_id input =
  match Token.token input with
  | Space | Newline -> fetch_module_id input
  | Ident s -> s
  | _ -> Token.error input "missing module or theory name"

let resolve env ?(infix=false) () =
  Docref.resolve
    ~src:env.src ~theory:env.theory
    ~infix (Token.position env.input)

let process_href env (href : Docref.href) s =
  match href with

  | Docref.Def(id,proof) ->
    env.declared <- Sid.add id.self env.declared ;
    if id.id_qid = [] then
      Pdoc.pp env.out Pdoc.pp_html s
    else
      Pdoc.printf env.out "<a id=\"%a\">%a</a>" Id.pp_aname id Pdoc.pp_html s ;
    process_axioms env id ;
    process_proof env id proof

  | Docref.Ref id ->
    if env.clone_decl > 0 && id.id_qid = [] then
      begin
        Docref.set_instance env.cenv id.self ;
        env.clone_inst <- true ;
      end ;
    Pdoc.printf env.out "<a title=\"%a\" href=\"%a\">%a</a>"
      Id.pp_title id
      (Id.pp_ahref ~scope:env.scope) id
      Pdoc.pp_html s

  | Docref.NoRef ->
    Pdoc.pp_html_s env.out s

(* -------------------------------------------------------------------------- *)
(* --- Printing Declarations                                              --- *)
(* -------------------------------------------------------------------------- *)

let pp_ident ~env ?attr ?name fmt id =
  try
    let id = Id.resolve ~lib:env.src.lib id in
    let pp_name fmt =
      match name with
      | Some a -> Format.pp_print_string fmt a
      | None -> Id.pp_local fmt id
    in
    begin match attr with
      | None -> Format.fprintf fmt "<a"
      | Some a -> Format.fprintf fmt "<a class=\"%s\"" a
    end ;
    Format.fprintf fmt " title=\"%a\" href=\"%a\">%t</a>"
      Id.pp_title id
      (Id.pp_ahref ~scope:None) id
      pp_name
  with Not_found ->
    (* Builtin *)
    Format.pp_print_string fmt @@
    match name with
    | Some a -> a
    | None -> id.id_string

let rec pp_type ~env ~arg fmt (ty : Why3.Ty.ty) =
  if arg then Format.pp_print_char fmt ' ' ;
  match ty.ty_node with
  | Tyvar tv -> Format.pp_print_string fmt tv.tv_name.id_string
  | Tyapp(ts,[]) -> pp_ident ~env fmt ts.ts_name
  | Tyapp(ts,args) ->
    begin
      if arg then Format.pp_print_char fmt '(' ;
      pp_ident ~env fmt ts.ts_name ;
      List.iter (pp_type ~env ~arg:true fmt) args ;
      if arg then Format.pp_print_char fmt ')' ;
    end

let attributes (rs : Why3.Expr.rsymbol) =
  (if Why3.Expr.rs_ghost rs then ["ghost"] else []) @
  (match Why3.Expr.rs_kind rs with
   | RKnone | RKlocal -> []
   | RKfunc -> ["function"]
   | RKpred -> ["predicate"]
   | RKlemma -> ["lemma"])

let defkind = function
  | { Why3.Expr.c_node = Cany } -> "val"," "
  | _ -> "let"," = "

let declare env n kwd ?(attr=[]) id =
  begin
    let r = Id.resolve ~lib:env.src.lib id in
    Pdoc.printf env.out "%a%a" Pdoc.pp_spaces n Pdoc.pp_keyword kwd ;
    List.iter (Pdoc.printf env.out " %a" Pdoc.pp_attribute) attr ;
    Pdoc.printf env.out " <a id=\"%a\">%a</a>" Id.pp_aname r Id.pp_local r ;
    process_axioms env r ;
    process_proof env r @@ Docref.find_proof r.self env.theory ;
  end

let definition env ?(op=" = ") def =
  Pdoc.printf env.out "%s{%a}@\n" op
    (pp_ident ~env ~attr:"attribute" ~name:"def.") def

let symbol env n ?attr (ls : Why3.Term.lsymbol) ?op def =
  match ls.ls_value with
  | None ->
    declare env n "predicate" ?attr ls.ls_name ;
    List.iter (Pdoc.pp env.out @@ pp_type ~env ~arg:true) ls.ls_args ;
    definition env ?op def ;
  | Some r ->
    declare env n "function" ?attr ls.ls_name ;
    List.iter (Pdoc.pp env.out @@ pp_type ~env ~arg:true) ls.ls_args ;
    Pdoc.printf env.out " : %a" (pp_type ~env ~arg:false) r ;
    definition env ?op def

let decl env n id (d : Why3.Decl.decl) def =
  ignore def ;
  match d.d_node with
  | Dtype { ts_def = NoDef } ->
    declare env n "type" id ; definition env ~op:" " def
  | Dtype _ | Ddata _ ->
    declare env n "type" id ; definition env def
  | Dparam ls ->
    symbol env n ls ~op:" " def
  | Dlogic ds ->
    List.iter
      (fun (ls,_) ->
         if ls.Why3.Term.ls_name = id then
           symbol env n ls def
      ) ds
  | Dind _ ->
    declare env n "inductive" id ; definition env def ;
  | Dprop(Plemma,_,_) ->
    declare env n "lemma" id ; definition env ~op:" " def
  | Dprop(Paxiom,_,_) ->
    declare env n "axiom" id ; definition env ~op:" " def
  | Dprop(Pgoal,_,_) -> ()

let signature env (rs : Why3.Expr.rsymbol) =
  begin
    List.iter
      (fun x ->
         Pdoc.pp env.out (pp_type ~env ~arg:true) Why3.Ity.(ty_of_ity x.pv_ity)
      ) rs.rs_cty.cty_args ;
    Pdoc.printf env.out " : %a"
      (pp_type ~env ~arg:false)
      Why3.Ity.(ty_of_ity rs.rs_cty.cty_result) ;
  end

let mdecl env n id (pd: Why3.Pdecl.pdecl) def =
  match pd.pd_node with
  | PDpure -> List.iter (fun d -> decl env n id d def) pd.pd_pure
  | PDtype _ -> declare env n "type" id ; definition env def
  | PDexn _ -> declare env n "exception" id ; definition env ~op:" " def
  | PDlet dlet ->
    begin
      match dlet with
      | LDvar(pv,_) ->
        declare env n "let" id ;
        Pdoc.printf env.out " : %a"
          (pp_type ~env ~arg:false) pv.pv_vs.vs_ty ;
        definition env def
      | LDsym(rs,df) ->
        let kwd,op = defkind df in
        declare env n kwd ~attr:(attributes rs) id ;
        signature env rs ;
        definition env ~op def
      | LDrec defs ->
        List.iter
          (fun (d : Why3.Expr.rec_defn) ->
             let rs = d.rec_sym in
             if Why3.Ident.id_equal id rs.rs_name then
               begin
                 let kwd,op = defkind d.rec_fun in
                 declare env n kwd ~attr:(attributes rs) id ;
                 signature env rs ;
                 definition env ~op def ;
               end
          ) defs
    end

(* Declare 'id' from cloned definition 'def' in theory 'th' *)
let declaration env n id (th : Why3.Theory.theory) def =
  try
    env.declared <- Sid.add id env.declared ;
    let m = Why3.Pmodule.restore_module th in
    let pd = Why3.Ident.Mid.find id m.mod_known in
    mdecl env n id pd def
  with Not_found ->
  try decl env n id (Why3.Ident.Mid.find id th.th_known) def
  with Not_found ->
    assert false

(* -------------------------------------------------------------------------- *)
(* --- Flushing Declarations                                              --- *)
(* -------------------------------------------------------------------------- *)

let by_clone_line a b =
  let la = Id.line a.Docref.id_source in
  let lb = Id.line b.Docref.id_source in
  Int.compare la lb

let current_instance env (c : Docref.clone) =
  Docref.current_instance env.cenv c.id_instance &&
  not @@ Sid.mem c.id_target env.declared

let process_clone_proofs env clones =
  let stuck,proved = List.fold_left
      (fun (s,p) clone ->
         match Docref.find_proof clone.Docref.id_target env.theory with
         | None -> (s,p)
         | Some c -> s + Crc.stuck c, p + Crc.proved c
      ) (0,0) clones
  in
  Pdoc.pp env.out pp_verdict (Crc.nverdict ~stuck ~proved)

let process_clone_axioms env clones =
  match env.theory with
  | None -> ()
  | Some theory ->
    let sound = Soundness.compute env.senv theory in
    let signature = theory.signature in
    let hs = List.fold_left
        (fun hs clone ->
           match Axioms.parameter signature clone.Docref.id_target with
           | None -> hs
           | Some p -> add_axiom hs p
        ) { free with sound } clones in
    Pdoc.pp env.out pp_axioms hs

let process_clone_section env (th : Docref.theory) =
  let cloned =
    List.sort by_clone_line @@
    List.filter (current_instance env) th.clones
  in
  if cloned <> [] then
    begin
      text env ;
      let n0 = env.clone_decl in
      let n2 = n0 + 2 in
      let n4 = n2 + 2 in
      Pdoc.printf env.out
        " <span class=\"section\">\
         {<span class=\"section-toggle\">…</span>\
         <span class=\"section-text\">@\n\
         %a<span class=\"comment section-toggle\">begin</span>@\n"
        Pdoc.pp_spaces n2 ;
      List.iter
        (fun (clone : Docref.clone) ->
           declaration env n4 clone.id_target th.theory clone.id_source
        ) cloned ;
      Pdoc.printf env.out
        "%a<span class=\"comment section-toggle\">end</span>@\n\
         %a</span>}</span>"
        Pdoc.pp_spaces n2 Pdoc.pp_spaces n0 ;
      process_clone_axioms env cloned ;
      process_clone_proofs env cloned ;
    end

let process_clones env =
  begin
    if env.clone_inst then
      match env.theory with
      | None -> ()
      | Some th ->
        process_clone_section env th ;
        env.clone_inst <- false ;
        env.clone_decl <- 0 ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Foldable Sections                                                  --- *)
(* -------------------------------------------------------------------------- *)

let pp_active fmt b =
  if b then Format.fprintf fmt " active"

let process_open_section env ~active title =
  text env ;
  env.section <- succ env.section ;
  Pdoc.printf env.out
    "<span class=\"section level%d\">\
     <span class=\"comment\">{</span>\
     <span class=\"attribute section-toggle\">%s</span>\
     <span class=\"comment section-text%a\">…</span>\
     <span class=\"comment\">}</span>\
     <span class=\"section-text%a\">"
    (min env.section 3) title pp_active (not active) pp_active active

let process_close_section env title =
  env.section <- pred env.section ;
  text env ;
  Pdoc.printf env.out
    "<span class=\"comment\">{</span>\
     <span class=\"attribute section-toggle\">%s</span>\
     <span class=\"comment\">}</span>\
     </span></span>"
    (if title = "" then "…" else title)

(* -------------------------------------------------------------------------- *)
(* --- Module & Theory Processing                                         --- *)
(* -------------------------------------------------------------------------- *)

let process_open_module env key =
  begin
    if not (env.mode = Body && env.opened = 0) then
      Token.error env.input "unexpected module or theory" ;
    let forking = env.block in
    let prelude = Pdoc.buffered env.out in
    let id = fetch_module_id env.input in
    let href = resolve env () in
    let kind = String.capitalize_ascii key in
    let url = Docref.derived env.src id in
    let path = String.concat "." env.src.lib in
    let theory = Docref.Mstr.find_opt id env.src.theories in
    bsync ~wanted:Raw env ;
    Pdoc.printf env.out
      "<pre class=\"src\">%a <a title=\"%s.%s\" href=\"%s\">%s</a>"
      Pdoc.pp_keyword key path id url id ;
    Pdoc.printf env.crc
      "<pre class=\"src\">%a <a id=\"%s\" href=\"%s\">%s.%s</a>"
      Pdoc.pp_keyword key id url path id ;
    process_axioms_summary env theory ; (* out *)
    process_proofs_summary env id theory ; (* out & crc *)
    Pdoc.printf env.out "</pre>@." ;
    Pdoc.printf env.crc "</pre>@." ;
    let file = Filename.concat env.dir url in
    let title = Printf.sprintf "%s %s.%s" kind path id in
    Pdoc.fork env.out ~file ~title ;
    Pdoc.printf env.out
      "<header>\
       %s <code class=\"src\"><a href=\"%s.index.html\">%s</a>.%s</code>\
       </header>@\n"
      kind env.src.urlbase path id ;
    env.mode <- Body ;
    env.block <- Raw ;
    if prelude <> "" then
      begin
        bsync ~wanted:forking env ;
        Pdoc.pp env.out Format.pp_print_string prelude ;
        bsync ~wanted:Raw env ;
      end ;
    push env Pre ;
    env.block <- Raw ;
    env.file <- Pre ;
    bsync env ;
    text env ;
    env.scope <- Some id ;
    env.theory <- theory ;
    env.declared <- Sid.empty ;
    Pdoc.pp env.out Pdoc.pp_keyword key ;
    Pdoc.pp_print_char env.out ' ' ;
    process_href env href id ;
    process_axioms_summary env theory ;
    process_proofs_summary env ~crc:false id theory ;
    Docref.set_container env.cenv ~path ~id ;
  end

let process_close_module env key =
  begin
    text env ;
    Pdoc.printf env.out "%a@\n</pre>@\n" Pdoc.pp_keyword key ;
    process_module_axioms env ;
    Pdoc.close env.out ;
    env.mode <- Body ;
    env.block <- Raw ;
    env.file <- Body ;
    env.scope <- None ;
    env.theory <- None ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Identifiers                                                        --- *)
(* -------------------------------------------------------------------------- *)

let process_ident env s =
  if Docref.is_keyword s then
    begin match s with
      | "module" | "theory" -> process_open_module env s
      | "end" when env.opened = 0 -> process_close_module env s
      | _ ->
        if is_opening s then env.opened <- succ env.opened ;
        if is_closing s then env.opened <- pred env.opened ;
        if s = "clone" then env.clone_decl <- Token.indent env.input ;
        text env ;
        Pdoc.pp env.out Pdoc.pp_keyword s
    end
  else
    begin
      text env ;
      let href = resolve env () in
      process_href env href s
    end

(* -------------------------------------------------------------------------- *)
(* --- Style Processing                                                   --- *)
(* -------------------------------------------------------------------------- *)

let process_style env m =
  text env ;
  if env.mode = m then pop env else push env m

let rec list env n =
  match env.mode with
  | Body | Head _ | Pre -> ()
  | Emph -> Token.error env.input "unclosed bold style"
  | Bold -> Token.error env.input "unclosed emphasis style"
  | Par -> pop env ; push env (List n)
  | Div -> push env (List n)
  | Item _ -> pop env ; list env n
  | List m ->
    if n < m then
      begin
        pop env ;
        list env n ;
      end
    else
    if n > m then
      push env (List n)

let process_dash env s =
  let s = String.length s in
  if s = 1 && Token.startline env.input then
    list env (Token.indent env.input)
  else
    begin
      text env ;
      Pdoc.printf env.out "&%cdash;" (if s > 1 then 'm' else 'n')
    end

let process_header env h =
  begin
    bsync ~wanted:Doc env ;
    let level = String.length h in
    let buffer = Pdoc.push env.out in
    push env (Head(buffer,level))
  end

(* -------------------------------------------------------------------------- *)
(* --- Space & Newline Processing                                           --- *)
(* -------------------------------------------------------------------------- *)

let process_space env =
  match env.mode with
  | Body | Div | List _ -> ()
  | Par | Item _ | Head _ | Emph | Bold -> env.space <- true
  | Pre ->
    match env.indent with
    | Code -> bsync env ; Pdoc.pp_print_char env.out ' '
    | Head n -> env.indent <- Head (succ n)
    | Lines(l,n) -> env.indent <- Lines(l,succ n)

let process_newline env =
  match env.mode with
  | Body -> if Token.emptyline env.input then Pdoc.flush env.out
  | Div -> ()
  | Par | List _ | Item _ ->
    if Token.emptyline env.input then
      close ~parblock:true env
    else
      env.space <- true
  | Emph | Bold -> env.space <- true
  | Head(buffer,level) ->
    head env buffer level ;
    pop env
  | Pre ->
    match env.indent with
    | Code -> env.indent <- Lines(1,0)
    | Lines(n,_) -> env.indent <- Lines(succ n,0)
    | Head _ -> env.indent <- Head 0

(* -------------------------------------------------------------------------- *)
(* --- References                                                         --- *)
(* -------------------------------------------------------------------------- *)

let process_reference ~wenv ~env r =
  try
    let src = env.src in
    let scope = env.scope in
    let name, id = Docref.reference ~wenv ~src ~scope r in
    let r = Id.resolve ~lib:src.lib id in
    text env ;
    Pdoc.printf env.out
      "<code class=\"src\"><a title=\"%a\" href=\"%a\">%s</a></code>"
      Id.pp_title r (Id.pp_ahref ~scope:None) r name
  with
  | Not_found -> Token.error env.input "unknown reference"
  | Failure msg -> Token.error env.input "%s" msg

(* -------------------------------------------------------------------------- *)
(* --- Token Processing                                                   --- *)
(* -------------------------------------------------------------------------- *)

let parse env =
  let wenv = env.wenv in
  let out = env.out in
  while not (Token.eof env.input) do
    match Token.token env.input with
    | Eof -> close env ; bsync env
    | Char c -> text env ; Pdoc.pp_html_c out c
    | Text s -> text env ; Pdoc.pp_html_s out s
    | Comment s -> text env ; Pdoc.pp_html_s out ~className:"comment" s
    | Verb s -> text env ;
      Pdoc.printf out "<code class=\"src\">%a</code>" Pdoc.pp_html s
    | Ref s -> process_reference ~wenv ~env s
    | Style(Emph,_) -> process_style env Emph
    | Style(Bold,_) -> process_style env Bold
    | Style(Head,h) -> process_header env h
    | Style(Dash,n) -> process_dash env n
    | Style _ -> ()
    | OpenDoc ->
      begin
        Pdoc.flush ~onlyspace:false out ;
        close env ;
        push env Div ;
      end
    | CloseDoc ->
      begin
        close env ;
        push env env.file ;
      end
    | OpenSection(active,title) -> process_open_section env ~active title
    | CloseSection title -> process_close_section env title
    | Space -> process_space env
    | Newline -> process_newline env
    | Ident s ->
      process_ident env s ;
      process_clones env
    | Infix s ->
      text env ;
      let href = resolve env ~infix:true () in
      process_href env href s
  done

(* -------------------------------------------------------------------------- *)
(* --- Chapters                                                           --- *)
(* -------------------------------------------------------------------------- *)

type chapter = { src: Docref.source option ; hfile: string ; page: string }
let chapters = ref []
let chapter ?src ~hfile ~page () =
  chapters := { src ; hfile ; page } :: !chapters

let documentation () =
  let rec docs cs = function
    | [] -> cs
    | c::rs -> docs (if c.src = None then c::cs else cs) rs
  in docs [] !chapters

let package () =
  let cmap = ref Docref.Mstr.empty in
  List.iter
    (fun c ->
       Option.iter
         (fun (src : Docref.source) ->
            let path = String.concat "." src.lib in
            cmap := Docref.Mstr.add path c !cmap ;
         ) c.src
    ) !chapters ;
  let mark = ref [] in
  let pool = ref [] in
  let rec walk c =
    Option.iter
      (fun (src : Docref.source) ->
         if not (List.memq c !mark) then
           begin
             mark := c :: !mark ;
             Docref.Mstr.iter
               (fun _ (thy : Docref.theory) ->
                  List.iter
                    (fun (thd : Why3.Theory.theory) ->
                       let lp,_,_ = Id.path ~lib:src.lib thd.th_name in
                       let path = String.concat "." lp in
                       try walk @@ Docref.Mstr.find path !cmap
                       with Not_found -> ()
                    ) thy.depends
               ) src.theories ;
             pool := c :: !pool
           end ;
      ) c.src
  in
  Docref.Mstr.iter (fun _ c -> walk c) !cmap ;
  List.rev !pool

(* -------------------------------------------------------------------------- *)
(* --- Titling Document                                                   --- *)
(* -------------------------------------------------------------------------- *)

let document_title ~title ~page =
  if title <> "" then Printf.sprintf "%s — %s" title page else page

(* -------------------------------------------------------------------------- *)
(* --- Markdown File Processing                                           --- *)
(* -------------------------------------------------------------------------- *)

let process_markdown ~wenv ~cenv ~henv ~senv ~out:dir ~title file =
  begin
    let src = Docref.empty () in
    let input = Token.input ~doc:true file in
    let basename = Filename.chop_extension @@ Filename.basename file in
    let page = String.capitalize_ascii basename in
    let title = document_title ~title ~page in
    let hfile = basename ^ ".html" in
    let ofile = Filename.concat dir hfile in
    let out = Pdoc.output ~file:ofile ~title in
    let crc = Pdoc.null () in
    let env = {
      dir ; src ; input ; out ; crc ; wenv ; cenv ; henv ; senv ;
      space = false ;  indent = Code ;
      block = Raw; mode = Body ; file = Body ; stack = [] ;
      scope = None ; theory = None ;
      clone_decl = 0 ;
      clone_inst = false ;
      proof_href = 0 ;
      declared = Sid.empty ;
      opened = 0 ; section = 0 ;
    } in
    chapter ~hfile ~page () ;
    Pdoc.printf out "<header>%a</header>@\n" Pdoc.pp_html title ;
    Pdoc.flush out ;
    push env Div ;
    parse env ;
    Pdoc.close_all out ;
  end

(* -------------------------------------------------------------------------- *)
(* --- MLW File Processing                                                --- *)
(* -------------------------------------------------------------------------- *)

let process_source ~wenv ~cenv ~henv ~senv ~out:dir
    ~title file (src : Docref.source) =
  let path = String.concat "." src.lib in
  let page = Printf.sprintf "Library %s" path in
  let title = document_title ~title ~page in
  let hfile = src.urlbase ^ ".index.html"in
  let ofile = Filename.concat dir hfile in
  let out = Pdoc.output ~file:ofile ~title in
  let crc = Pdoc.output
      ~file:(Filename.concat dir (src.urlbase ^ ".proof.html"))
      ~title:(Printf.sprintf "Proofs %s" path) in
  let input = Token.input file in
  let env = {
    dir ; src ; input ; out ; crc ; wenv ; henv ; cenv ; senv ;
    space = false ; indent = Code ;
    block = Raw ; mode = Body ; file = Body ; stack = [] ;
    scope = None ; theory = None ;
    clone_decl = 0 ;
    clone_inst = false ;
    proof_href = 0 ;
    declared = Sid.empty ;
    opened = 0 ; section = 0 ;
  } in
  begin
    chapter ~src ~hfile ~page () ;
    Pdoc.printf out
      "<header>Library \
       <a href=\"index.html\"><code>%s</code></a></header>@\n" path ;
    Pdoc.flush out ;
    Pdoc.printf crc "<header>Proofs (<code>%s</code>)</header>@\n" path ;
    Pdoc.printf crc "<h1>Prover Calibration</h1>@\n<pre class=\"src\">@\n" ;
    Calibration.iter
      (fun p n t ->
         Pdoc.printf crc "  %-10s n=%d %a (%s)@\n"
           (Crc.shortname p) n Utils.pp_time t p
      ) src.profile ;
    Pdoc.printf crc "</pre>@\n<h1>Proof Certificates</h1>@\n" ;
    Pdoc.flush crc ;
    parse env ;
    Pdoc.close_all out ;
    Pdoc.close_all crc ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Generating Index                                                   --- *)
(* -------------------------------------------------------------------------- *)

let pp_chapters out ~title cs =
  if cs <> [] then
    begin
      Pdoc.printf out "<h1>%s</h1>@\n<div class=\"doc\">@\n<ul>@\n" title ;
      List.iter
        (fun c ->
           Pdoc.printf out "<li><a href=\"%s\">%s</a></li>@\n" c.hfile c.page
        ) cs ;
      Pdoc.printf out "</ul>@\n</div>@." ;
    end

let index ~out:dir ~title =
  let hfile = "index.html" in
  if List.for_all (fun c -> c.hfile <> hfile) !chapters then
    begin
      let title = document_title ~title ~page:"Index" in
      let ofile = Filename.concat dir hfile in
      let out = Pdoc.output ~file:ofile ~title in
      Pdoc.printf out "<header>%s</header>@\n" title ;
      pp_chapters out ~title:"Documentation" @@ documentation () ;
      pp_chapters out ~title:"Package" @@ package () ;
      Pdoc.flush out ;
      Pdoc.close_all out ;
    end

(* -------------------------------------------------------------------------- *)
(* --- Main Doc Command                                                   --- *)
(* -------------------------------------------------------------------------- *)

let shared ~out ~file =
  let tgt = Filename.concat out file in
  let src = Meta.shared file in
  Utils.copy ~src ~tgt

let preprocess ~cenv ~henv ~wenv ~senv file =
  file ,
  if Filename.check_suffix file ".md" then None else
  if Filename.check_suffix file ".mlw" then
    let src = Docref.parse ~cenv ~henv ~wenv file in
    Soundness.register senv src ; Some src
  else
    Utils.failwith "Don't known what to do with %S" file

let process ~wenv ~henv ~out ~title (file,kind) =
  match kind with
  | None -> process_markdown ~wenv ~henv ~out ~title file
  | Some src -> process_source ~wenv ~henv ~out ~title file src

let generate ~out ~title ~files ~url =
  begin
    (* Keywords *)
    let cenv = Docref.init () in
    (* Package config *)
    let env = Wenv.init () in
    (* Why3 environment *)
    let wenv = env.wenv in
    (* Axioms config *)
    let henv = Axioms.init env in
    (* Sounness config *)
    let senv = Soundness.init () in
    (* Shared resources *)
    Utils.mkdirs @@ Filename.concat out "fonts" ;
    shared ~out ~file:"style.css" ;
    shared ~out ~file:"script.js" ;
    shared ~out ~file:"icofont.min.css" ;
    shared ~out ~file:"fonts/icofont.woff" ;
    shared ~out ~file:"fonts/icofont.woff2" ;
    (* Pre-processing *)
    List.map (preprocess ~wenv ~cenv ~henv ~senv) files |>
    (* Doc generation *)
    List.iter (process ~wenv ~cenv ~henv ~senv ~title ~out) ;
    (* Indexing *)
    index ~out ~title ;
    (* Final output *)
    Utils.log "Generated %s%s/index.html@."
      (if url then "file://" else "") (Utils.absolute out) ;
  end

(* -------------------------------------------------------------------------- *)
