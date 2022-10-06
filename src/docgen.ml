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

let is_opening = function
  | "scope" | "match" | "try" | "begin" -> true
  | _ -> false

let is_closing = function
  | "end" -> true
  | _ -> false

(* -------------------------------------------------------------------------- *)
(* --- Environment                                                        --- *)
(* -------------------------------------------------------------------------- *)

type env = {
  dir : string ; (* destination directory *)
  src : Docref.source ; (* source file infos *)
  input : Token.input ; (* input lexer *)
  out : Pdoc.output ; (* output buffer *)
  crc : Pdoc.output ; (* proofs buffer *)
  henv : Axioms.henv ; (* axioms *)
  mutable proof_href : int ; (* proof href *)
  mutable clone_decl : int ; (* clone decl indent (when > 0) *)
  mutable clone_path : string ; (* clone path *)
  mutable clone_order : int ; (* clone ranking *)
  mutable declared : Sid.t ; (* locally declared *)
  mutable scope : string option ; (* current module name *)
  mutable theory : Docref.theory option ; (* current module theory *)
  mutable space : bool ; (* leading space in Par mode *)
  mutable mode : mode ; (* current output mode *)
  mutable file : mode ; (* global file output mode *)
  mutable stack : mode list ; (* currently opened modes *)
  mutable opened : int ; (* number of pending 'end' *)
  mutable section : int ; (* code foldable section depth *)
}

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
  | Body | Head _ -> ()
  | Div -> Pdoc.pp_print_string env.out "<div class=\"doc\">\n"
  | Pre -> Pdoc.pp_print_string env.out "<pre class=\"src\">\n"
  | Par -> Pdoc.pp_print_string env.out "<p>"
  | List _ -> Pdoc.pp_print_string env.out "<ul>\n"
  | Item _ -> Pdoc.pp_print_string env.out "<li>"
  | Emph -> space env ; Pdoc.pp_print_string env.out "<em>"
  | Bold -> space env ; Pdoc.pp_print_string env.out "<strong>"

let pop env =
  let m = env.mode in
  begin match env.stack with [] -> () | old :: stk ->
    env.mode <- old ;
    env.stack <- stk ;
  end ;
  match m with
  | Body | Head _ -> ()
  | Pre -> Pdoc.pp_print_string env.out "</pre>\n" ; clear env
  | Div -> Pdoc.pp_print_string env.out "</div>\n" ; clear env
  | Par -> Pdoc.pp_print_string env.out "</p>\n" ; clear env
  | List _ -> Pdoc.pp_print_string env.out "</ul>\n" ; clear env
  | Item _ -> Pdoc.pp_print_string env.out "</li>\n" ; clear env
  | Emph -> Pdoc.pp_print_string env.out "</em>" (* keep space *)
  | Bold -> Pdoc.pp_print_string env.out "</strong>" (* keep space *)

let text env =
  match env.mode with
  | Body -> push env env.file
  | Div -> push env Par
  | List n -> push env (Item n)
  | Pre -> ()
  | Par | Item _ | Head _ | Emph | Bold -> space env

let rec close env =
  match env.mode with
  | Body -> ()
  | Emph -> Token.error env.input "unclosed emphasis style"
  | Bold -> Token.error env.input "unclosed bold style"
  | Div | Par | List _ | Item _ | Pre | Head _ -> pop env ; close env

(* -------------------------------------------------------------------------- *)
(* --- Icons                                                              --- *)
(* -------------------------------------------------------------------------- *)

let icon_valid = "icon valid icofont-check"
let icon_partial = "icon warning icofont-exclamation-tringle"
let icon_failed = "icon failed icofont-exclamation-circle"
let icon_external = "icon remark icofont-exclamation-circle"
let icon_parameter = "icon remark icofont-question-circle"
let icon_assumed = "icon warning icofont-question-circle"

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
    pp_mark ?href ~title ~cla:icon_valid fmt
  | `Partial(p,n) ->
    let title = Printf.sprintf "Partial proof (%d/%d goals)" p n in
    pp_mark ?href ~title ~cla:icon_partial fmt
  | `Failed _ ->
    let title = "Failed (no proof)" in
    pp_mark ?href ~title ~cla:icon_failed fmt

let pp_verdict = pp_vlink ?href:None

let href_proofs env path fmt = Format.fprintf fmt "_%s#%s" env.src.url path

let process_proofs_summary env ?path = function
  | None -> ()
  | Some Docref.{ proofs } ->
    let stuck,proved =
      Docref.Mstr.fold
        (fun _g c (s,p) -> s + Crc.stuck c , p + Crc.proved c)
        proofs (0,0) in
    let href = Option.map (href_proofs env) path in
    let r = Crc.nverdict ~stuck ~proved in
    Pdoc.pp env.out (pp_vlink ?href) r ;
    if path <> None then Pdoc.pp env.crc pp_verdict r

let rec child n fmt crc =
  Format.fprintf fmt "@\n%a" Pdoc.pp_spaces n ;
  match crc with
  | Crc.Stuck ->
    Format.fprintf fmt "stuck<span class=\"%s\"></span>" icon_failed
  | Crc.Prover(p,t) ->
    Format.fprintf fmt "%s %a" (Crc.shortname p) Utils.pp_time t
  | Crc.Transf { id ; children ; stuck ; proved } ->
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
      | Crc.Transf _ -> child 4 fmt crc
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

let process_axioms env (id : Id.id) =
  match env.theory with
  | None -> ()
  | Some { signature } ->
    match Axioms.parameter signature id.self with
    | None -> ()
    | Some { kind ; builtin ; extern } ->
      match builtin, extern with
      | Some op, None ->
        let title = Printf.sprintf "Built-in symbol (%s)" op in
        Pdoc.ppt env.out (pp_mark ~cla:icon_external ~title)
      | None, Some ext ->
        let title = Printf.sprintf "External OCaml symbol (%s)" ext in
        Pdoc.ppt env.out (pp_mark ~cla:icon_external ~title)
      | Some op, Some ext ->
        let title = Printf.sprintf
            "Built-in symbol (%s), extracted to OCaml (%s)" op ext in
        Pdoc.ppt env.out (pp_mark ~cla:icon_external ~title)
      | None,None ->
        let title =
          match kind with
          | Type _ | Logic _ -> "Parameter"
          | Value _ -> "Constrained Parameter"
          | Axiom _ -> "Hypothesis"
        in
        Pdoc.ppt env.out (pp_mark ~cla:icon_parameter ~title)

let process_assumed env kind =
  try
    let id = Id.resolve ~lib:env.src.lib @@ Axioms.ident kind in
    let key = match kind with
      | Type _ -> "type"
      | Logic _ -> "logic"
      | Value _ -> "value"
      | Axiom _ -> "axiom"
    in
    Pdoc.printf env.crc
      "<pre class=\"src\"> %a <a id=\"%a\" href=\"%a\">%a</a>%t</pre>"
      Pdoc.pp_keyword key
      Id.pp_proof_aname id
      (Id.pp_ahref ~scope:None) id
      Id.pp_local id
      (fun fmt -> pp_mark ~title:"Assumed" ~cla:icon_assumed fmt)
  with Not_found -> ()

let process_module_axioms env =
  match env.theory with
  | None -> ()
  | Some { theory } ->
    List.iter
      (fun thy ->
         List.iter
           (process_assumed env)
           (Axioms.assumed @@ Axioms.signature env.henv thy)
      ) (Axioms.dependencies env.henv theory)

type axioms = { ext : int ; prm : int ; hyp :  int }
let free = { ext = 0 ; prm = 0 ; hyp = 0 }

let add_axiom hs (p : Axioms.parameter) =
  match p with
  | { builtin = None ; extern = None ; kind = Axiom _ } ->
    { hs with hyp = succ hs.hyp }
  | { builtin = None ; extern = None } ->
    { hs with prm = succ hs.prm }
  | _ ->
    { hs with ext = succ hs.ext }

let pp_axioms fmt { ext ; prm ; hyp } =
  if ext+prm+hyp > 0 then
    let cla = if prm + hyp > 0 then icon_parameter else icon_external in
    let title =
      let text k single msg =
        if k = 0 then [] else if k = 1 then [single] else [msg k] in
      String.concat ", " @@ List.concat [
        text prm "1 parameter" @@ Printf.sprintf "%d parameters" ;
        text hyp "1 hypothesis" @@ Printf.sprintf "%d hypotheses" ;
        text ext "1 external symbol" @@ Printf.sprintf "%d external symbols" ;
      ] in
    pp_mark ~cla ~title fmt

let process_axioms_summary env = function
  | None -> ()
  | Some Docref.{ signature } ->
    let hs = List.fold_left add_axiom free @@ Axioms.parameters signature in
    Pdoc.pp env.out pp_axioms hs

(* -------------------------------------------------------------------------- *)
(* --- References                                                         --- *)
(* -------------------------------------------------------------------------- *)

let rec fetch_id input =
  match Token.token input with
  | Space | Newline -> fetch_id input
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
    Pdoc.printf env.out "<a id=\"%a\">%a</a>" Id.pp_aname id Pdoc.pp_html s ;
    process_axioms env id ;
    process_proof env id proof

  | Docref.Ref id ->
    if env.clone_decl > 0 && id.id_qid = [] then
      begin
        env.clone_order <- succ env.clone_order ;
        env.clone_path <- Id.cat (id.id_lib @ id.id_mod :: id.id_qid) ;
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

let rec pp_type ~env ~par fmt (ty : Why3.Ty.ty) =
  match ty.ty_node with
  | Tyvar tv -> Format.pp_print_string fmt tv.tv_name.id_string
  | Tyapp(ts,[]) -> pp_ident ~env fmt ts.ts_name
  | Tyapp(ts,args) ->
    begin
      if par then Format.pp_print_char fmt '(' ;
      pp_ident ~env fmt ts.ts_name ;
      List.iter (Format.fprintf fmt " %a" (pp_type ~env ~par:true)) args ;
      if par then Format.pp_print_char fmt ')' ;
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
    List.iter (Pdoc.pp env.out @@ pp_type ~env ~par:true) ls.ls_args ;
    definition env ?op def ;
  | Some r ->
    declare env n "function" ?attr ls.ls_name ;
    List.iter (Pdoc.pp env.out @@ pp_type ~env ~par:true) ls.ls_args ;
    Pdoc.printf env.out " : %a" (pp_type ~env ~par:false) r ;
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
         Pdoc.printf env.out " %a"
           (pp_type ~env ~par:true)
           Why3.Ity.(ty_of_ity x.pv_ity)
      ) rs.rs_cty.cty_args ;
    Pdoc.printf env.out " : %a"
      (pp_type ~env ~par:false)
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
          (pp_type ~env ~par:false) pv.pv_vs.vs_ty ;
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

let declaration env n id (th : Why3.Theory.theory) def =
  try
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

let by_source_line a b =
  let la = Id.line a.Docref.id_source in
  let lb = Id.line b.Docref.id_source in
  Int.compare la lb

let in_section env (c : Docref.clone) =
  let s = c.id_section in
  s.cloned_path = env.clone_path &&
  s.cloned_order = env.clone_order &&
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
  | Some { signature } ->
    let hs = List.fold_left
        (fun hs clone ->
           match Axioms.parameter signature clone.Docref.id_target with
           | None -> hs
           | Some p -> add_axiom hs p
        ) free clones in
    Pdoc.pp env.out pp_axioms hs

let process_clone_section env (th : Docref.theory) =
  let cloned =
    List.sort by_source_line @@
    List.filter (in_section env) th.clones
  in
  if cloned <> [] then
    begin
      text env ;
      let n0 = env.clone_decl in
      let n = n0 + 2 in
      Pdoc.printf env.out
        " <span class=\"section\">\
         {<span class=\"section-toggle\">…</span>\
         <span class=\"section-text\">@\n\
         %a<span class=\"comment section-toggle\">begin</span>@\n"
        Pdoc.pp_spaces n ;
      List.iter
        (fun (clone : Docref.clone) ->
           declaration env (n + 2) clone.id_target th.theory clone.id_source
        ) cloned ;
      Pdoc.printf env.out
        "%a<span class=\"comment section-toggle\">end</span>@\n\
         %a</span>}</span>"
        Pdoc.pp_spaces n Pdoc.pp_spaces n0 ;
      process_clone_axioms env cloned ;
      process_clone_proofs env cloned ;
    end

let process_clones env =
  begin
    if env.clone_path <> "" then
      match env.theory with
      | None -> ()
      | Some th ->
        process_clone_section env th ;
        env.clone_path <- "" ;
        env.clone_decl <- 0 ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Foldable Sections                                                  --- *)
(* -------------------------------------------------------------------------- *)

let pp_active fmt b =
  if b then Format.fprintf fmt " active"

let process_open_section env ~active title =
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
    let prelude = Pdoc.buffered env.out in
    let id = fetch_id env.input in
    let href = resolve env () in
    let kind = String.capitalize_ascii key in
    let url = Docref.derived env.src id in
    let path = String.concat "." env.src.lib in
    let theory = Docref.Mstr.find_opt id env.src.theories in
    Pdoc.printf env.out
      "<pre class=\"src\">%a <a title=\"%s.%s\" href=\"%s\">%s</a>"
      Pdoc.pp_keyword key path id url id ;
    Pdoc.printf env.crc
      "<pre class=\"src\">%a <a href=\"%s\">%s.%s</a>"
      Pdoc.pp_keyword key url path id ;
    process_axioms_summary env theory ;
    process_proofs_summary env ~path theory ;
    Pdoc.printf env.out "</pre>@." ;
    Pdoc.printf env.crc "</pre>@." ;
    let file = Filename.concat env.dir url in
    let title = Printf.sprintf "%s %s.%s" kind path id in
    Pdoc.fork env.out ~file ~title ;
    Pdoc.printf env.out
      "<header>\
       %s <code class=\"src\"><a href=\"%s\">%s</a>.%s</code>\
       </header>@\n"
      kind env.src.url path id ;
    Pdoc.pp env.out Format.pp_print_string prelude ;
    push env Pre ;
    env.file <- Pre ;
    env.scope <- Some id ;
    env.theory <- theory ;
    env.declared <- Sid.empty ;
    Pdoc.pp env.out Pdoc.pp_keyword key ;
    Pdoc.pp_print_char env.out ' ' ;
    process_href env href id ;
    process_axioms_summary env theory ;
    process_proofs_summary env theory ;
  end

let process_close_module env key =
  begin
    Pdoc.printf env.out "%a@\n</pre>@\n" Pdoc.pp_keyword key ;
    process_module_axioms env ;
    env.mode <- Body ;
    env.file <- Body ;
    env.scope <- None ;
    env.theory <- None ;
    Pdoc.close env.out ;
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
        if s = "clone" then
          env.clone_decl <- Token.indent env.input ;
        Pdoc.pp env.out Pdoc.pp_keyword s
    end
  else
    let href = resolve env () in
    process_href env href s

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
    Pdoc.pp_print_char env.out ' '

let process_newline env =
  match env.mode with
  | Body -> if Token.emptyline env.input then Pdoc.flush env.out
  | Div | List _ -> ()
  | Par | Item _ ->
    if Token.emptyline env.input then
      pop env
    else
      env.space <- true
  | Emph | Bold -> env.space <- true
  | Head(buffer,level) ->
    let title = Pdoc.buffered env.out in
    Pdoc.pop env.out buffer ;
    Pdoc.header env.out ~level ~title () ;
    pop env
  | Pre ->
    Pdoc.pp_print_char env.out '\n' ;
    Pdoc.flush env.out

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
(* --- File Processing                                                    --- *)
(* -------------------------------------------------------------------------- *)

let process_file ~wenv ~henv ~out:dir file =
  let src = Docref.parse ~henv ~wenv file in
  let path = String.concat "." src.lib in
  let title = Printf.sprintf "Library %s" path in
  let ofile = Filename.concat dir src.url in
  let out = Pdoc.output ~file:ofile ~title in
  let crc = Pdoc.output
      ~file:(Filename.concat dir ("_" ^ src.url))
      ~title:(Printf.sprintf "Proofs %s" path) in
  let input = Token.input file in
  let env = {
    dir ; src ; input ; out ; crc ; space = false ; henv ;
    scope = None ; theory = None ;
    mode = Body ; file = Body ; stack = [] ;
    clone_decl = 0 ;
    clone_path = "" ;
    clone_order = 0 ;
    proof_href = 0 ;
    declared = Sid.empty ;
    opened = 0 ; section = 0 ;
  } in
  begin
    Pdoc.printf out "<header>Library <code>%s</code></header>@\n" path ;
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
    while not (Token.eof env.input) do
      match Token.token env.input with
      | Eof -> close env
      | Char c -> text env ; Pdoc.pp_html_c out c
      | Text s -> text env ; Pdoc.pp_html_s out s
      | Comment s ->
        text env ;
        Pdoc.pp_html_s out ~className:"comment" s
      | Verb s ->
        text env ;
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
        text env ;
        process_ident env s ;
        process_clones env
      | Infix s ->
        text env ;
        let href = resolve env ~infix:true () in
        process_href env href s
    done ;
    Pdoc.close_all out ;
    Pdoc.close_all crc ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Main Doc Command                                                   --- *)
(* -------------------------------------------------------------------------- *)

let shared ~out ~file =
  let tgt = Filename.concat out file in
  let src = Meta.shared file in
  Utils.copy ~src ~tgt

let generate ~out ~files =
  begin
    let wenv, henv = Docref.init () in
    Utils.mkdirs @@ Filename.concat out "fonts" ;
    shared ~out ~file:"style.css" ;
    shared ~out ~file:"script.js" ;
    shared ~out ~file:"icofont.min.css" ;
    shared ~out ~file:"fonts/icofont.woff" ;
    shared ~out ~file:"fonts/icofont.woff2" ;
    List.iter (process_file ~wenv ~henv ~out) files
  end

(* -------------------------------------------------------------------------- *)
