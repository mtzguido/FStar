module FStarC.Html

open FStarC.Effect
open FStarC.Util

module Env = FStarC.TypeChecker.Env
module S = FStarC.Syntax.Syntax
module Range = FStarC.Range.Ops
module Find = FStarC.Find
module Filepath = FStarC.Filepath
module BU = FStarC.Util
module SB = FStarC.StringBuffer
module Tok = FStarC.Html.Tokenizer
module Print = FStarC.Syntax.Print.Pretty
module Stats = FStarC.Stats
open FStarC.Html.Tokenizer

(* ---------- CSS ---------- *)

let css_text : string =
  String.concat "\n" [
    "<style>";
    "body { margin: 0; padding: 0; background: #fafafa; }";
    "pre.fstar-source {";
    "  font-family: 'SFMono-Regular', Consolas, 'Liberation Mono', Menlo, monospace;";
    "  font-size: 14px;";
    "  line-height: 1.6;";
    "  padding: 0;";
    "  margin: 0;";
    "  overflow-x: auto;";
    "  counter-reset: line;";
    "}";
    "pre.fstar-source > span {";
    "  display: block;";
    "  padding: 0 16px 0 60px;";
    "  text-indent: -60px;";
    "  counter-increment: line;";
    "}";
    "pre.fstar-source > span::before {";
    "  content: counter(line);";
    "  display: inline-block;";
    "  width: 44px;";
    "  text-align: right;";
    "  margin-right: 16px;";
    "  color: #999;";
    "  font-size: 13px;";
    "  -webkit-user-select: none;";
    "  user-select: none;";
    "  text-indent: 0;";
    "}";
    "pre.fstar-source > span:nth-child(odd) { background: #f0f0f0; }";
    "pre.fstar-source > span:nth-child(even) { background: #f7f7f7; }";
    "pre.fstar-source > span:hover { background: #fffde7; }";
    "pre.fstar-source > span:target { background: #fff3b0; }";
    ".kw { color: #0000ff; font-weight: bold; }";
    ".op { color: #555; }";
    ".str { color: #a31515; }";
    ".num { color: #098658; }";
    ".ch { color: #a31515; }";
    ".cm { color: #008000; font-style: italic; }";
    ".cn { color: #267f99; }";
    ".at { color: #7d4e8d; }";
    ".pp { color: #808080; }";
    ".id { }";
    "a { color: inherit; text-decoration: none; }";
    "a:hover { text-decoration: underline; }";
    ".nav { font-family: sans-serif; font-size: 13px; padding: 8px 16px;";
    "  background: #f0f0f0; border-bottom: 1px solid #ddd; }";
    ".nav a { color: #0366d6; }";
    "[data-tip] { cursor: help; }";
    "</style>";
    "<script>";
    "document.addEventListener('DOMContentLoaded',function(){";
    "var t=document.createElement('div');t.id='tip';";
    "t.style.cssText='display:none;position:fixed;z-index:10;white-space:pre-wrap;word-break:break-all;max-width:90vw;font:13px SFMono-Regular,Consolas,Liberation Mono,Menlo,monospace;color:#333;background:#f8f8f0;border:1px solid #ccc;border-radius:4px;padding:4px 8px;box-shadow:0 2px 6px rgba(0,0,0,.15);pointer-events:none';";
    "document.body.appendChild(t);";
    "document.addEventListener('mouseover',function(e){var el=e.target.closest('[data-tip]');if(el){t.textContent=el.getAttribute('data-tip');t.style.display='block'}});";
    "document.addEventListener('mouseout',function(e){if(e.target.closest('[data-tip]'))t.style.display='none'});";
    "document.addEventListener('mousemove',function(e){if(t.style.display==='block'){var x=e.clientX+12,y=e.clientY+12;if(x+t.offsetWidth>window.innerWidth)x=e.clientX-t.offsetWidth-4;if(y+t.offsetHeight>window.innerHeight)y=e.clientY-t.offsetHeight-4;t.style.left=x+'px';t.style.top=y+'px'}});";
    "});";
    "</script>"
  ]

(* ---------- HTML helpers ---------- *)

let html_escape (s : string) : ML string =
  let len = FStarC.String.strlen s in
  let buf = SB.create (len + 16) in
  let rec go (buf : SB.t) (i : int) : ML SB.t =
    if i >= len then buf
    else
      let c = FStarC.String.index s i in
      let buf =
        if c = BU.char_of_int 38 then SB.add "&amp;" buf
        else if c = BU.char_of_int 60 then SB.add "&lt;" buf
        else if c = BU.char_of_int 62 then SB.add "&gt;" buf
        else if c = BU.char_of_int 34 then SB.add "&quot;" buf
        else SB.add (FStarC.String.make 1 c) buf
      in
      go buf (i + 1)
  in
  SB.contents (go buf 0)

let css_class_string (cls : css_class) : string =
  match cls with
  | Kw  -> "kw"
  | Op  -> "op"
  | Str -> "str"
  | Num -> "num"
  | Ch  -> "ch"
  | Cm  -> "cm"
  | Cn  -> "cn"
  | At  -> "at"
  | Pp  -> "pp"
  | Id  -> "id"

let html_header (title : string) : ML string =
  String.concat "\n" [
    "<!DOCTYPE html>";
    "<html>";
    "<head>";
    "<meta charset=\"utf-8\">";
    "<title>" ^ html_escape title ^ "</title>";
    css_text;
    "</head>";
    "<body>"
  ]

let html_footer : string =
  "</pre>\n</body>\n</html>\n"

(* ---------- Identifier resolution ---------- *)

let html_filename_of (basename : string) : string =
  basename ^ ".html"

(* Prepare the dsenv for name resolution by applying the opens/abbrevs
   from the checked module's last declaration. *)
let prepare_dsenv_for_module
  (env : Env.env)
  (modul : S.modul)
  : ML FStarC.Syntax.DsEnv.env
=
  let dsenv = env.dsenv in
  (* Get opens from the last declaration (which accumulates all opens) *)
  match List.rev modul.S.declarations with
  | [] -> dsenv
  | last :: _ ->
    List.fold_left (fun dsenv oa ->
      match oa with
      | Inl (lid, _kind, restriction) ->
        let (_, r) = FStarC.Errors.catch_errors (fun () ->
          FStarC.Syntax.DsEnv.push_namespace dsenv lid restriction
        ) in
        (match r with Some dsenv' -> dsenv' | None -> dsenv)
      | Inr (_abbrev_id, _abbrev_lid) ->
        dsenv
    ) dsenv last.S.sigopens_and_abbrevs

let try_qname (env : Env.env) (this_file : string) (l : FStarC.Ident.lident) : ML (option (string & option string)) =
  let qn = Stats.record "html.lookup_qname" (fun () -> Env.lookup_qname env l) in
  match qn with
  | Some (Inr (se, _), _rng) ->
    let def_range = se.S.sigrng in
    let def_file = Range.file_of_range def_range in
    let def_start = Range.start_of_range def_range in
    let def_line = Range.line_of_pos def_start in
    let url =
      if def_file = this_file then
        "#L" ^ string_of_int def_line
      else
        html_filename_of def_file ^ "#L" ^ string_of_int def_line
    in
    (* Extract type from the sigelt for tooltip *)
    let print_type (t : S.term) : ML string =
      Stats.record "html.term_to_string" (fun () -> Print.term_to_string t)
    in
    let type_str =
      match se.S.sigel with
      | S.Sig_let { lbs = (_, lbs) } ->
        (match List.tryFind (fun (lb : S.letbinding) ->
          match lb.S.lbname with
          | Inr fv -> FStarC.Ident.lid_equals (S.lid_of_fv fv) l
          | _ -> false
        ) lbs with
        | Some lb -> Some (FStarC.Ident.string_of_lid l ^ " : " ^ print_type lb.S.lbtyp)
        | None -> None)
      | S.Sig_declare_typ { t } ->
        Some (FStarC.Ident.string_of_lid l ^ " : " ^ print_type t)
      | S.Sig_inductive_typ { t } ->
        Some (FStarC.Ident.string_of_lid l ^ " : " ^ print_type t)
      | S.Sig_datacon { t } ->
        Some (FStarC.Ident.string_of_lid l ^ " : " ^ print_type t)
      | _ -> Some (FStarC.Ident.string_of_lid l)
    in
    Some (url, type_str)
  | _ -> None

(* Try to resolve a token's text to a definition-site link and type tooltip. *)
let resolve_token
  (env : Env.env)
  (dsenv : FStarC.Syntax.DsEnv.env)
  (this_file : string)
  (token_text : string)
  : ML (option (string & option string))
=
  let path = FStarC.String.split [FStarC.Util.char_of_int 46 (* '.' *)] token_text in
  let lid = FStarC.Ident.lid_of_path path FStarC.Range.Type.dummyRange in
  let resolved = Stats.record "html.resolve_fqn" (fun () ->
    FStarC.Syntax.DsEnv.resolve_to_fully_qualified_name dsenv lid
  ) in
  match resolved with
  | Some fq_lid -> try_qname env this_file fq_lid
  | None -> try_qname env this_file lid

(* ---------- Annotation building ---------- *)

type annotation = {
  ann_start_line : int;
  ann_start_col  : int;
  ann_end_line   : int;
  ann_end_col    : int;
  ann_cls        : css_class;
  ann_text       : string;
  ann_link       : option string;
  ann_title      : option string;
}

(* Try to resolve a dotted module path to a link.
   Checks against known loaded modules in the environment. *)
let resolve_module
  (env : Env.env)
  (module_text : string)
  : ML (option string)
=
  let found = List.tryFind (fun (m : S.modul) ->
    FStarC.Ident.string_of_lid m.S.name = module_text
  ) (Env.modules env) in
  match found with
  | Some m ->
    let mn = FStarC.Ident.string_of_lid m.S.name in
    let def_file =
      if m.S.is_interface then mn ^ ".fsti"
      else mn ^ ".fst"
    in
    Some (html_filename_of def_file)
  | None -> None

(* Post-process annotations: merge adjacent Cn.Cn.Cn sequences that
   form known module names into single linked annotations. *)
let linkify_modules
  (env : Env.env)
  (anns : list annotation)
  : ML (list annotation)
=
  let rec go (acc : list annotation) (rest : list annotation) : ML (list annotation) =
    match rest with
    | [] -> List.rev acc
    | a :: rest' ->
      if a.ann_cls = Cn && None? a.ann_link then
        (* Try to collect a dotted module path: Cn . Cn . Cn ... *)
        let rec collect_path (text : string) (remaining : list annotation) : string & list annotation =
          match remaining with
          | dot :: name :: rest''
            when dot.ann_cls = Op && dot.ann_text = "." && name.ann_cls = Cn ->
            collect_path (text ^ "." ^ name.ann_text) rest''
          | _ -> (text, remaining)
        in
        let (full_path, _remaining) = collect_path a.ann_text rest' in
        (match resolve_module env full_path with
         | Some url ->
           (* Replace the first Cn's link, leave the dots/subsequent Cn tokens with same link *)
           let a' = { a with ann_link = Some url } in
           (* Also link the intermediate dots and Cn tokens *)
           let rec link_rest (acc : list annotation) (remain : list annotation) (n_to_link : int) : list annotation & list annotation =
             if n_to_link <= 0 then (acc, remain)
             else match remain with
               | tok :: rest'' ->
                 link_rest ({ tok with ann_link = Some url } :: acc) rest'' (n_to_link - 1)
               | [] -> (acc, [])
           in
           (* Count how many tokens we consumed (pairs of dot+name after the first) *)
           let parts = FStarC.String.split [FStarC.Util.char_of_int 46] full_path in
           let extra_tokens = op_Multiply (List.length parts - 1) 2 in (* dot+name pairs *)
           let (linked, remaining') = link_rest [a'] rest' extra_tokens in
           go (linked @ acc) remaining'
         | None ->
           go (a :: acc) rest')
      else
        go (a :: acc) rest'
  in
  go [] anns

(* Collect local binder type info from the checked module.
   Walks each declaration's terms to find binders and record
   their positions → type strings. Returns a map keyed by
   (basename, line, col). *)
let collect_binder_types (modul : S.modul) : ML (FStarC.SMap.t string) =
  let map : FStarC.SMap.t string = FStarC.SMap.create 256 in
  let record_binder (b : S.binder) : ML unit =
    let bv = b.S.binder_bv in
    let rng = S.range_of_bv bv in
    let file = Range.file_of_range rng in
    let start = Range.start_of_range rng in
    let line = Range.line_of_pos start in
    let col = Range.col_of_pos start in
    let ty_str = FStarC.Ident.string_of_id bv.S.ppname ^ " : " ^ Print.term_to_string bv.S.sort in
    let key = file ^ ":" ^ string_of_int line ^ ":" ^ string_of_int col in
    FStarC.SMap.add map key ty_str
  in
  let rec walk_term (t : S.term) : ML unit =
    match t.S.n with
    | S.Tm_abs { bs; body } ->
      List.iter record_binder bs;
      walk_term body
    | S.Tm_arrow { bs; comp = c } ->
      List.iter record_binder bs;
      walk_comp c
    | S.Tm_refine { b; phi } ->
      record_binder { S.binder_bv = b; S.binder_qual = None; S.binder_positivity = None; S.binder_attrs = [] };
      walk_term phi
    | S.Tm_app { hd; args } ->
      walk_term hd;
      List.iter (fun (t, _) -> walk_term t) args
    | S.Tm_match { scrutinee; brs } ->
      walk_term scrutinee;
      List.iter (fun (_, _, body) -> walk_term body) brs
    | S.Tm_let { lbs = (_, lbs); body } ->
      List.iter (fun (lb : S.letbinding) ->
        walk_term lb.S.lbtyp;
        walk_term lb.S.lbdef
      ) lbs;
      walk_term body
    | S.Tm_ascribed { tm; asc = (asc, _, _) } ->
      walk_term tm;
      (match asc with Inl t -> walk_term t | Inr _ -> ())
    | S.Tm_meta { tm } -> walk_term tm
    | _ -> ()
  and walk_comp (c : S.comp) : ML unit =
    match c.S.n with
    | S.Total t -> walk_term t
    | S.GTotal t -> walk_term t
    | S.Comp ct -> walk_term ct.S.result_typ
  in
  List.iter (fun (se : S.sigelt) ->
    match se.S.sigel with
    | S.Sig_let { lbs = (_, lbs) } ->
      List.iter (fun (lb : S.letbinding) ->
        walk_term lb.S.lbtyp;
        walk_term lb.S.lbdef
      ) lbs
    | S.Sig_declare_typ { t } -> walk_term t
    | S.Sig_inductive_typ { params; t } ->
      List.iter record_binder params;
      walk_term t
    | S.Sig_datacon { t } -> walk_term t
    | _ -> ()
  ) modul.S.declarations;
  map

(* Look up a local binder type from the collected info *)
let lookup_local_type
  (binder_info : FStarC.SMap.t string)
  (file : string)
  (line : int)
  (col : int)
  : ML (option string)
=
  let key = file ^ ":" ^ string_of_int line ^ ":" ^ string_of_int col in
  FStarC.SMap.try_find binder_info key

let build_annotations
  (env : Env.env)
  (dsenv : FStarC.Syntax.DsEnv.env)
  (binder_info : FStarC.SMap.t string)
  (source_file : string)
  (spans : list token_span)
  : ML (list annotation)
=
  let this_basename = Filepath.basename source_file in
  (* Cache: token text → resolution result *)
  let cache : FStarC.SMap.t (option (string & option string)) = FStarC.SMap.create 256 in
  let resolve_cached (text : string) : ML (option (string & option string)) =
    match FStarC.SMap.try_find cache text with
    | Some r -> r
    | None ->
      let r = resolve_token env dsenv this_basename text in
      FStarC.SMap.add cache text r;
      r
  in
  let anns = List.map (fun (span : token_span) ->
    let resolved =
      match span.cls with
      | Id | Cn -> resolve_cached span.text
      | _ -> None
    in
    let (link, title) = match resolved with
      | Some (url, t) -> (Some url, t)
      | None ->
        (* Try local binder info for hover *)
        let local_tip =
          match span.cls with
          | Id | Cn -> Stats.record "html.lookup_local" (fun () -> lookup_local_type binder_info this_basename span.start_line span.start_col)
          | _ -> None
        in
        (None, local_tip)
    in
    { ann_start_line = span.start_line;
      ann_start_col  = span.start_col;
      ann_end_line   = span.end_line;
      ann_end_col    = span.end_col;
      ann_cls        = span.cls;
      ann_text       = span.text;
      ann_link       = link;
      ann_title      = title }
  ) spans in
  let anns = Stats.record "html.linkify_modules" (fun () -> linkify_modules env anns) in
  Stats.record "html.linkify_qualified" (fun () ->
  (* Post-process: resolve dotted qualified names like Classical.forall_intro.
     When we see a sequence Cn/Id . Id/Cn where the components are unresolved,
     try resolving the full dotted path. *)
  let rec linkify_qualified (acc : list annotation) (rest : list annotation) : ML (list annotation) =
    match rest with
    | [] -> List.rev acc
    | a :: dot :: b :: rest'
      when (a.ann_cls = Cn || a.ann_cls = Id)
        && dot.ann_cls = Op && dot.ann_text = "."
        && (b.ann_cls = Cn || b.ann_cls = Id)
        && None? a.ann_link && None? b.ann_link ->
      (* Collect the full dotted path *)
      let rec collect (text : string) (tokens : list annotation) (remaining : list annotation)
        : string & list annotation & list annotation =
        match remaining with
        | d :: n :: rest''
          when d.ann_cls = Op && d.ann_text = "."
            && (n.ann_cls = Cn || n.ann_cls = Id) && None? n.ann_link ->
          collect (text ^ "." ^ n.ann_text) (tokens @ [d; n]) rest''
        | _ -> (text, tokens, remaining)
      in
      let full_text = a.ann_text ^ "." ^ b.ann_text in
      let (full_path, extra_tokens, remaining) = collect full_text [dot; b] rest' in
      (match resolve_token env dsenv this_basename full_path with
       | Some (url, tip) ->
         let a' = { a with ann_link = Some url; ann_title = tip } in
         let linked = List.map (fun tok -> { tok with ann_link = Some url; ann_title = tip }) extra_tokens in
         linkify_qualified (List.rev linked @ (a' :: acc)) remaining
       | None ->
         linkify_qualified (a :: acc) (dot :: b :: rest'))
    | a :: rest' ->
      linkify_qualified (a :: acc) rest'
  in
  linkify_qualified [] anns
  )

(* ---------- HTML emission ---------- *)

(* Split source text into lines. Each line includes its trailing newline if present. *)
let split_lines (s : string) : list string =
  FStarC.String.split ['\n'] s

let emit_html_core
  (basename : string)
  (lines : list string)
  (annotations : list annotation)
  (source_text : string)
  : ML string
=
  let buf = SB.create (op_Multiply (FStarC.String.strlen source_text) 2) in
  let buf = SB.add (html_header basename) buf in
  (* Add fst↔fsti companion link before the <pre> *)
  let buf =
    let companion =
      if BU.ends_with basename ".fst"
      then Some (basename ^ "i")
      else if BU.ends_with basename ".fsti"
      then Some (FStarC.String.substring basename 0 (FStarC.String.strlen basename - 1))
      else None
    in
    match companion with
    | Some c ->
      SB.add ("<div class=\"nav\"><a href=\"" ^ html_filename_of c ^ "\">&#x2192; " ^ html_escape c ^ "</a></div>\n") buf
    | None -> buf
  in
  let buf = SB.add "<pre class=\"fstar-source\">" buf in
  let buf = Stats.record "html.emit.lines" (fun () ->
  List.fold_left (fun (buf, active_ann, remaining_anns) (line_idx, line_text) ->
    let line_num = line_idx + 1 in
    let buf = SB.add ("<span id=\"L" ^ string_of_int line_num ^ "\">") buf in
    (* Re-open a carried-over multi-line annotation *)
    let buf = match active_ann with
      | Some a ->
        let cls_str = css_class_string a.ann_cls in
        if a.ann_cls = Id && None? a.ann_title then buf
        else SB.add ("<span class=\"" ^ cls_str ^ "\">") buf
      | None -> buf
    in
    (* Take annotations that start on this line from the front of the sorted list *)
    let rec take_line (acc : list annotation) (rest : list annotation) : list annotation & list annotation =
      match rest with
      | a :: rest' when a.ann_start_line = line_num -> take_line (a :: acc) rest'
      | _ -> (List.rev acc, rest)
    in
    let (line_anns, remaining_anns) = take_line [] remaining_anns in
    let line_len = FStarC.String.strlen line_text in
    (* Walk through columns of this line *)
    let rec walk (buf : SB.t) (col : int) (active : option annotation) (pending : list annotation) : ML (SB.t & option annotation) =
      (* Check if current annotation ends at this column *)
      let (buf, active) =
        match active with
        | Some a when a.ann_end_line = line_num && a.ann_end_col <= col ->
          let buf = match a.ann_link with
            | Some _ -> SB.add "</a></span>" buf
            | None ->
              if a.ann_cls = Id && None? a.ann_title then buf
              else SB.add "</span>" buf
          in
          (buf, None)
        | _ -> (buf, active)
      in
      if col >= line_len then (buf, active)
      else
        (* Check if a new annotation starts at this column *)
        let (buf, active, pending) =
          match active, pending with
          | None, a :: rest when a.ann_start_col = col ->
            let cls_str = css_class_string a.ann_cls in
            let tip_attr = match a.ann_title with
              | Some t -> " data-tip=\"" ^ html_escape t ^ "\""
              | None -> ""
            in
            let buf = match a.ann_link with
              | Some url ->
                SB.add ("<span class=\"" ^ cls_str ^ "\"" ^ tip_attr ^ "><a href=\"" ^ url ^ "\">") buf
              | None ->
                if a.ann_cls = Id && None? a.ann_title then buf
                else SB.add ("<span class=\"" ^ cls_str ^ "\"" ^ tip_attr ^ ">") buf
            in
            (buf, Some a, rest)
          | _ -> (buf, active, pending)
        in
        (* Emit the character *)
        let c = FStarC.String.index line_text col in
        let buf =
          if c = '&' then SB.add "&amp;" buf
          else if c = '<' then SB.add "&lt;" buf
          else if c = '>' then SB.add "&gt;" buf
          else if c = '"' then SB.add "&quot;" buf
          else SB.add (FStarC.String.make 1 c) buf
        in
        walk buf (col + 1) active pending
    in
    let (buf, active_ann) = Stats.record "html.emit.walk" (fun () -> walk buf 0 active_ann line_anns) in
    (* Close the annotation span if still active, so the line </span> nests properly *)
    let buf = match active_ann with
      | Some a ->
        if a.ann_cls = Id && None? a.ann_title then buf
        else SB.add "</span>" buf
      | None -> buf
    in
    (* Close the line span and add newline *)
    let buf = SB.add "</span>" buf in
    (buf, active_ann, remaining_anns)
  ) (buf, None, annotations) (List.mapi (fun i l -> (i, l)) lines)
  ) in
  let (buf, _, _) = buf in
  let buf = SB.add html_footer buf in
  Stats.record "html.emit.contents" (fun () -> SB.contents buf)

(* ---------- Main entry point ---------- *)

let generate_html
  (env : Env.env)
  (modul : S.modul)
  (filenames : list string)
  : ML unit
=
  let dsenv = Stats.record "html.dsenv_prep" (fun () -> prepare_dsenv_for_module env modul) in
  let binder_info = Stats.record "html.binder_collect" (fun () -> collect_binder_types modul) in
  List.iter (fun source_file ->
    let basename = Filepath.basename source_file in
    let source_text = BU.file_get_contents source_file in
    let spans = Stats.record "html.tokenize" (fun () -> tokenize_file source_file) in
    let annotations = Stats.record "html.annotate" (fun () -> build_annotations env dsenv binder_info source_file spans) in
    let html_content = Stats.record "html.emit" (fun () ->
      let lines = split_lines source_text in
      emit_html_core basename lines annotations source_text
    ) in
    let out_name = html_filename_of basename in
    let out_path = Find.prepend_output_dir out_name in
    BU.write_file out_path html_content;
    if not (FStarC.Options.silent ()) then
      FStarC.Format.print1 "Wrote %s\n" out_path
  ) filenames
