(*
   Copyright 2008-2015 Abhishek Anand, Nikhil Swamy and Microsoft Research

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)
module FStar.Extraction.ML.RegEmb

(* This module handles registering plugins and generating
embeddings for their types. *)

open FStar
open FStar.Compiler
open FStar.Compiler.Effect
open FStar.Compiler.List
open FStar.Const
open FStar.Extraction.ML.Syntax
open FStar.Extraction.ML.UEnv
open FStar.Syntax.Syntax

module BU    = FStar.Compiler.Util
module Code  = FStar.Extraction.ML.Code
module EMB   = FStar.Syntax.Embeddings
module PC    = FStar.Parser.Const
module Print = FStar.Syntax.Print
module S     = FStar.Syntax.Syntax
module SS    = FStar.Syntax.Subst
module Term  = FStar.Extraction.ML.Term
module U     = FStar.Syntax.Util
module Util  = FStar.Extraction.ML.Util

exception Unsupported of string

let splitlast s = let x::xs = List.rev s in (List.rev xs, x)

let mk e = with_ty MLTY_Top e

let ml_name : Ident.lid -> mlexpr =
  fun l ->
    let s = Ident.path_of_lid l in
    let ns, id = splitlast s in
    mk (MLE_Name (ns, id))
let ml_ctor : Ident.lid -> list mlexpr -> mlexpr =
  fun l args ->
    let s = Ident.path_of_lid l in
    let ns, id = splitlast s in
    mk (MLE_CTor ((ns, id), args))

let ml_none : mlexpr = mk (MLE_Name (["FStar"; "Pervasives"; "Native"], "None"))
let ml_some : mlexpr = mk (MLE_Name (["FStar"; "Pervasives"; "Native"], "Some"))

let tm_fvar_lid    = Ident.lid_of_str "FStar.Syntax.Syntax.Tm_fvar"
let fv_eq_lid_lid  = Ident.lid_of_str "FStar.Syntax.Syntax.fv_eq_lid"
let s_fvar_lid     = Ident.lid_of_str "FStar.Syntax.Syntax.fvar"
let lid_of_str_lid = Ident.lid_of_str "FStar.Ident.lid_of_str" // :^)
let mk_app_lid     = Ident.lid_of_str "FStar.Syntax.Util.mk_app"
let nil_lid        = Ident.lid_of_str "Prims.Nil"
let cons_lid       = Ident.lid_of_str "Prims.Cons"
let embed_lid      = Ident.lid_of_str "FStar.Syntax.Embeddings.Base.extracted_embed"

let ml_magic : mlexpr =
  mk (MLE_Coerce (ml_unit, MLTY_Top, MLTY_Top))

let embedding_for (a:S.typ) : mlexpr =
  ml_magic

let rec as_ml_list (ts : list mlexpr) : mlexpr =
  match ts with
  | [] -> ml_ctor nil_lid []
  | t::ts -> ml_ctor cons_lid [t; as_ml_list ts]

let fresh : string -> string =
  let r = BU.mk_ref 0 in
  fun s ->
    let v = !r in
    r := v+1;
    s^"_"^(string_of_int v)

let mk_unembed (ctors: list sigelt) : mlexpr =
  let e_branches : ref (list mlbranch) = BU.mk_ref [] in
  let arg_v = fresh "tm" in
  ctors |> List.iter (fun ctor ->
    match ctor.sigel with
    | Sig_datacon {lid; us; t; ty_lid; num_ty_params; mutuals} ->
      let fv = fresh "fv" in
      let bs, c = U.arrow_formals t in
      let vs = List.map (fun b -> fresh (Ident.string_of_id b.binder_bv.ppname), b.binder_bv.sort) bs in

      let pat_s = MLP_Const (MLC_String (Ident.string_of_lid lid)) in
      let pat_args = MLP_CTor ((["Prims"], "Nil"), List.map (fun (v, _) -> MLP_Var v) vs) in
      let pat_both = MLP_Tuple [pat_s; pat_args] in

      let head = ml_name lid in
      let ret = mk (MLE_App (head, List.map (fun (v, _) -> mk (MLE_Var v)) vs)) in
      let ret = mk (MLE_App (ml_some, [ret])) in
      let br = (pat_both, None, ret) in

      e_branches := br :: !e_branches
    | _ -> failwith "impossible, filter above"
  );
  let nomatch : mlbranch = (MLP_Wild, None, ml_none) in
  let branches = List.rev (nomatch :: !e_branches) in
  let sc = mk (MLE_Var arg_v) in
  let def = mk (MLE_Match (sc, branches)) in
  let lam = mk (MLE_Fun ([arg_v, MLTY_Top], def)) in
  lam

let mk_embed (ctors: list sigelt) : mlexpr =
  let e_branches : ref (list mlbranch) = BU.mk_ref [] in
  let arg_v = fresh "tm" in
  ctors |> List.iter (fun ctor ->
    match ctor.sigel with
    | Sig_datacon {lid; us; t; ty_lid; num_ty_params; mutuals} ->
      let fv = fresh "fv" in
      let bs, c = U.arrow_formals t in
      let vs = List.map (fun b -> fresh (Ident.string_of_id b.binder_bv.ppname), b.binder_bv.sort) bs in
      let pat = MLP_CTor (splitlast (Ident.path_of_lid lid), List.map (fun v -> MLP_Var (fst v)) vs) in
      let fvar = ml_name s_fvar_lid in
      let lid_of_str = ml_name lid_of_str_lid in
      let head = mk (MLE_App (fvar, [
                    mk (MLE_App (lid_of_str, [mk (MLE_Const (MLC_String (Ident.string_of_lid lid)))]));
                    ml_none]))
      in
      let mk_mk_app t ts =
        mk (MLE_App (ml_name mk_app_lid, [t; as_ml_list ts]))
      in
      let args =
        vs |> List.map (fun (v, ty) ->
          let vt = mk (MLE_Var v) in
          mk (MLE_App (ml_name embed_lid, [embedding_for ty; vt]))
        )
      in
      let ret = mk_mk_app head args in
      let br = (pat, None, ret) in

      e_branches := br :: !e_branches
    | _ -> failwith "impossible, filter above"
  );
  let branches = List.rev !e_branches in
  let sc = mk (MLE_Var arg_v) in
  let def = mk (MLE_Match (sc, branches)) in
  let lam = mk (MLE_Fun ([arg_v, MLTY_Top], def)) in
  lam

let __do_handle_plugin (g: uenv) (arity_opt: option int) (se: sigelt) : list mlmodule1 =
  // BU.print2 "Got plugin with attrs = %s; arity_opt=%s"
  //          (List.map Print.term_to_string se.sigattrs |> String.concat " ")
  //          (match arity_opt with None -> "None" | Some x -> "Some " ^ string_of_int x);
  let w = with_ty MLTY_Top in
  let r = se.sigrng in
  match se.sigel with
  | Sig_let {lbs} ->
      let mk_registration lb : list mlmodule1 =
         let fv = BU.right lb.lbname in
         let fv_lid = fv.fv_name.v in
         let fv_t = lb.lbtyp in
         let ml_name_str = MLE_Const (MLC_String (Ident.string_of_lid fv_lid)) in
         match Util.interpret_plugin_as_term_fun g fv fv_t arity_opt ml_name_str with
         | Some (interp, nbe_interp, arity, plugin) ->
             let register, args =
               if plugin
               then (["FStar_Tactics_Native"], "register_plugin"), [interp; nbe_interp]
               else (["FStar_Tactics_Native"], "register_tactic"), [interp]
             in
             let h = with_ty MLTY_Top <| MLE_Name register in
             let arity  = MLE_Const (MLC_Int(string_of_int arity, None)) in
             let app = with_ty MLTY_Top <| MLE_App (h, [w ml_name_str; w arity] @ args) in
             [MLM_Top app]
         | None -> []
      in
      List.collect mk_registration (snd lbs)

  | Sig_bundle {ses} ->
    let typ_sigelt =
      match List.filter (fun se -> match se.sigel with | Sig_inductive_typ _ -> true | _ -> false) ses with
      | [se] -> se
      | _ -> raise (Unsupported "mutual inductives")
    in
    let Sig_inductive_typ {lid=tlid} = typ_sigelt.sigel in
    let name = Ident.string_of_id (List.last (Ident.ids_of_lid tlid)) in

    let ctors = List.filter (fun se -> match se.sigel with | Sig_datacon _ -> true | _ -> false) ses in
    let ml_name = mk (MLE_Const (MLC_String (Ident.string_of_lid tlid))) in
    let ml_unembed = mk_unembed ctors in
    let ml_embed = mk_embed ctors in
    let def = mk (MLE_App (mk (MLE_Name (["FStar"; "Syntax"; "Embeddings"; "Base"], "mk_extracted_embedding")), [
                    ml_name;
                    ml_unembed;
                    ml_embed]))
    in
    let lb = {
      mllb_name     = "e_" ^ name;
      mllb_tysc     = None;
      mllb_add_unit = false;
      mllb_def      = def;
      mllb_meta     = [];
      print_typ     = false;
    }
    in
    [MLM_Let (NonRec, [lb])]

  | _ -> []

let do_handle_plugin (g: uenv) (arity_opt: option int) (se: sigelt) : list mlmodule1 =
  try __do_handle_plugin g arity_opt se with
  | Unsupported msg ->
    // FIXME: Change error code
    Errors.log_issue se.sigrng (Errors.Warning_PluginNotImplemented,
        BU.format2 "Could not generate a plugin for %s, reason = %s" (Print.sigelt_to_string_short se) msg);
    []

(* When extracting a plugin, each top-level definition marked with a `@plugin` attribute
   is extracted along with an invocation to FStar.Tactics.Native.register_tactic or register_plugin,
   which installs the compiled term as a primitive step in the normalizer
 *)
let maybe_register_plugin (g:uenv) (se:sigelt) : list mlmodule1 =
    let plugin_with_arity attrs =
        BU.find_map attrs (fun t ->
              let head, args = U.head_and_args t in
              if not (U.is_fvar PC.plugin_attr head)
              then None
              else match args with
                   | [({n=Tm_constant (Const_int(s, _))}, _)] ->
                     Some (Some (BU.int_of_string s))
                   | _ -> Some None)
    in
    if Options.codegen() <> Some Options.Plugin
    then []
    else
      match plugin_with_arity se.sigattrs with
      | None -> []
      (* ignore projectors and discriminators, they get a @@plugin attribute inherited
      from the type, but we don't need to handle them. *)
      | Some _ when List.existsb (function Projector _ | Discriminator _ -> true | _ -> false) se.sigquals ->
        []
      | Some arity_opt ->
        do_handle_plugin g arity_opt se
