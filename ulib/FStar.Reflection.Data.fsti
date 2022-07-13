(*
   Copyright 2008-2018 Microsoft Research

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
module FStar.Reflection.Data

open FStar.Reflection.Types

noeq
type vconst =
  | C_Unit      : vconst
  | C_Int       : int -> vconst // Not exposing the full details of our integer repr.
  | C_True      : vconst
  | C_False     : vconst
  | C_String    : string -> vconst
  | C_Range     : range -> vconst
  | C_Reify     : vconst
  | C_Reflect   : name -> vconst
  (* TODO: complete *)

// This is shadowing `pattern` from Prims (for smt_pats)
noeq
type pattern =
    | Pat_Constant : vconst -> pattern              // A built-in constant
    | Pat_Cons     : fv -> list (pattern * bool) -> pattern
                                                    // A fully applied constructor, each boolean marks
                                                    // whether the argument was an explicitly-provided
                                                    // implicit argument
    | Pat_Var      : bv -> pattern                  // Pattern bound variable
    | Pat_Wild     : bv -> pattern                  // Wildcard (GM: why is this not Pat_var too?)
    | Pat_Dot_Term : bv -> term -> pattern          // Dot pattern: resolved by other elements in the pattern and type

type branch = pattern * term  // | pattern -> term

noeq
type aqualv =
    | Q_Implicit
    | Q_Explicit
    | Q_Meta of term

type argv = term * aqualv

noeq
type bv_view = {
    bv_ppname : string;
    bv_index : int;
    bv_sort : typ;
}

type universes = list universe

noeq
type universe_view =
  | Uv_Zero : universe_view
  | Uv_Succ : universe -> universe_view
  | Uv_Max  : universes -> universe_view
  | Uv_BVar : int -> universe_view
  | Uv_Name : univ_name -> universe_view
  | Uv_Unif : universe_uvar -> universe_view
  | Uv_Unk   : universe_view

noeq
type term_view =
  | Tv_Var    : v:bv -> term_view
  | Tv_BVar   : v:bv -> term_view
  | Tv_FVar   : v:fv -> term_view
  | Tv_UInst  : v:fv -> us:universes -> term_view
  | Tv_App    : hd:term -> a:argv -> term_view
  | Tv_Abs    : bv:binder -> body:term -> term_view
  | Tv_Arrow  : bv:binder -> c:comp -> term_view
  | Tv_Type   : universe -> term_view
  | Tv_Refine : bv:bv -> ref:term -> term_view
  | Tv_Const  : vconst -> term_view
  | Tv_Uvar   : int -> ctx_uvar_and_subst -> term_view
  | Tv_Let    : recf:bool -> attrs:(list term) -> bv:bv -> def:term -> body:term -> term_view
  | Tv_Match  : scrutinee:term -> ret:option match_returns_ascription -> brs:(list branch) -> term_view
  | Tv_AscribedT : e:term -> t:term -> tac:option term -> use_eq:bool -> term_view
  | Tv_AscribedC : e:term -> c:comp -> tac:option term -> use_eq:bool -> term_view
  | Tv_Unknown  : term_view // Baked in "None"

// Very basic for now
noeq
type comp_view =
  | C_Total     : ret:typ -> u:universe -> decr:(list term) -> comp_view
  | C_GTotal    : ret:typ -> u:universe -> decr:(list term) -> comp_view
  | C_Lemma     : term -> term -> term -> comp_view // pre, post, patterns
  | C_Eff       : us:universes ->
                  eff_name:name ->
                  result:term ->
                  eff_args:(list argv) ->
                  comp_view

(* Constructor for an inductive type. See explanation in
[Sg_Inductive] below. *)
type ctor = name & typ


noeq
type lb_view = {
    lb_fv : fv;
    lb_us : list univ_name;
    lb_typ : typ;
    lb_def : term
}


noeq
type sigelt_view =
  | Sg_Let :
      (r:bool) ->
      (lbs:list letbinding) ->
      sigelt_view

  // Sg_Inductive basically coalesces the Sig_bundle used internally,
  // where the type definition and its constructors are split.
  // While that might be better for typechecking, this is probably better for metaprogrammers
  // (no mutually defined types for now)
  | Sg_Inductive :
      (nm:name) ->              // name of the inductive type being defined
      (univs:list univ_name) -> // universe variables
      (params:binders) ->       // parameters
      (typ:typ) ->              // the type annotation for the inductive, i.e., indices -> Type #u
      (cts:list ctor) ->        // the constructors, opened with univs and applied to params already
      sigelt_view

  | Sg_Val :
      (nm:name) ->
      (univs:list univ_name) ->
      (typ:typ) ->
      sigelt_view

  | Unk

(* Qualifiers for sigelts, see FStar.Syntax.Syntax for an explanation. *)
noeq
type qualifier =
  | Assumption
  | New
  | Private
  | Unfold_for_unification_and_vcgen
  | Visible_default
  | Irreducible
  | Inline_for_extraction
  | NoExtract
  | Noeq
  | Unopteq
  | TotalEffect
  | Logic
  | Reifiable
  | Reflectable       of name
  | Discriminator     of name
  | Projector         of name * ident
  | RecordType        of list ident * list ident
  | RecordConstructor of list ident * list ident
  | Action            of name
  | ExceptionConstructor
  | HasMaskedEffect
  | Effect
  | OnlyName

let var : eqtype = nat

type exp : Type =
  | Unit : exp
  | Var : var -> exp
  | Mult : exp -> exp -> exp


type decls = list sigelt

(* Comparison of a term_view to term. Allows to recurse while changing the view *)
[@@ remove_unused_type_parameters [0; 1]]
let smaller (tv:term_view) (t:term) : Type0 =
    match tv with
    | Tv_App l r ->
        l << t /\ r << t /\ fst r << t

    | Tv_Abs b t'
    | Tv_Arrow b t' ->
        b << t /\ t' << t

    | Tv_Refine b t' ->
        bv << t /\ t' << t

    | Tv_Let r attrs bv t1 t2 ->
        attrs << t /\ bv << t /\ t1 << t /\ t2 << t

    | Tv_Match t1 ret_opt brs ->
        t1 << t /\ ret_opt << t /\ brs << t

    | Tv_AscribedT e ty tac _use_eq ->
        e << t /\ ty << t /\ tac << t

    | Tv_AscribedC e c tac _use_eq ->
        e << t /\ c << t /\ tac << t

    | Tv_Var v
    | Tv_BVar v ->
        v << t

    | Tv_Type _
    | Tv_Const _
    | Tv_Unknown
    | Tv_Uvar _ _
    | Tv_FVar _
    | Tv_UInst _ _ -> True

[@@ remove_unused_type_parameters [0; 1]]
let smaller_comp (cv:comp_view) (c:comp) : Type0 =
    match cv with
    | C_Total t _ md
    | C_GTotal t _ md ->
        t << c /\ md << c
    | C_Lemma pre post pats ->
        pre << c /\ post << c /\ pats << c
    | C_Eff _us eff res args ->
        res << c /\ args << c

[@@ remove_unused_type_parameters [0; 1]]
let smaller_bv (bvv:bv_view) (bv:bv) : Type0 =
    bvv.bv_sort << bv

[@@ remove_unused_type_parameters [0; 1]]
let smaller_binder (b:binder) ((bv, (q, attrs)): bv * (aqualv * list term)) : Type0 =
      bv << b /\ attrs << b
    /\ ( match q with
      | Q_Meta m -> m << b
      | _ -> True )

[@@ remove_unused_type_parameters [0; 1]]
let smaller_universe (uv:universe_view) (u:universe) : Type0 =
  match uv with
  | Uv_Succ u' -> u' << u
  | Uv_Max us -> us << u
  | Uv_Zero
  | Uv_BVar _
  | Uv_Name _
  | Uv_Unif _
  | Uv_Unk -> True

[@@ remove_unused_type_parameters [0; 1]]
let smaller_letbinding (lbv:lb_view) (lb: letbinding) : Type0 =
    lbv.lb_typ << lb /\ lbv.lb_def << lb
