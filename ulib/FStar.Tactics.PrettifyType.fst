module FStar.Tactics.PrettifyType

(* The single thing you should call here is entry function,
as the body of a splice. We could make this a plugin eventually,
not doing it now to now complicate the build (and this is pretty
fast anyway). *)

//#set-options "--print_implicits --print_full_names --print_universes"

open FStar.Tactics.V2.Bare
open FStar.List.Tot { (@), unsnoc }

(* public *)
unfold
let f_inverses #a #b (f : b -> a) (g : a -> b) (x:a) =
  squash (f (g x) == x)

private
let add_suffix (s:string) (nm:name) : name =
  explode_qn (implode_qn nm ^ s)

private
let add_prefix (s:string) (nm:name) : name =
  assume (List.length nm > 0);
  let first, last = unsnoc nm in
  first @ [s ^ last]

private
type atom = term

private
noeq
type parsed_type =
  | Atom of atom
  | Tuple2 of parsed_type * parsed_type
  | Either of parsed_type * parsed_type

private
let rec parsed_type_to_string (t:parsed_type) : Tac string =
  match t with
  | Atom t -> term_to_string t
  | Tuple2 (a, b) ->
    "(" ^ parsed_type_to_string a ^ ", " ^ parsed_type_to_string b ^ ")"
  | Either (a, b) ->
    "(" ^ parsed_type_to_string a ^ " + " ^ parsed_type_to_string b ^ ")"

private
let rec parse_prod_type (t:term) : Tac parsed_type =
  let hd, args = collect_app t in
  match inspect hd, args with
  | Tv_UInst fv _, [(a, Q_Explicit); (b, Q_Explicit)]
  | Tv_FVar fv, [(a, Q_Explicit); (b, Q_Explicit)] ->
    if inspect_fv fv = explode_qn (`%tuple2) then
      Tuple2 (parse_prod_type a, parse_prod_type b)
    else
      Atom t
  | _ ->
    Atom t

private
let rec parse_sum_type (t:term) : Tac parsed_type =
  let hd, args = collect_app t in
  match inspect hd, args with
  | Tv_UInst fv _, [(a, Q_Explicit); (b, Q_Explicit)]
  | Tv_FVar fv, [(a, Q_Explicit); (b, Q_Explicit)] ->
    if inspect_fv fv = explode_qn (`%either) then
      Either (parse_sum_type a, parse_sum_type b)
    else
      parse_prod_type t
  | _ ->
    parse_prod_type t

private
let parse_type = parse_sum_type

private
noeq
type prod_type = 
  | Prod : list atom -> prod_type

private
noeq
type flat_type = 
  | Sum : list prod_type -> flat_type

private
let prod_type_to_string (t:prod_type) : Tac string =
  match t with
  | Prod ts ->
    let ts = ts |> Tactics.Util.map term_to_string in
    "(" ^ String.concat ", " ts ^ ")"

private
let flat_type_to_string (t:flat_type) : Tac string =
  match t with
  | Sum ts ->
    let ts = ts |> Tactics.Util.map prod_type_to_string in
    "(" ^ String.concat " + " ts ^ ")"

private
let rec as_prod_type (t : parsed_type) : Tac prod_type =
  match t with
  | Tuple2 (a, b) ->
    let Prod aa = as_prod_type a in
    let Prod bb = as_prod_type b in
    Prod (aa @ bb)
  | Atom t -> Prod [t]
  | Either _ -> fail "as_prod_type: not a product type"

private
let rec as_flat_type (t:parsed_type) : Tac flat_type =
  match t with
  | Either (a, b) ->
    let Sum aa = as_flat_type a in
    let Sum bb = as_flat_type b in
    Sum (aa @ bb)
  | _ -> Sum [as_prod_type t]

// let unitv_                 : term = `()
// let unitt_                 : term = `(unit)
// let empty_                 : term = `(empty)
// let either_ (a b : term)   : term = `(either (`#a) (`#b))
// let tuple2_ (a b : term)   : term = `(tuple2 (`#a) (`#b))
// let mktuple2_ (a b : term) : term = `(Mktuple2 (`#a) (`#b))

private
let get_typ_def (nm : name) : Tac term =
  let e = top_env () in
  let se = lookup_typ e nm in
  match se with
  | None -> fail "ctors_of_typ: type not found"
  | Some se -> (
    let sev = inspect_sigelt se in
    match sev with
    | Sg_Let {lbs=[lb]} -> lb.lb_def
    | _ ->
      fail "get_typ_def: not a let binding?"
  )

private
let mk_ctor (tynm : name) (i : nat) (fat : prod_type) : Tac ctor =
  let Prod fields = fat in
  let bs = fields |> Tactics.Util.map
    (function f -> fresh_binder f <: binder)
  in
  let nm = add_prefix "Mk" tynm |> add_suffix (string_of_int i) in
  let ty = mk_tot_arr bs (pack (Tv_FVar (pack_fv tynm))) in
  nm, ty

(* Returns a singleton list with the definition of the fancy type. And the constructors
to be used by other calls. *)
private
let mk_fancy_type (nm pretty_nm :name ) (fat : flat_type) : Tac (list ctor & decls) =
  let Sum cases = fat in
  let ctors = cases |> Tactics.Util.mapi (mk_ctor pretty_nm) in
  let sv = Sg_Inductive {
   nm = pretty_nm;
   univs = [];
   params = [];
   typ = (`Type0);
   ctors = ctors;
  } in
  let se = pack_sigelt sv in
  ctors, [se]

private
let rec parsed_type_pat (at : parsed_type) : Tac (pattern & binders) =
  match at with
  | Atom t ->
    let b = fresh_binder t <: binder in
    Pat_Var { v=b; sort = Sealed.seal (`_) }, [b]
  | Tuple2 (a, b) ->
    let p1, bs1 = parsed_type_pat a in
    let p2, bs2 = parsed_type_pat b in
    let mktuple2 = pack_fv (explode_qn (`%Mktuple2)) in
    let p =
      Pat_Cons {
        head = mktuple2;
        univs = None;
        subpats = [(p1, false); (p2, false)];
      }
    in
    p, bs1 @ bs2
  | _ ->
    fail "should not happen: parsed_type_pat: not a product type"

private
let rec parsed_type_expr (at : parsed_type) (bs : binders) : Tac (term & binders) =
  // print ("parsed_type_expr of " ^ parsed_type_to_string at
  //   ^ " with len bs: " ^  string_of_int (List.length bs));
  match at with
  | Atom t ->
    guard (not (Nil? bs));
    let b::bs = bs in
    pack (Tv_Var b), bs
  | Tuple2 (a, b) ->
    let e1, bs = parsed_type_expr a bs in
    let e2, bs = parsed_type_expr b bs in
    let mktuple2 = pack_fv (explode_qn (`%Mktuple2)) in
    let e : term = mk_e_app (Tv_FVar mktuple2) [e1; e2] in
    e, bs
  | _ ->
    fail "should not happen: parsed_type_pat: not a product type"

private
let mk_right_case (tynm pretty_tynm : name) (i : nat) (at : parsed_type) : Tac branch =
  let p, bs = parsed_type_pat at in
  let ctor_nm = add_prefix "Mk" pretty_tynm |> add_suffix (string_of_int i) in
  let body = pack (Tv_FVar (pack_fv ctor_nm)) in
  let body = mk_e_app body (Tactics.Util.map (fun (b:binder) -> pack (Tv_Var b)) bs) in
  p, body

private
let rec mk_right_body (tynm pretty_tynm : name) (at : parsed_type) (i : nat) (sc : term) : Tac (nat & term) =
  match at with
  | Either (l, r) ->
    let v1 = fresh_binder (`_) in
    let v2 = fresh_binder (`_) in
    let pat_inl = Pat_Cons {
      head = pack_fv (explode_qn (`%Inl));
      univs = None;
      subpats = [(Pat_Var {v=v1; sort=Sealed.seal (`_)}, false)];
    } in
    let pat_inr = Pat_Cons {
      head = pack_fv (explode_qn (`%Inr));
      univs = None;
      subpats = [(Pat_Var {v=v2; sort=Sealed.seal (`_)}, false)];
    } in
    let i, body1 = mk_right_body tynm pretty_tynm l i (pack (Tv_Var v1)) in
    let br1 = pat_inl, body1 in
    let i, body2 = mk_right_body tynm pretty_tynm r i (pack (Tv_Var v2)) in
    let br2 = pat_inr, body2 in
    let brs = [br1; br2] in
    i, pack (Tv_Match sc None brs)
  | _ ->
    (* Single case match. *)
    let branch = mk_right_case tynm pretty_tynm i at in
    i+1, pack (Tv_Match sc None [branch])

private
let mk_right (tynm pretty_tynm : name) (at : parsed_type) (fat : flat_type) : Tac decls =
  let b = fresh_binder (pack (Tv_FVar (pack_fv tynm))) in
  let sv = Sg_Let {
    isrec = false;
    lbs = [
      {
        lb_fv = pack_fv (add_suffix "_right" pretty_tynm);
        lb_us = [];
        lb_typ = mk_tot_arr [b]
                            (pack (Tv_FVar (pack_fv pretty_tynm)));
        lb_def = mk_abs [b] (snd <| mk_right_body tynm pretty_tynm at 0 (pack (Tv_Var b)));
      }
    ]
  }
  in
  [pack_sigelt sv]

private
let mk_left_case (tynm pretty_tynm : name) (i : nat) (at : parsed_type) : Tac branch =
  let p, bs = parsed_type_pat at in
  let ctor_nm = add_prefix "Mk" pretty_tynm |> add_suffix (string_of_int i) in
  let body = pack (Tv_FVar (pack_fv ctor_nm)) in
  let body = mk_e_app body (Tactics.Util.map (fun (b:binder) -> pack (Tv_Var b)) bs) in
  p, body

private
let rec mk_left_branches (ff : term -> Tac term) (tynm pretty_tynm : name) (at : parsed_type) (ctors : list ctor) : Tac (list ctor & list branch) =
  match at with
  | Either (l, r) ->
    let inl (t:term) : term = mk_e_app (Tv_FVar (pack_fv (explode_qn (`%Inl)))) [t] in
    let inr (t:term) : term = mk_e_app (Tv_FVar (pack_fv (explode_qn (`%Inr)))) [t] in
    let ctors, brs1 = mk_left_branches (fun t -> ff (inl t)) tynm pretty_tynm l ctors in
    let ctors, brs2 = mk_left_branches (fun t -> ff (inr t)) tynm pretty_tynm r ctors in
    ctors, brs1 @ brs2
  | _ ->
    guard (not (Nil? ctors));
    let (c_nm, c_ty)::ctors = ctors in

    let bs, _ = collect_arr c_ty in
    let bs = bs |> Tactics.Util.map (fun b -> fresh_binder b <: binder) in
    let p = Pat_Cons {
      head = pack_fv c_nm;
      univs = None;
      subpats = Tactics.Util.map (fun (b:binder) -> Pat_Var {v=b; sort=Sealed.seal (`_)}, false) bs;
    } in
    let body, rest_bs = parsed_type_expr at bs in
    let body = ff body in
    guard (Nil? rest_bs);
    (* Single case match. *)
    ctors, [(p ,body)]

private
let mk_left_body (tynm pretty_tynm : name) (at : parsed_type) (ctors : list ctor) (sc : term) : Tac term =
  let ctors, brs = mk_left_branches (fun t -> t) tynm pretty_tynm at ctors in
  guard (Nil? ctors);
  pack (Tv_Match sc None brs)

private
let mk_left ctors (tynm pretty_tynm : name) (at : parsed_type) (fat : flat_type) : Tac decls =
  let b = fresh_binder (pack (Tv_FVar (pack_fv pretty_tynm))) in
  let sv = Sg_Let {
    isrec = false;
    lbs = [
      {
        lb_fv = pack_fv (add_suffix "_left" pretty_tynm);
        lb_us = [];
        lb_typ = mk_tot_arr [fresh_binder (pack (Tv_FVar (pack_fv pretty_tynm)))]
                            (pack (Tv_FVar (pack_fv tynm)));
        lb_def = mk_abs [b] (mk_left_body tynm pretty_tynm at ctors (Tv_Var b));
      }
    ]
  }
  in
  [pack_sigelt sv]

private
let mk_left_right_case (tynm pretty_tynm : name) (i : nat) (at : parsed_type) : Tac branch =
  let p, _ = parsed_type_pat at in
  p, (`())

private
let rec mk_left_right_body (tynm pretty_tynm : name) (at : parsed_type) (i : nat) (sc : term) : Tac (nat & term) =
  match at with
  | Either (l, r) ->
    let v1 = fresh_binder (`_) in
    let v2 = fresh_binder (`_) in
    let pat_inl = Pat_Cons {
      head = pack_fv (explode_qn (`%Inl));
      univs = None;
      subpats = [(Pat_Var {v=v1; sort=Sealed.seal (`_)}, false)];
    } in
    let pat_inr = Pat_Cons {
      head = pack_fv (explode_qn (`%Inr));
      univs = None;
      subpats = [(Pat_Var {v=v2; sort=Sealed.seal (`_)}, false)];
    } in
    let i, body1 = mk_left_right_body tynm pretty_tynm l i (pack (Tv_Var v1)) in
    let br1 = pat_inl, body1 in
    let i, body2 = mk_left_right_body tynm pretty_tynm r i (pack (Tv_Var v2)) in
    let br2 = pat_inr, body2 in
    let brs = [br1; br2] in
    i, pack (Tv_Match sc None brs)
  | _ ->
    (* Single case match. *)
    let branch = mk_left_right_case tynm pretty_tynm i at in
    i+1, pack (Tv_Match sc None [branch])

private
let mk_left_right (tynm pretty_tynm : name) (at : parsed_type) : Tac decls =
  let b = fresh_binder (pack (Tv_FVar (pack_fv tynm))) in
  let tm_left  : term = Tv_FVar <| pack_fv (add_suffix "_left" pretty_tynm) in
  let tm_right : term = Tv_FVar <| pack_fv (add_suffix "_right" pretty_tynm) in
  let sv = Sg_Let {
    isrec = false;
    lbs = [
      {
        lb_fv = pack_fv (add_suffix "_left_right" pretty_tynm);
        lb_us = [];
        lb_typ =
          mk_tot_arr
            [b]
            (`(f_inverses (`#tm_left) (`#tm_right) (`#b)));
        lb_def = mk_abs [b] (snd <| mk_left_right_body tynm pretty_tynm at 0 (pack (Tv_Var b)));
      }
    ]
  }
  in
  [pack_sigelt sv]

private
let rec mk_right_left_branches (tynm pretty_tynm : name) (at : parsed_type) (ctors : list ctor) : Tac (list ctor & list branch) =
  match at with
  | Either (l, r) ->
    let inl (t:term) : term = mk_e_app (Tv_FVar (pack_fv (explode_qn (`%Inl)))) [t] in
    let inr (t:term) : term = mk_e_app (Tv_FVar (pack_fv (explode_qn (`%Inr)))) [t] in
    let ctors, brs1 = mk_right_left_branches tynm pretty_tynm l ctors in
    let ctors, brs2 = mk_right_left_branches tynm pretty_tynm r ctors in
    ctors, brs1 @ brs2
  | _ ->
    guard (not (Nil? ctors));
    let (c_nm, c_ty)::ctors = ctors in

    let bs, _ = collect_arr c_ty in
    let bs = bs |> Tactics.Util.map (fun b -> fresh_binder b <: binder) in
    let p = Pat_Cons {
      head = pack_fv c_nm;
      univs = None;
      subpats = Tactics.Util.map (fun (b:binder) -> Pat_Var {v=b; sort=Sealed.seal (`_)}, false) bs;
    } in
    let body = (`()) in
    // let body = ff body in
    // guard (Nil? rest_bs);
    (* Single case match. *)
    ctors, [(p ,body)]

private
let mk_right_left_body (tynm pretty_tynm : name) (at : parsed_type) (ctors : list ctor) (sc : term) : Tac term =
  let ctors, brs = mk_right_left_branches tynm pretty_tynm at ctors in
  guard (Nil? ctors);
  pack (Tv_Match sc None brs)

private
let mk_right_left (tynm pretty_tynm : name) (at : parsed_type) (ctors : list ctor) : Tac decls =
  let b = fresh_binder (pack (Tv_FVar (pack_fv pretty_tynm))) in
  let tm_left  : term = Tv_FVar <| pack_fv (add_suffix "_left" pretty_tynm) in
  let tm_right : term = Tv_FVar <| pack_fv (add_suffix "_right" pretty_tynm) in
  let bt : term = b in
  let sv = Sg_Let {
    isrec = false;
    lbs = [
      {
        lb_fv = pack_fv (add_suffix "_right_left" pretty_tynm);
        lb_us = [];
        lb_typ =
          mk_tot_arr
            [b]
            (`(f_inverses (`#tm_right) (`#tm_left) (`#bt)));
        lb_def = mk_abs [b] (mk_right_left_body tynm pretty_tynm at ctors (Tv_Var b));
      }
    ]
  }
  in
  [pack_sigelt sv]


// [@@plugin]
let entry (suf nm : string) : Tac decls =
  // print ("ENTRY, n quals = " ^ string_of_int (List.length (splice_quals ())));
  // print ("ENTRY, n attrs = " ^ string_of_int (List.length (splice_attrs ())));
  let quals = splice_quals () in
  let attrs = splice_attrs () in

  let nm = explode_qn nm in
  let def = get_typ_def nm in
  // print ("def: " ^ term_to_string def);
  let at = parse_type def in
  // print ("at: " ^ parsed_type_to_string at);
  let fat = as_flat_type at in
  // print ("fat: " ^ flat_type_to_string fat);
  let tynm = add_suffix suf nm in
  let ctors, tds = mk_fancy_type nm tynm fat in
  let ds = mk_right nm tynm at fat in
  let ds = ds @ mk_left  ctors nm tynm at fat in
  let ds = ds @ mk_left_right nm tynm at in
  let ds = ds @ mk_right_left nm tynm at ctors in
  let post_type (se : sigelt) : Tac sigelt =
    let quals = filter (fun q -> not (Unfold_for_unification_and_vcgen? q)) quals in
    let se = set_sigelt_quals quals se in
    let se = set_sigelt_attrs attrs se in
    se
  in
  let post_other (se : sigelt) : Tac sigelt =
    let quals = filter (fun q -> not (Noeq? q || Unopteq? q)) quals in
    let se = set_sigelt_quals quals se in
    let se = set_sigelt_attrs attrs se in
    se
  in
  map post_type tds @ map post_other ds
