module Deriving

open FStar.Tactics.Typeclasses
open FStar.Class.Printable
open FStar.List.Tot
open FStar.Tactics.V2

module TU = FStar.Tactics.Util

let mk_concat (sep : term) (ts : list term) : Tac term =
    mk_e_app (`String.concat) [sep; mk_list ts]

let mk_flatten ts = mk_concat (`"") ts

let paren (e : term) : Tac term =
    mk_flatten [mk_stringlit "("; e; mk_stringlit ")"]

let mk_print_bv (self : name) (f_self : term) (bvty : namedv & typ) : Tac term =
    let bv, ty = bvty in
    (* debug ("self = " ^ String.concat "." self ^ "\n>>>>>> f = : " ^ term_to_string f); *)
    let mk n = pack (Tv_FVar (pack_fv n)) in
    match inspect ty with
    | Tv_FVar fv ->
        if inspect_fv fv = self
        then mk_e_app f_self [pack (Tv_Var bv)]
        else let f = (`FStar.Class.Printable.to_string) in
             mk_e_app f [pack (Tv_Var bv)]
    | _ ->
        let f = (`FStar.Class.Printable.to_string) in
        mk_e_app f [pack (Tv_Var bv)]

let mk_printer_type (t : term) : Tac term =
    let b = fresh_binder_named "arg" t in
    let str = pack (Tv_FVar (pack_fv string_lid)) in
    let c = pack_comp (C_Total str) in
    pack (Tv_Arrow b c)

(* This tactics generates the entire let rec at once and
 * then uses exact. We could do something better. *)
let mk_printer_fun (dom : term) : Tac term =
    set_guard_policy SMT;
    let e = top_env () in
    (* Recursive binding *)
    let ff = fresh_namedv_named "ff_rec" in
    let ffty = mk_printer_type dom in
    let fftm = pack (Tv_Var ff) in

    let x = fresh_binder_named "v" dom in
    let xt_ns = match inspect dom with
                | Tv_FVar fv -> (inspect_fv fv)
                | _ -> fail "not a qname type?"
    in
    let se = match lookup_typ e xt_ns with
             | None -> fail "Type not found..?"
             | Some se -> se
    in

    match inspect_sigelt se with
    | Sg_Let _ -> fail "cannot create printer for let"
    | Sg_Inductive {params=bs; typ=y; ctors} ->
        let br1 ctor : Tac branch =
            let (name, t) = ctor in
            let pn = String.concat "." name in
            let t_args, _ = collect_arr t in
            let bv_ty_pats = TU.map (fun ti -> let bv = fresh_namedv_named "a" in ((bv, ti), (Pat_Var {v=bv; sort=seal ti}, false))) t_args in
            let bvs, pats = List.Tot.split bv_ty_pats in
            let head = pack (Tv_Const (C_String pn)) in
            let bod = mk_concat (mk_stringlit " ") (head :: TU.map (mk_print_bv xt_ns fftm) bvs) in
            let bod = match t_args with | [] -> bod | _ -> paren bod in
            (Pat_Cons {head=pack_fv name; univs=None; subpats=pats}, bod)
        in
        let branches = TU.map br1 ctors in
        let xi = fresh_binder_named "v_inner" dom in

        // Generate the match on the internal argument
        let m = pack (Tv_Match (pack (Tv_Var (binder_to_namedv xi))) None branches) in
        (* debug ("m = " ^ term_to_string m); *)

        // Wrap it into an internal function
        let f = pack (Tv_Abs xi m) in
        (* debug ("f = " ^ term_to_string f); *)

        // Wrap it in a let rec; basically:
        // let rec ff = fun t -> match t with { .... } in ff x
        let ff_bnd : binder = { namedv_to_simple_binder ff with sort = ffty } in
        let xtm = pack (Tv_Var (binder_to_namedv x)) in
        let b = pack (Tv_Let true [] ff_bnd f (mk_e_app fftm [xtm])) in
        (* print ("b = " ^ term_to_string b); *)

        // Wrap it in a lambda taking the initial argument
        let tm = pack (Tv_Abs x b) in
        (* debug ("tm = " ^ term_to_string tm); *)

        tm
    | _ -> fail "type not found?"

let rec maplast (f : 'a -> 'a) (l : list 'a) : list 'a =
    match l with
    | [] -> []
    | [x] -> [f x]
    | x::xs -> x :: (maplast f xs)

let mk_printer (dom : term) : Tac (decls & term) =
    let nm = match inspect dom with
             | Tv_FVar fv -> inspect_fv fv
             | _ -> fail "not an fv?"
    in
    let nm = maplast (fun s -> s ^ "_print") nm in
    let lb = {
      lb_fv = pack_fv nm;
      lb_us = [];
      lb_typ = mk_printer_type dom;
      lb_def = mk_printer_fun dom
    } in
    let sv : sigelt_view = Sg_Let {isrec=false; lbs=[lb]} in
    let ses : list sigelt = [pack_sigelt sv] in
    ses, pack (Tv_FVar (pack_fv nm))

let mk_printable_instance (dom : term) (printer_f : term) : Tac decls =
  let nm = match inspect dom with
           | Tv_FVar fv -> inspect_fv fv
           | _ -> fail "not an fv?"
  in
  let nm = maplast (fun s -> "printable_" ^ s) nm in
  let lb = {
    lb_fv = pack_fv nm;
    lb_us = [];
    lb_typ = (`FStar.Class.Printable.printable (`#dom));
    lb_def = (`FStar.Class.Printable.Mkprintable (`#printer_f));
  } in
  let sv : sigelt_view = Sg_Let {isrec=false; lbs=[lb]} in
  let se = pack_sigelt sv in
  let se = set_sigelt_attrs [`tcinstance] se in
  let ses : list sigelt = [se] in
  ses

let derive_printable (dom : term) : Tac decls =
  let printer, printer_f = mk_printer dom in
  let inst = mk_printable_instance dom printer_f in
  let r = printer @ inst in
  dump (term_to_string (quote r));
  r

noeq
type t1 =
    | A : int -> string -> t1
    | B : t1 -> int -> t1
    | C : t1
    | D : string -> t1
    | E : t1 -> t1

(* We need to provide the name of the generated definition
 * by hand, since desugaring this module occurs entirely
 * before running the metaprograms. *)
%splice[] (derive_printable (`t1))

let _ = assert_norm (to_string (A 5 "hey") == "(Deriving.A 5 \"hey\")")
let _ = assert_norm (to_string (B (D "thing") 42) = "(Deriving.B (Deriving.D \"thing\") 42)")
let _ = assert_norm (to_string C = "Deriving.C")
let _ = assert_norm (to_string (D "test") = "(Deriving.D \"test\")")
let _ = assert_norm (to_string (E (B (D "thing") 42)) = "(Deriving.E (Deriving.B (Deriving.D \"thing\") 42))")
// let _ = assert_norm (to_string (F (fun _ -> C)) = "(Deriving.F ?)")

noeq
type t2 =
  | G of int
  | H of t1

%splice[] (derive_printable (`t2))

let _ = assert_norm (to_string (G 42) = "(Deriving.G 42)")
let _ = assert_norm (to_string (H (D "asdf")) = "(Deriving.H (Deriving.D \"asdf\"))")

noeq
type t3 =
  | Z of (int -> int)

(* Fails: no instance to print (int -> int ) *)
[@@expect_failure]
%splice[] (derive_printable (`t3))
