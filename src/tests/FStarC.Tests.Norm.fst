(*
   Copyright 2008-2014 Nikhil Swamy and Microsoft Research

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
module FStarC.Tests.Norm
//Normalization tests

open FStarC
open FStarC.Effect
open FStarC.Syntax.Syntax
open FStarC.Tests.Pars
module S = FStarC.Syntax.Syntax
module U = FStarC.Syntax.Util
module Const = FStarC.Parser.Const
module BU = FStarC.Util
module N = FStarC.TypeChecker.Normalize
module Env = FStarC.TypeChecker.Env
open FStarC.Ident
open FStarC.Range
open FStarC.Tests.Util
open FStarC.Syntax.Subst { subst }

open FStarC.Class.Show

(* Count reductions, rather than timing them: a correct result alone cannot
   detect eagerly evaluated fields or repeated work across failed branches. *)
let demand_matching_tests () : ML unit =
  let r = dummyRange in
  let int n = U.exp_int n in
  let pat p = withinfo p r in
  let wild = pat (Pat_var (S.new_bv None S.tun)) in
  let pc n = match (int n).n with
    | Tm_constant c -> pat (Pat_constant c)
    | _ -> failwith "integer constant" in
  let ctor name = S.lid_as_fv (lid_of_path ["Test"; name] r) (Some Data_ctor) in
  let c = ctor "DemandC" in
  let d = ctor "DemandD" in
  let other = ctor "DemandOther" in
  let construct c args = S.mk_Tm_app (S.mk (Tm_fvar c) r) (List.map S.as_arg args) r in
  let pattern c args = pat (Pat_cons (c, None, List.map (fun p -> p, false) args)) in
  let mt scrutinee branches = S.mk (Tm_match {
    scrutinee; ret_opt=None; brs=branches; rc_opt=None}) r in
  let counts = List.map (fun _ -> mk_ref 0) [0; 1; 2; 3; 4] in
  let names = List.map (fun i -> lid_of_path ["Test"; "demand_probe_" ^ show i] r) [0; 1; 2; 3; 4] in
  let probe i t = app (S.fvar (List.nth names i) None) [t] in
  let primitives = List.mapi (fun i name ->
    FStarC.TypeChecker.Primops.Base.as_primitive_step_nbecbs true
      (name, 1, 0,
       (fun _ _ _ args ->
         let counter = List.nth counts i in
         counter := !counter + 1;
         match args with
         | [(t, _)] -> Some t
         | _ -> failwith "demand probe arity"),
       (fun _ _ _ -> None))) names in
  let run_env env id steps t expected expected_counts =
    List.iter (fun counter -> counter := 0) counts;
    let result = N.normalize_with_primitive_steps primitives steps env t in
    always id (term_eq result expected);
    let actual_counts = List.map (fun counter -> !counter) counts in
    if actual_counts <> expected_counts then
      failwith (Format.fmt3 "Demand test %s: reductions %s, expected %s"
        (show id) (show actual_counts) (show expected_counts))
  in
  let run id steps t expected expected_counts =
    run_env (Pars.init ()) id steps t expected expected_counts in
  List.iter (fun extra ->
    let steps = FStar.List.Tot.append extra [Env.Beta; Env.Iota; Env.Zeta; Env.Primops] in
    (* A wildcard does not even ask for the outer head. *)
    run 701 steps (mt (probe 0 (int 7)) [(wild, None, int 42)])
      (int 42) [0; 0; 0; 0; 0];
    (* All three branches inspect the same nested path. The second field
       is irrelevant, and every node on the inspected path is forced once. *)
    let s = probe 0 (construct c [probe 1 (construct d [probe 2 (int 2)]);
                                 probe 3 (int 99)]) in
    let branch n result = pattern c [pattern d [pc n]; wild], None, int result in
    run 702 steps (mt s [branch 0 10; branch 1 20; branch 2 42])
      (int 42) [1; 1; 1; 0; 0];
    (* Stop at the first failing nested test; do not inspect the sibling. *)
    run 703 steps (mt s [pattern c [pattern other []; pc 99], None, int 0;
                        wild, None, int 42])
      (int 42) [1; 1; 0; 0; 0];
    (* A false guard on a variable pattern does not force the scrutinee. *)
    run 704 steps (mt (probe 0 (int 7))
      [(wild, Some (probe 4 U.exp_false_bool), int 0); wild, None, int 42])
      (int 42) [0; 0; 0; 0; 1];
    (* A failed guard must retain the work done by its pattern. *)
    run 705 steps (mt s
      [pattern c [pattern d [pc 2]; wild], Some (probe 4 U.exp_false_bool), int 0;
       branch 2 42])
      (int 42) [1; 1; 1; 0; 1];
    (* Field binding order, including variables in nested constructors. *)
    let x = S.new_bv None S.tun in
    let y = S.new_bv None S.tun in
    let p = pattern c [pattern d [pat (Pat_var x)]; pat (Pat_var y)] in
    run 706 steps (mt s [U.branch (p, None, S.bv_to_name x)])
      (int 2) [1; 1; 1; 0; 0];
    (* Once an earlier constructor differs, a wildcard still need not force
       any of the constructor's fields. *)
    run 707 steps (mt s [pattern other [], None, int 0; wild, None, int 42])
      (int 42) [1; 0; 0; 0; 0])
    [[]; [Env.Weak]; [Env.Weak; Env.HNF]];
  (* A blocked guard retains the pattern binding in its then branch and the
     caller's environment in the fallback. Only residual branches are reduced. *)
  let x = S.new_bv None S.tun in
  let outer = S.new_bv None S.tun in
  let guard = S.new_bv None U.t_bool in
  let p = pattern c [pat (Pat_var x); wild] in
  let s = construct c [probe 0 (int 42); probe 1 (int 99)] in
  run 708 [Env.Beta; Env.Iota; Env.Zeta; Env.Primops]
    (mt s [U.branch (p, Some (S.bv_to_name guard), S.bv_to_name x);
           wild, None, S.bv_to_name outer])
    (U.if_then_else (S.bv_to_name guard) (int 42) (S.bv_to_name outer))
    [1; 0; 0; 0; 0];
  (* A guard can force a field before a subsequent pattern inspects it. *)
  let s = construct c [probe 0 U.exp_false_bool; probe 1 (int 99)] in
  run 709 [Env.Beta; Env.Iota; Env.Zeta; Env.Primops]
    (mt s [U.branch (p, Some (probe 4 (S.bv_to_name x)), int 0);
           pattern c [pat (Pat_constant (FStarC.Const.Const_bool false)); wild], None, int 42])
    (int 42) [1; 0; 0; 0; 1];
  (* Equality in a scrutinee must inspect computed fields too. Use a real
     inductive declaration so its injective arguments are known to the env. *)
  let _ = Pars.pars_and_tc_fragment
    "type demand_pair = | DemandPair : int -> int -> demand_pair | DemandPairOther" in
  let pair = ctor "DemandPair" in
  let pair_typ = S.fvar (lid_of_path ["Test"; "demand_pair"] r) None in
  let eq x y = S.mk_Tm_app (S.fvar Const.op_Eq None)
    [S.iarg pair_typ; S.as_arg x; S.as_arg y] r in
  let choose_bool b = mt b
    [pat (Pat_constant (FStarC.Const.Const_bool true)), None, int 42;
     wild, None, int 0] in
  let pair_value = construct pair [probe 0 (int 1); probe 1 (int 2)] in
  run 710 [Env.Beta; Env.Iota; Env.Zeta; Env.Primops]
    (choose_bool (eq pair_value (construct pair [int 1; int 2])))
    (int 42) [1; 1; 0; 0; 0];
  run 711 [Env.Beta; Env.Iota; Env.Zeta; Env.Primops]
    (choose_bool (eq pair_value (construct pair [int 0; probe 2 (int 3)])))
    (int 0) [1; 0; 0; 0; 0];
  run 712 [Env.Beta; Env.Iota; Env.Zeta; Env.Primops]
    (choose_bool (eq pair_value (construct (ctor "DemandPairOther") [])))
    (int 0) [0; 0; 0; 0; 0];
  (* Strictness only demands designated arguments, and only when unfolding
     is permitted. Install the attribute directly: this unit-test environment
     contains Prims, without the library declaration of the attribute. *)
  let _ = Pars.pars_and_tc_fragment
    "let demand_strict (x:int) (unused:int) = x" in
  let strict_lid = lid_of_path ["Test"; "demand_strict"] r in
  let env = Pars.init () in
  let se = Option.must (Env.lookup_sigelt env strict_lid) in
  let attr = app (S.fvar Const.strict_on_arguments_attr None)
    [FStarC.TypeChecker.Primops.Base.embed_simple r ([0] <: list Prims.int)] in
  let env = Env.push_sigelt_force env {se with sigattrs=attr::se.sigattrs} in
  let t = mt (app (S.fvar strict_lid None) [probe 0 (int 7); probe 1 (int 99)])
    [pc 7, None, int 42; wild, None, int 0] in
  let steps = [Env.Weak; Env.HNF; Env.Beta; Env.Iota; Env.Zeta; Env.Primops] in
  run_env env 713 (Env.UnfoldOnly [strict_lid]::steps) t (int 42) [1; 0; 0; 0; 0];
  run_env env 714 (Env.UnfoldOnly []::steps) t t [0; 0; 0; 0; 0];
  Format.print_string "Demand-driven matching tests passed\n"


(* A big chunk of this module is thunkued to not incur in a top-level effect. *)

let run_all () : ML unit =
  let b = mk_binder in
  let id     = pars "fun x -> x" in
  let apply  = pars "fun f x -> f x" in
  let twice  = pars "fun f x -> f (f x)" in
  let tt     = pars "fun x y -> x" in
  let ff     = pars "fun x y -> y" in
  let z      = pars "fun f x -> x" in
  let one    = pars "fun f x -> f x" in
  let two    = pars "fun f x -> f (f x)" in
  let succ   = pars "fun n f x -> f (n f x)" in
  let pred   = pars "fun n f x -> n (fun g h -> h (g f)) (fun y -> x) (fun y -> y)" in
  let mul    = pars "fun m n f -> m (n f)" in

  let rec encode n : ML term =
      if n = 0 then z
      else app succ [encode (n - 1)]
  in
  let minus m n = app n [pred; m] in
  let let_ x e e' : ML term = app (U.abs [b x] e' None) [e] in
  let mk_let x e e' : ML term =
      let e' = FStarC.Syntax.Subst.subst [NM(x, 0)] e' in
      mk (Tm_let {lbs=(false, [{lbname=Inl x; lbunivs=[]; lbtyp=tun; lbdef=e; lbeff=Const.effect_Tot_lid; lbattrs=[];lbpos=dummyRange}]); body=e'})
                            dummyRange
  in

  let lid x = lid_of_path ["Test"; x] dummyRange in
  let znat_l = S.lid_as_fv (lid "Z") (Some Data_ctor) in
  let snat_l = S.lid_as_fv (lid "S") (Some Data_ctor) in
  let tm_fv fv = mk (Tm_fvar fv) dummyRange in
  let znat : term = tm_fv znat_l in
  let snat s      = S.mk_Tm_app (tm_fv snat_l) [as_arg s] dummyRange in
  let pat p = withinfo p dummyRange in
  let snat_type = tm_fv (S.lid_as_fv (lid "snat") None) in
  let mk_match h branches =
      let branches = branches |> List.map U.branch in
      mk (Tm_match {scrutinee=h; ret_opt=None; brs=branches; rc_opt=None}) dummyRange
  in
  let pred_nat s  =
      let zbranch = pat (Pat_cons(znat_l, None, [])),
                    None,
                    znat in
      let sbranch = pat (Pat_cons(snat_l, None, [pat (Pat_var x), false])),
                    None,
                    mk (Tm_bvar({x with index=0})) dummyRange in
      mk_match s [zbranch;sbranch]
  in
  let minus_nat t1 t2 =
      let minus = m in
      let x = { x with sort = snat_type } in
      let y = { y with sort = snat_type } in
      let zbranch = pat (Pat_cons(znat_l, None, [])),
                    None,
                    nm x in
      let sbranch = pat (Pat_cons(snat_l, None, [pat (Pat_var n), false])),
                    None,
                    app (nm minus) [pred_nat (nm x); nm n] in
      let lb = {lbname=Inl minus; lbeff=lid_of_path ["Pure"] dummyRange; lbunivs=[]; lbtyp=tun;
                lbdef=subst [NM(minus, 0)] (U.abs [b x; b y] (mk_match (nm y) [zbranch; sbranch]) None);
                lbattrs=[]; lbpos=dummyRange} in
      mk (Tm_let {lbs=(true, [lb]); body= subst [NM(minus, 0)] (app (nm minus) [t1; t2])}) dummyRange
  in
  let encode_nat n : ML _ =
      let rec aux out n : ML _ =
          if n=0 then out
          else aux (snat out) (n - 1) in
      aux znat n
  in

  let default_tests =
    let _ = Pars.pars_and_tc_fragment "let rec copy (x:list int) : Tot (list int) = \
                                          match x with \
                                            | [] -> []  \
                                            | hd::tl -> hd::copy tl" in
    let _ = Pars.pars_and_tc_fragment "let recons (x:list 'a) : Tot (list 'a) = \
                                          match x with \
                                            | [] -> []  \
                                            | hd::tl -> hd::tl" in
    let _ = Pars.pars_and_tc_fragment "let rev (x:list 'a) : Tot (list 'a) = \
                                          let rec aux (x:list 'a) (out:list 'a) : Tot (list 'a) = \
                                              match x with \
                                                | [] -> out \
                                                | hd::tl -> aux tl (hd::out) in \
                                            aux x []" in
    let _ = Pars.pars_and_tc_fragment "type t = \
                                        | A : int -> int -> t \
                                        | B : int -> int -> t \
                                      let f = function \
                                        | A x y \
                                        | B y x -> y - x" in
    let _ = Pars.pars_and_tc_fragment "type snat = | Z | S : snat -> snat" in
    let _ = Pars.pars_and_tc_fragment "type tb = | T | F" in
    let _ = Pars.pars_and_tc_fragment "type rb = | A1 | A2 | A3" in
    let _ = Pars.pars_and_tc_fragment "type hb = | H : tb -> hb" in
    let _ = Pars.pars_and_tc_fragment "let select (i:tb) (x:'a) (y:'a) : Tot 'a = \
                                          match i with \
                                            | T -> x \
                                            | F -> y" in
    let _ = Pars.pars_and_tc_fragment "let select_int3 (i:int) (x:'a) (y:'a) (z:'a) : Tot 'a = \
                                          match i with \
                                            | 0 -> x \
                                            | 1 -> y \
                                            | _ -> z" in
    let _ = Pars.pars_and_tc_fragment "let select_bool (b:bool) (x:'a) (y:'a) : Tot 'a = \
                                          if b then x else y" in
    let _ = Pars.pars_and_tc_fragment "let select_string3 (s:string) (x:'a) (y:'a) (z:'a) : Tot 'a = \
                                          match s with \
                                          | \"abc\" -> x \
                                          | \"def\" -> y \
                                          | _ -> z" in
    let _ = Pars.pars_and_tc_fragment "let recons_m (x:list tb) = \
                                          match x with \
                                            | [] -> []  \
                                            | hd::tl -> hd::tl" in
    let _ = Pars.pars_and_tc_fragment "let rec copy_tb_list_2 (x:list tb) : Tot (list tb) = \
                                          match x with \
                                            | [] -> []  \
                                            | [hd] -> [hd]
                                            | hd1::hd2::tl -> hd1::hd2::copy_tb_list_2 tl" in
    let _ = Pars.pars_and_tc_fragment "let rec copy_list_2 (x:list 'a) : Tot (list 'a) = \
                                          match x with \
                                            | [] -> []  \
                                            | [hd] -> [hd]
                                            | hd1::hd2::tl -> hd1::hd2::copy_list_2 tl" in
    let _ = Pars.pars_and_tc_fragment "let (x1:int{x1>3}) = 6" in
    let _ = Pars.pars_and_tc_fragment "let (x2:int{x2+1>3 /\ not (x2-5>0)}) = 2" in
    let _ = Pars.pars_and_tc_fragment "let my_plus (x:int) (y:int) = x + y" in
    let _ = Pars.pars_and_tc_fragment "let (x3:int{forall (a:nat). a > x2}) = 7" in

    let _ = Pars.pars_and_tc_fragment "let idd (x: 'a) = x" in
    let _ = Pars.pars_and_tc_fragment "let revtb (x: tb) = match x with | T -> F | F -> T" in
    let _ = Pars.pars_and_tc_fragment "let id_tb (x: tb) = x" in
    let _ = Pars.pars_and_tc_fragment "let fst_a (x: 'a) (y: 'a) = x" in
    let _ = Pars.pars_and_tc_fragment "let id_list (x: list 'a) = x" in
    let _ = Pars.pars_and_tc_fragment "let id_list_m (x: list tb) = x" in //same as recons_m, but no pattern matching
    [ (0, (app apply [one; id; nm n]), (nm n))
    ; (1, (app id [nm x]), (nm x))
    ; (1, (app apply [tt; nm n; nm m]), (nm n))
    ; (2, (app apply [ff; nm n; nm m]), (nm m))
    ; (3, (app apply [apply; apply; apply; apply; apply; ff; nm n; nm m]), (nm m))
    ; (4, (app twice [apply; ff; nm n; nm m]), (nm m))
    ; (5, (minus one z), one)
    ; (6, (app pred [one]), z)
    ; (7, (minus one one), z)
    ; (8, (app mul [one; one]), one)
    ; (9, (app mul [two; one]), two)
    ; (10, (app mul [app succ [one]; one]), two)
    ; (11, (minus (encode 10) (encode 10)), z)
    ; (12, (minus (encode 100) (encode 100)), z)
    ; (13, (let_ x (encode 100) (minus (nm x) (nm x))), z)

    // ; (14, (let_ x (encode 1000) (minus (nm x) (nm x))), z) //takes ~10s; wasteful for CI
    ; (15, (let_ x (app succ [one])
                          (let_ y (app mul [nm x; nm x])
                          (let_ h (app mul [nm y; nm y])
                            (minus (nm h) (nm h))))), z)
    ; (16, (mk_let x (app succ [one])
                        (mk_let y (app mul [nm x; nm x])
                        (mk_let h (app mul [nm y; nm y])
                        (minus (nm h) (nm h))))), z)
    ; (17, (let_ x (app succ [one])
                        (let_ y (app mul [nm x; nm x])
                        (let_ h (app mul [nm y; nm y])
                        (minus (nm h) (nm h))))), z)
    ; (18, (pred_nat (snat (snat znat))), (snat znat))
    ; (19, tc_term (minus_nat (snat (snat znat)) (snat znat)), (snat znat)) // requires local recdef
    ; (20, tc_term (minus_nat (encode_nat 10) (encode_nat 10)), znat)
    ; (21, tc_term (minus_nat (encode_nat 100) (encode_nat 100)), znat)
    // ; (22, tc_term (minus_nat (encode_nat 10000) (encode_nat 10000)), znat) // Stack overflow in Normalizer when run with mono
    // ; (23, tc_term (minus_nat (encode_nat 1000000) (encode_nat 1000000)), znat) //this one takes about 30 sec and ~3.5GB of memory. Stack overflow in NBE when run with mono
    ; (24, (tc "recons [0;1]"), (tc "[0;1]"))
    ; (241, (tc "recons [false;true;false]"), (tc "[false;true;false]"))
    ; (25, (tc "copy [0;1]"), (tc "[0;1]"))
    ; (26, (tc "rev [0;1;2;3;4;5;6;7;8;9;10]"), (tc "[10;9;8;7;6;5;4;3;2;1;0]"))
    // Type defs not yet implemented for NBE
    // ; (271, (tc "(FStar.String.substring \"abcdef\" 1 2)"), (tc "\"bc\"")) //VD: Not sure why, but this test fails on the normalizer
    // ; (27, (tc "(rev (FStar.String.list_of_string \"abcd\"))"), (tc "['d'; 'c'; 'b'; 'a']"))// -- CH: works up to an unfolding too much (char -> char')
    ; (28, (tc "(fun x y z q -> z) T T F T"), (tc "F"))
    ; (29, (tc "[T; F]"), (tc "[T; F]"))
    ; (31, (tc "id_tb T"), (tc "T"))
    ; (32, (tc "(fun #a x -> x) #tb T"), (tc "T"))
    ; (33, (tc "revtb T"), (tc "F"))
    ; (34, (tc "(fun x y -> x) T F"), (tc "T"))
    ; (35, (tc "fst_a T F"), (tc "T"))
    ; (36, (tc "idd T"), (tc "T"))
    ; (301, (tc "id_list [T]"), (tc "[T]"))
    ; (3012, (tc "id_list_m [T]"), (tc "[T]"))
    ; (302, (tc "recons_m [T; F]"), (tc "[T; F]"))
    ; (303, (tc "select T A1 A3"), (tc "A1"))
    ; (3031, (tc "select T 3 4"), (tc "3"))
    ; (3032, (tc "select_bool false 3 4"), (tc "4"))
    ; (3033, (tc "select_int3 1 7 8 9"), (tc "8"))
    ; (3034, (tc "[5]"), (tc "[5]"))
    ; (3035, (tc "[\"abcd\"]"), (tc "[\"abcd\"]"))
    ; (3036, (tc "select_string3 \"def\" 5 6 7"), (tc "6"))
    //; (3037, (tc "['c']"), (tc "['c']")) //VD: Fails unless FStar.Char is imported (see FStarC.Tests.Pars)
    ; (305, (tc "idd T"), (tc "T"))
    ; (306, (tc "recons [T]"), (tc "[T]"))
    ; (307, (tc "copy_tb_list_2 [T;F;T;F;T;F;F]"), (tc "[T;F;T;F;T;F;F]"))
    ; (308, (tc "copy_list_2    [T;F;T;F;T;F;F]"), (tc "[T;F;T;F;T;F;F]"))

    ; (304, (tc "rev [T; F; F]"), (tc "[F; F; T]"))
    ; (305, (tc "rev [[T]; [F; T]]"), (tc "[[F; T]; [T]]"))

    ; (309, (tc "x1"), (tc "6"))
    ; (310, (tc "x2"), (tc "2"))
    //; (311, (tc "x3"), (tc "7")) // Throws parsing exceptiomn

    // Tests for primops
    ; (401, (tc "7 + 3"), (tc "10"))
    ; (402, (tc "true && false"), (tc "false"))
    ; (403, (tc "3 = 5"), (tc "false"))
    ; (404, (tc "\"abc\" ^ \"def\""), (tc "\"abcdef\""))
    ; (405, (tc "(fun (x:list int) -> match x with | [] -> 0 | hd::tl -> 1) []"), (tc "0"))

    // Test for refinements
    // ; (501, (tc "fun (x1:int{x1>(3+1)}) -> x1 + (1 + 0)"), (tc "fun (x1:int{x1>4}) -> x1 + 1")) // ZP : Fails because the two functions are not syntactically equal
    // ; (502, (tc "x1:int{x1>(3+1)}"), (tc "x1:int{x1>4}"))
    ]
  in


  let run_either i r expected (normalizer: _ -> _ -> ML _) : ML unit =
  //    force_term r;
      Format.print1 "%s: ... \n\n" (show i);
      let tcenv = Pars.init() in
      Options.parse_cmd_line() |> ignore; //set the command line args for debugging
      let x = normalizer tcenv r in
      Options.init(); //reset them
      Options.set_option "print_universes" (Options.Bool true);
      Options.set_option "print_implicits" (Options.Bool true);
      Options.set_option "ugly" (Options.Bool true);
      Options.set_option "print_bound_var_types" (Options.Bool true);
      // ignore (Options.set_options "--debug Test --debug univ_norm,NBE");
      always i (term_eq (U.unascribe x) expected)
  in

  let run_whnf i r expected =
    let open Env in
    let steps =
      [ Primops;
        Weak;
        HNF;
        UnfoldUntil delta_constant ]
    in
    run_either i r expected  (N.normalize steps)
  in

  let run_interpreter i r expected =
    run_either i r expected (N.normalize [Env.Beta; Env.UnfoldUntil delta_constant; Env.Primops])
  in
  let run_nbe i r expected =
    run_either i r expected (FStarC.TypeChecker.NBE.normalize_for_unit_test [FStarC.TypeChecker.Env.UnfoldUntil delta_constant])
  in
  let run_interpreter_with_time i r expected =
    let interp () = run_interpreter i r expected in
    (i, snd (FStarC.Util.return_execution_time interp))
  in

  let run_whnf_with_time i r expected =
    let whnf () = run_whnf i r expected in
    (i, snd (FStarC.Util.return_execution_time whnf))
  in

  let run_nbe_with_time i r expected =
    let nbe () = run_nbe i r expected in
    (i, snd (FStarC.Util.return_execution_time nbe))
  in

  let run_tests tests (run : _ -> _ -> _ -> ML 'a) : ML (list 'a) =
    let l = List.map (function (no, test, res) -> run no test res) tests in
    l
  in

  let whnf_tests =
      let _ = Pars.pars_and_tc_fragment "assume val def : Type0" in
      let _ = Pars.pars_and_tc_fragment "assume val pred : prop" in
      let _ = Pars.pars_and_tc_fragment "let def0 (y:int) = def" in
      let _ = Pars.pars_and_tc_fragment "unfold let def1 (y:int) = x:def0 y { pred }" in
      let def_def1 = tc "x:def0 17 { pred }" in
      let def_def1_unfolded = tc "x:def { pred }" in
      // let tests =  //We should expect this ... for 602
      //   [(601, tc "def1 17", def_def1);
      //    (602, def_def1, def_def1)]
      // in
      let tests =  //but the current behavior is this
                  //if we can fix the normalizer, we should change test 602
        [(601, tc "def1 17", def_def1);
        (602, def_def1, def_def1_unfolded)]
      in
      tests
  in

  let run_all_whnf () =
    Format.print_string "Testing Normlizer WHNF\n";
    let _ = run_tests whnf_tests run_whnf in
    Format.print_string "Normalizer WHNF ok\n"
  in

  let run_all_nbe () =
      Format.print_string "Testing NBE\n";
      let _ = run_tests default_tests run_nbe in
      Format.print_string "NBE ok\n"
  in

  let run_all_interpreter () =
      Format.print_string "Testing the normalizer\n";
      let _ = run_tests default_tests run_interpreter in
      Format.print_string "Normalizer ok\n"
  in

  let run_all_whnf_with_time () =
    Format.print_string "Testing WHNF\n";
    let l = run_tests whnf_tests run_whnf_with_time in
    Format.print_string "WHNF ok\n";
    l
  in

  let run_all_nbe_with_time () =
    Format.print_string "Testing NBE\n";
    let l = run_tests default_tests run_nbe_with_time in
    Format.print_string "NBE ok\n";
    l
  in

  let run_all_interpreter_with_time () =
    Format.print_string "Testing the normalizer\n";
    let l = run_tests default_tests run_interpreter_with_time in
    Format.print_string "Normalizer ok\n";
    l
  in


  // old compare
  let run_both_with_time i r expected =
    let nbe () = run_nbe i r expected in
    let norm () = run_interpreter i r expected in
    FStarC.Util.measure_execution_time "nbe" nbe;
    Format.print_string "\n";
    FStarC.Util.measure_execution_time "normalizer" norm;
    Format.print_string "\n"
  in

  let compare () =
    Format.print_string "Comparing times for normalization and nbe\n";
    run_both_with_time 14 (let_ x (encode 1000) (minus (nm x) (nm x))) z
  in

  let compare_times (l_int l_nbe : list (int & BaseTypes.float)) =
    Format.print_string "Comparing times for normalization and nbe\n";
    List.iter2 (fun res1 res2 ->
                  let (t1, time_int) = res1 in
                  let (t2, time_nbe) = res2 in
                  if (t1 = t2) // sanity check
                  then
                    Format.print3 "Test %s\nNBE %s\nInterpreter %s\n"
                      (show t1)
                      (BU.string_of_float time_nbe)
                      (BU.string_of_float time_int)
                  else
                    Format.print_string "Test numbers do not match...\n"
                ) l_int l_nbe
  in

  let run_all () =
      Format.print1 "%s" (show znat);
      let _ = run_all_whnf_with_time () in
      let l_int = run_all_interpreter_with_time () in
      let l_nbe = run_all_nbe_with_time () in
      compare_times l_int l_nbe
  in

  run_all ();
  demand_matching_tests ()
