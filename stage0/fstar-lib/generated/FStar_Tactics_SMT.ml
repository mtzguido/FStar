open Prims
let (smt_sync : unit -> (unit, unit) FStar_Tactics_Effect.tac_repr) =
  fun uu___ ->
    let uu___1 = FStar_Tactics_V2_Builtins.get_vconfig () in
    FStar_Tactics_Effect.tac_bind
      (FStar_Sealed.seal
         (Obj.magic
            (FStar_Range.mk_range "FStar.Tactics.SMT.fst" (Prims.of_int (8))
               (Prims.of_int (40)) (Prims.of_int (8)) (Prims.of_int (56)))))
      (FStar_Sealed.seal
         (Obj.magic
            (FStar_Range.mk_range "FStar.Tactics.SMT.fst" (Prims.of_int (8))
               (Prims.of_int (29)) (Prims.of_int (8)) (Prims.of_int (56)))))
      (Obj.magic uu___1)
      (fun uu___2 ->
         (fun uu___2 ->
            Obj.magic (FStar_Tactics_V2_Builtins.t_smt_sync uu___2)) uu___2)
let _ =
  FStar_Tactics_Native.register_tactic "FStar.Tactics.SMT.smt_sync"
    (Prims.of_int (2))
    (fun psc ->
       fun ncb ->
         fun us ->
           fun args ->
             FStar_Tactics_InterpFuns.mk_tactic_interpretation_1
               "FStar.Tactics.SMT.smt_sync (plugin)"
               (FStar_Tactics_Native.from_tactic_1 smt_sync)
               FStar_Syntax_Embeddings.e_unit FStar_Syntax_Embeddings.e_unit
               psc ncb us args)
let (smt_sync' :
  Prims.nat -> Prims.nat -> (unit, unit) FStar_Tactics_Effect.tac_repr) =
  fun fuel ->
    fun ifuel ->
      let uu___ = FStar_Tactics_V2_Builtins.get_vconfig () in
      FStar_Tactics_Effect.tac_bind
        (FStar_Sealed.seal
           (Obj.magic
              (FStar_Range.mk_range "FStar.Tactics.SMT.fst"
                 (Prims.of_int (12)) (Prims.of_int (15)) (Prims.of_int (12))
                 (Prims.of_int (29)))))
        (FStar_Sealed.seal
           (Obj.magic
              (FStar_Range.mk_range "FStar.Tactics.SMT.fst"
                 (Prims.of_int (12)) (Prims.of_int (32)) (Prims.of_int (16))
                 (Prims.of_int (20))))) (Obj.magic uu___)
        (fun uu___1 ->
           (fun vcfg ->
              let uu___1 =
                Obj.magic
                  (FStar_Tactics_Effect.lift_div_tac
                     (fun uu___2 ->
                        {
                          FStar_VConfig.initial_fuel = fuel;
                          FStar_VConfig.max_fuel = fuel;
                          FStar_VConfig.initial_ifuel = ifuel;
                          FStar_VConfig.max_ifuel = ifuel;
                          FStar_VConfig.detail_errors =
                            (vcfg.FStar_VConfig.detail_errors);
                          FStar_VConfig.detail_hint_replay =
                            (vcfg.FStar_VConfig.detail_hint_replay);
                          FStar_VConfig.no_smt = (vcfg.FStar_VConfig.no_smt);
                          FStar_VConfig.quake_lo =
                            (vcfg.FStar_VConfig.quake_lo);
                          FStar_VConfig.quake_hi =
                            (vcfg.FStar_VConfig.quake_hi);
                          FStar_VConfig.quake_keep =
                            (vcfg.FStar_VConfig.quake_keep);
                          FStar_VConfig.retry = (vcfg.FStar_VConfig.retry);
                          FStar_VConfig.smtencoding_elim_box =
                            (vcfg.FStar_VConfig.smtencoding_elim_box);
                          FStar_VConfig.smtencoding_nl_arith_repr =
                            (vcfg.FStar_VConfig.smtencoding_nl_arith_repr);
                          FStar_VConfig.smtencoding_l_arith_repr =
                            (vcfg.FStar_VConfig.smtencoding_l_arith_repr);
                          FStar_VConfig.smtencoding_valid_intro =
                            (vcfg.FStar_VConfig.smtencoding_valid_intro);
                          FStar_VConfig.smtencoding_valid_elim =
                            (vcfg.FStar_VConfig.smtencoding_valid_elim);
                          FStar_VConfig.tcnorm = (vcfg.FStar_VConfig.tcnorm);
                          FStar_VConfig.no_plugins =
                            (vcfg.FStar_VConfig.no_plugins);
                          FStar_VConfig.no_tactics =
                            (vcfg.FStar_VConfig.no_tactics);
                          FStar_VConfig.z3cliopt =
                            (vcfg.FStar_VConfig.z3cliopt);
                          FStar_VConfig.z3smtopt =
                            (vcfg.FStar_VConfig.z3smtopt);
                          FStar_VConfig.z3refresh =
                            (vcfg.FStar_VConfig.z3refresh);
                          FStar_VConfig.z3rlimit =
                            (vcfg.FStar_VConfig.z3rlimit);
                          FStar_VConfig.z3rlimit_factor =
                            (vcfg.FStar_VConfig.z3rlimit_factor);
                          FStar_VConfig.z3seed = (vcfg.FStar_VConfig.z3seed);
                          FStar_VConfig.z3version =
                            (vcfg.FStar_VConfig.z3version);
                          FStar_VConfig.trivial_pre_for_unannotated_effectful_fns
                            =
                            (vcfg.FStar_VConfig.trivial_pre_for_unannotated_effectful_fns);
                          FStar_VConfig.reuse_hint_for =
                            (vcfg.FStar_VConfig.reuse_hint_for)
                        })) in
              Obj.magic
                (FStar_Tactics_Effect.tac_bind
                   (FStar_Sealed.seal
                      (Obj.magic
                         (FStar_Range.mk_range "FStar.Tactics.SMT.fst"
                            (Prims.of_int (13)) (Prims.of_int (18))
                            (Prims.of_int (14)) (Prims.of_int (68)))))
                   (FStar_Sealed.seal
                      (Obj.magic
                         (FStar_Range.mk_range "FStar.Tactics.SMT.fst"
                            (Prims.of_int (16)) (Prims.of_int (4))
                            (Prims.of_int (16)) (Prims.of_int (20)))))
                   (Obj.magic uu___1)
                   (fun uu___2 ->
                      (fun vcfg' ->
                         Obj.magic
                           (FStar_Tactics_V2_Builtins.t_smt_sync vcfg'))
                        uu___2))) uu___1)
let _ =
  FStar_Tactics_Native.register_tactic "FStar.Tactics.SMT.smt_sync'"
    (Prims.of_int (3))
    (fun psc ->
       fun ncb ->
         fun us ->
           fun args ->
             FStar_Tactics_InterpFuns.mk_tactic_interpretation_2
               "FStar.Tactics.SMT.smt_sync' (plugin)"
               (FStar_Tactics_Native.from_tactic_2 smt_sync')
               FStar_Syntax_Embeddings.e_int FStar_Syntax_Embeddings.e_int
               FStar_Syntax_Embeddings.e_unit psc ncb us args)
let (get_rlimit : unit -> (Prims.int, unit) FStar_Tactics_Effect.tac_repr) =
  fun uu___ ->
    let uu___1 = FStar_Tactics_V2_Builtins.get_vconfig () in
    FStar_Tactics_Effect.tac_bind
      (FStar_Sealed.seal
         (Obj.magic
            (FStar_Range.mk_range "FStar.Tactics.SMT.fst" (Prims.of_int (20))
               (Prims.of_int (45)) (Prims.of_int (20)) (Prims.of_int (60)))))
      (FStar_Sealed.seal
         (Obj.magic
            (FStar_Range.mk_range "FStar.Tactics.SMT.fst" (Prims.of_int (20))
               (Prims.of_int (45)) (Prims.of_int (20)) (Prims.of_int (69)))))
      (Obj.magic uu___1)
      (fun uu___2 ->
         FStar_Tactics_Effect.lift_div_tac
           (fun uu___3 -> uu___2.FStar_VConfig.z3rlimit))
let _ =
  FStar_Tactics_Native.register_tactic "FStar.Tactics.SMT.get_rlimit"
    (Prims.of_int (2))
    (fun psc ->
       fun ncb ->
         fun us ->
           fun args ->
             FStar_Tactics_InterpFuns.mk_tactic_interpretation_1
               "FStar.Tactics.SMT.get_rlimit (plugin)"
               (FStar_Tactics_Native.from_tactic_1 get_rlimit)
               FStar_Syntax_Embeddings.e_unit FStar_Syntax_Embeddings.e_int
               psc ncb us args)
let (set_rlimit : Prims.int -> (unit, unit) FStar_Tactics_Effect.tac_repr) =
  fun v ->
    let uu___ =
      let uu___1 = FStar_Tactics_V2_Builtins.get_vconfig () in
      FStar_Tactics_Effect.tac_bind
        (FStar_Sealed.seal
           (Obj.magic
              (FStar_Range.mk_range "FStar.Tactics.SMT.fst"
                 (Prims.of_int (21)) (Prims.of_int (59)) (Prims.of_int (21))
                 (Prims.of_int (73)))))
        (FStar_Sealed.seal
           (Obj.magic
              (FStar_Range.mk_range "FStar.Tactics.SMT.fst"
                 (Prims.of_int (21)) (Prims.of_int (59)) (Prims.of_int (21))
                 (Prims.of_int (91))))) (Obj.magic uu___1)
        (fun uu___2 ->
           FStar_Tactics_Effect.lift_div_tac
             (fun uu___3 ->
                {
                  FStar_VConfig.initial_fuel =
                    (uu___2.FStar_VConfig.initial_fuel);
                  FStar_VConfig.max_fuel = (uu___2.FStar_VConfig.max_fuel);
                  FStar_VConfig.initial_ifuel =
                    (uu___2.FStar_VConfig.initial_ifuel);
                  FStar_VConfig.max_ifuel = (uu___2.FStar_VConfig.max_ifuel);
                  FStar_VConfig.detail_errors =
                    (uu___2.FStar_VConfig.detail_errors);
                  FStar_VConfig.detail_hint_replay =
                    (uu___2.FStar_VConfig.detail_hint_replay);
                  FStar_VConfig.no_smt = (uu___2.FStar_VConfig.no_smt);
                  FStar_VConfig.quake_lo = (uu___2.FStar_VConfig.quake_lo);
                  FStar_VConfig.quake_hi = (uu___2.FStar_VConfig.quake_hi);
                  FStar_VConfig.quake_keep =
                    (uu___2.FStar_VConfig.quake_keep);
                  FStar_VConfig.retry = (uu___2.FStar_VConfig.retry);
                  FStar_VConfig.smtencoding_elim_box =
                    (uu___2.FStar_VConfig.smtencoding_elim_box);
                  FStar_VConfig.smtencoding_nl_arith_repr =
                    (uu___2.FStar_VConfig.smtencoding_nl_arith_repr);
                  FStar_VConfig.smtencoding_l_arith_repr =
                    (uu___2.FStar_VConfig.smtencoding_l_arith_repr);
                  FStar_VConfig.smtencoding_valid_intro =
                    (uu___2.FStar_VConfig.smtencoding_valid_intro);
                  FStar_VConfig.smtencoding_valid_elim =
                    (uu___2.FStar_VConfig.smtencoding_valid_elim);
                  FStar_VConfig.tcnorm = (uu___2.FStar_VConfig.tcnorm);
                  FStar_VConfig.no_plugins =
                    (uu___2.FStar_VConfig.no_plugins);
                  FStar_VConfig.no_tactics =
                    (uu___2.FStar_VConfig.no_tactics);
                  FStar_VConfig.z3cliopt = (uu___2.FStar_VConfig.z3cliopt);
                  FStar_VConfig.z3smtopt = (uu___2.FStar_VConfig.z3smtopt);
                  FStar_VConfig.z3refresh = (uu___2.FStar_VConfig.z3refresh);
                  FStar_VConfig.z3rlimit = v;
                  FStar_VConfig.z3rlimit_factor =
                    (uu___2.FStar_VConfig.z3rlimit_factor);
                  FStar_VConfig.z3seed = (uu___2.FStar_VConfig.z3seed);
                  FStar_VConfig.z3version = (uu___2.FStar_VConfig.z3version);
                  FStar_VConfig.trivial_pre_for_unannotated_effectful_fns =
                    (uu___2.FStar_VConfig.trivial_pre_for_unannotated_effectful_fns);
                  FStar_VConfig.reuse_hint_for =
                    (uu___2.FStar_VConfig.reuse_hint_for)
                })) in
    FStar_Tactics_Effect.tac_bind
      (FStar_Sealed.seal
         (Obj.magic
            (FStar_Range.mk_range "FStar.Tactics.SMT.fst" (Prims.of_int (21))
               (Prims.of_int (59)) (Prims.of_int (21)) (Prims.of_int (91)))))
      (FStar_Sealed.seal
         (Obj.magic
            (FStar_Range.mk_range "FStar.Tactics.SMT.fst" (Prims.of_int (21))
               (Prims.of_int (45)) (Prims.of_int (21)) (Prims.of_int (93)))))
      (Obj.magic uu___)
      (fun uu___1 ->
         (fun uu___1 ->
            Obj.magic (FStar_Tactics_V2_Builtins.set_vconfig uu___1)) uu___1)
let _ =
  FStar_Tactics_Native.register_tactic "FStar.Tactics.SMT.set_rlimit"
    (Prims.of_int (2))
    (fun psc ->
       fun ncb ->
         fun us ->
           fun args ->
             FStar_Tactics_InterpFuns.mk_tactic_interpretation_1
               "FStar.Tactics.SMT.set_rlimit (plugin)"
               (FStar_Tactics_Native.from_tactic_1 set_rlimit)
               FStar_Syntax_Embeddings.e_int FStar_Syntax_Embeddings.e_unit
               psc ncb us args)
let (get_initial_fuel :
  unit -> (Prims.int, unit) FStar_Tactics_Effect.tac_repr) =
  fun uu___ ->
    let uu___1 = FStar_Tactics_V2_Builtins.get_vconfig () in
    FStar_Tactics_Effect.tac_bind
      (FStar_Sealed.seal
         (Obj.magic
            (FStar_Range.mk_range "FStar.Tactics.SMT.fst" (Prims.of_int (23))
               (Prims.of_int (45)) (Prims.of_int (23)) (Prims.of_int (61)))))
      (FStar_Sealed.seal
         (Obj.magic
            (FStar_Range.mk_range "FStar.Tactics.SMT.fst" (Prims.of_int (23))
               (Prims.of_int (45)) (Prims.of_int (23)) (Prims.of_int (74)))))
      (Obj.magic uu___1)
      (fun uu___2 ->
         FStar_Tactics_Effect.lift_div_tac
           (fun uu___3 -> uu___2.FStar_VConfig.initial_fuel))
let _ =
  FStar_Tactics_Native.register_tactic "FStar.Tactics.SMT.get_initial_fuel"
    (Prims.of_int (2))
    (fun psc ->
       fun ncb ->
         fun us ->
           fun args ->
             FStar_Tactics_InterpFuns.mk_tactic_interpretation_1
               "FStar.Tactics.SMT.get_initial_fuel (plugin)"
               (FStar_Tactics_Native.from_tactic_1 get_initial_fuel)
               FStar_Syntax_Embeddings.e_unit FStar_Syntax_Embeddings.e_int
               psc ncb us args)
let (get_initial_ifuel :
  unit -> (Prims.int, unit) FStar_Tactics_Effect.tac_repr) =
  fun uu___ ->
    let uu___1 = FStar_Tactics_V2_Builtins.get_vconfig () in
    FStar_Tactics_Effect.tac_bind
      (FStar_Sealed.seal
         (Obj.magic
            (FStar_Range.mk_range "FStar.Tactics.SMT.fst" (Prims.of_int (24))
               (Prims.of_int (45)) (Prims.of_int (24)) (Prims.of_int (61)))))
      (FStar_Sealed.seal
         (Obj.magic
            (FStar_Range.mk_range "FStar.Tactics.SMT.fst" (Prims.of_int (24))
               (Prims.of_int (45)) (Prims.of_int (24)) (Prims.of_int (75)))))
      (Obj.magic uu___1)
      (fun uu___2 ->
         FStar_Tactics_Effect.lift_div_tac
           (fun uu___3 -> uu___2.FStar_VConfig.initial_ifuel))
let _ =
  FStar_Tactics_Native.register_tactic "FStar.Tactics.SMT.get_initial_ifuel"
    (Prims.of_int (2))
    (fun psc ->
       fun ncb ->
         fun us ->
           fun args ->
             FStar_Tactics_InterpFuns.mk_tactic_interpretation_1
               "FStar.Tactics.SMT.get_initial_ifuel (plugin)"
               (FStar_Tactics_Native.from_tactic_1 get_initial_ifuel)
               FStar_Syntax_Embeddings.e_unit FStar_Syntax_Embeddings.e_int
               psc ncb us args)
let (get_max_fuel : unit -> (Prims.int, unit) FStar_Tactics_Effect.tac_repr)
  =
  fun uu___ ->
    let uu___1 = FStar_Tactics_V2_Builtins.get_vconfig () in
    FStar_Tactics_Effect.tac_bind
      (FStar_Sealed.seal
         (Obj.magic
            (FStar_Range.mk_range "FStar.Tactics.SMT.fst" (Prims.of_int (25))
               (Prims.of_int (45)) (Prims.of_int (25)) (Prims.of_int (61)))))
      (FStar_Sealed.seal
         (Obj.magic
            (FStar_Range.mk_range "FStar.Tactics.SMT.fst" (Prims.of_int (25))
               (Prims.of_int (45)) (Prims.of_int (25)) (Prims.of_int (70)))))
      (Obj.magic uu___1)
      (fun uu___2 ->
         FStar_Tactics_Effect.lift_div_tac
           (fun uu___3 -> uu___2.FStar_VConfig.max_fuel))
let _ =
  FStar_Tactics_Native.register_tactic "FStar.Tactics.SMT.get_max_fuel"
    (Prims.of_int (2))
    (fun psc ->
       fun ncb ->
         fun us ->
           fun args ->
             FStar_Tactics_InterpFuns.mk_tactic_interpretation_1
               "FStar.Tactics.SMT.get_max_fuel (plugin)"
               (FStar_Tactics_Native.from_tactic_1 get_max_fuel)
               FStar_Syntax_Embeddings.e_unit FStar_Syntax_Embeddings.e_int
               psc ncb us args)
let (get_max_ifuel : unit -> (Prims.int, unit) FStar_Tactics_Effect.tac_repr)
  =
  fun uu___ ->
    let uu___1 = FStar_Tactics_V2_Builtins.get_vconfig () in
    FStar_Tactics_Effect.tac_bind
      (FStar_Sealed.seal
         (Obj.magic
            (FStar_Range.mk_range "FStar.Tactics.SMT.fst" (Prims.of_int (26))
               (Prims.of_int (45)) (Prims.of_int (26)) (Prims.of_int (61)))))
      (FStar_Sealed.seal
         (Obj.magic
            (FStar_Range.mk_range "FStar.Tactics.SMT.fst" (Prims.of_int (26))
               (Prims.of_int (45)) (Prims.of_int (26)) (Prims.of_int (71)))))
      (Obj.magic uu___1)
      (fun uu___2 ->
         FStar_Tactics_Effect.lift_div_tac
           (fun uu___3 -> uu___2.FStar_VConfig.max_ifuel))
let _ =
  FStar_Tactics_Native.register_tactic "FStar.Tactics.SMT.get_max_ifuel"
    (Prims.of_int (2))
    (fun psc ->
       fun ncb ->
         fun us ->
           fun args ->
             FStar_Tactics_InterpFuns.mk_tactic_interpretation_1
               "FStar.Tactics.SMT.get_max_ifuel (plugin)"
               (FStar_Tactics_Native.from_tactic_1 get_max_ifuel)
               FStar_Syntax_Embeddings.e_unit FStar_Syntax_Embeddings.e_int
               psc ncb us args)
let (set_initial_fuel :
  Prims.int -> (unit, unit) FStar_Tactics_Effect.tac_repr) =
  fun v ->
    let uu___ =
      let uu___1 = FStar_Tactics_V2_Builtins.get_vconfig () in
      FStar_Tactics_Effect.tac_bind
        (FStar_Sealed.seal
           (Obj.magic
              (FStar_Range.mk_range "FStar.Tactics.SMT.fst"
                 (Prims.of_int (28)) (Prims.of_int (59)) (Prims.of_int (28))
                 (Prims.of_int (73)))))
        (FStar_Sealed.seal
           (Obj.magic
              (FStar_Range.mk_range "FStar.Tactics.SMT.fst"
                 (Prims.of_int (28)) (Prims.of_int (59)) (Prims.of_int (28))
                 (Prims.of_int (96))))) (Obj.magic uu___1)
        (fun uu___2 ->
           FStar_Tactics_Effect.lift_div_tac
             (fun uu___3 ->
                {
                  FStar_VConfig.initial_fuel = v;
                  FStar_VConfig.max_fuel = (uu___2.FStar_VConfig.max_fuel);
                  FStar_VConfig.initial_ifuel =
                    (uu___2.FStar_VConfig.initial_ifuel);
                  FStar_VConfig.max_ifuel = (uu___2.FStar_VConfig.max_ifuel);
                  FStar_VConfig.detail_errors =
                    (uu___2.FStar_VConfig.detail_errors);
                  FStar_VConfig.detail_hint_replay =
                    (uu___2.FStar_VConfig.detail_hint_replay);
                  FStar_VConfig.no_smt = (uu___2.FStar_VConfig.no_smt);
                  FStar_VConfig.quake_lo = (uu___2.FStar_VConfig.quake_lo);
                  FStar_VConfig.quake_hi = (uu___2.FStar_VConfig.quake_hi);
                  FStar_VConfig.quake_keep =
                    (uu___2.FStar_VConfig.quake_keep);
                  FStar_VConfig.retry = (uu___2.FStar_VConfig.retry);
                  FStar_VConfig.smtencoding_elim_box =
                    (uu___2.FStar_VConfig.smtencoding_elim_box);
                  FStar_VConfig.smtencoding_nl_arith_repr =
                    (uu___2.FStar_VConfig.smtencoding_nl_arith_repr);
                  FStar_VConfig.smtencoding_l_arith_repr =
                    (uu___2.FStar_VConfig.smtencoding_l_arith_repr);
                  FStar_VConfig.smtencoding_valid_intro =
                    (uu___2.FStar_VConfig.smtencoding_valid_intro);
                  FStar_VConfig.smtencoding_valid_elim =
                    (uu___2.FStar_VConfig.smtencoding_valid_elim);
                  FStar_VConfig.tcnorm = (uu___2.FStar_VConfig.tcnorm);
                  FStar_VConfig.no_plugins =
                    (uu___2.FStar_VConfig.no_plugins);
                  FStar_VConfig.no_tactics =
                    (uu___2.FStar_VConfig.no_tactics);
                  FStar_VConfig.z3cliopt = (uu___2.FStar_VConfig.z3cliopt);
                  FStar_VConfig.z3smtopt = (uu___2.FStar_VConfig.z3smtopt);
                  FStar_VConfig.z3refresh = (uu___2.FStar_VConfig.z3refresh);
                  FStar_VConfig.z3rlimit = (uu___2.FStar_VConfig.z3rlimit);
                  FStar_VConfig.z3rlimit_factor =
                    (uu___2.FStar_VConfig.z3rlimit_factor);
                  FStar_VConfig.z3seed = (uu___2.FStar_VConfig.z3seed);
                  FStar_VConfig.z3version = (uu___2.FStar_VConfig.z3version);
                  FStar_VConfig.trivial_pre_for_unannotated_effectful_fns =
                    (uu___2.FStar_VConfig.trivial_pre_for_unannotated_effectful_fns);
                  FStar_VConfig.reuse_hint_for =
                    (uu___2.FStar_VConfig.reuse_hint_for)
                })) in
    FStar_Tactics_Effect.tac_bind
      (FStar_Sealed.seal
         (Obj.magic
            (FStar_Range.mk_range "FStar.Tactics.SMT.fst" (Prims.of_int (28))
               (Prims.of_int (59)) (Prims.of_int (28)) (Prims.of_int (96)))))
      (FStar_Sealed.seal
         (Obj.magic
            (FStar_Range.mk_range "FStar.Tactics.SMT.fst" (Prims.of_int (28))
               (Prims.of_int (45)) (Prims.of_int (28)) (Prims.of_int (98)))))
      (Obj.magic uu___)
      (fun uu___1 ->
         (fun uu___1 ->
            Obj.magic (FStar_Tactics_V2_Builtins.set_vconfig uu___1)) uu___1)
let _ =
  FStar_Tactics_Native.register_tactic "FStar.Tactics.SMT.set_initial_fuel"
    (Prims.of_int (2))
    (fun psc ->
       fun ncb ->
         fun us ->
           fun args ->
             FStar_Tactics_InterpFuns.mk_tactic_interpretation_1
               "FStar.Tactics.SMT.set_initial_fuel (plugin)"
               (FStar_Tactics_Native.from_tactic_1 set_initial_fuel)
               FStar_Syntax_Embeddings.e_int FStar_Syntax_Embeddings.e_unit
               psc ncb us args)
let (set_initial_ifuel :
  Prims.int -> (unit, unit) FStar_Tactics_Effect.tac_repr) =
  fun v ->
    let uu___ =
      let uu___1 = FStar_Tactics_V2_Builtins.get_vconfig () in
      FStar_Tactics_Effect.tac_bind
        (FStar_Sealed.seal
           (Obj.magic
              (FStar_Range.mk_range "FStar.Tactics.SMT.fst"
                 (Prims.of_int (29)) (Prims.of_int (59)) (Prims.of_int (29))
                 (Prims.of_int (73)))))
        (FStar_Sealed.seal
           (Obj.magic
              (FStar_Range.mk_range "FStar.Tactics.SMT.fst"
                 (Prims.of_int (29)) (Prims.of_int (59)) (Prims.of_int (29))
                 (Prims.of_int (96))))) (Obj.magic uu___1)
        (fun uu___2 ->
           FStar_Tactics_Effect.lift_div_tac
             (fun uu___3 ->
                {
                  FStar_VConfig.initial_fuel =
                    (uu___2.FStar_VConfig.initial_fuel);
                  FStar_VConfig.max_fuel = (uu___2.FStar_VConfig.max_fuel);
                  FStar_VConfig.initial_ifuel = v;
                  FStar_VConfig.max_ifuel = (uu___2.FStar_VConfig.max_ifuel);
                  FStar_VConfig.detail_errors =
                    (uu___2.FStar_VConfig.detail_errors);
                  FStar_VConfig.detail_hint_replay =
                    (uu___2.FStar_VConfig.detail_hint_replay);
                  FStar_VConfig.no_smt = (uu___2.FStar_VConfig.no_smt);
                  FStar_VConfig.quake_lo = (uu___2.FStar_VConfig.quake_lo);
                  FStar_VConfig.quake_hi = (uu___2.FStar_VConfig.quake_hi);
                  FStar_VConfig.quake_keep =
                    (uu___2.FStar_VConfig.quake_keep);
                  FStar_VConfig.retry = (uu___2.FStar_VConfig.retry);
                  FStar_VConfig.smtencoding_elim_box =
                    (uu___2.FStar_VConfig.smtencoding_elim_box);
                  FStar_VConfig.smtencoding_nl_arith_repr =
                    (uu___2.FStar_VConfig.smtencoding_nl_arith_repr);
                  FStar_VConfig.smtencoding_l_arith_repr =
                    (uu___2.FStar_VConfig.smtencoding_l_arith_repr);
                  FStar_VConfig.smtencoding_valid_intro =
                    (uu___2.FStar_VConfig.smtencoding_valid_intro);
                  FStar_VConfig.smtencoding_valid_elim =
                    (uu___2.FStar_VConfig.smtencoding_valid_elim);
                  FStar_VConfig.tcnorm = (uu___2.FStar_VConfig.tcnorm);
                  FStar_VConfig.no_plugins =
                    (uu___2.FStar_VConfig.no_plugins);
                  FStar_VConfig.no_tactics =
                    (uu___2.FStar_VConfig.no_tactics);
                  FStar_VConfig.z3cliopt = (uu___2.FStar_VConfig.z3cliopt);
                  FStar_VConfig.z3smtopt = (uu___2.FStar_VConfig.z3smtopt);
                  FStar_VConfig.z3refresh = (uu___2.FStar_VConfig.z3refresh);
                  FStar_VConfig.z3rlimit = (uu___2.FStar_VConfig.z3rlimit);
                  FStar_VConfig.z3rlimit_factor =
                    (uu___2.FStar_VConfig.z3rlimit_factor);
                  FStar_VConfig.z3seed = (uu___2.FStar_VConfig.z3seed);
                  FStar_VConfig.z3version = (uu___2.FStar_VConfig.z3version);
                  FStar_VConfig.trivial_pre_for_unannotated_effectful_fns =
                    (uu___2.FStar_VConfig.trivial_pre_for_unannotated_effectful_fns);
                  FStar_VConfig.reuse_hint_for =
                    (uu___2.FStar_VConfig.reuse_hint_for)
                })) in
    FStar_Tactics_Effect.tac_bind
      (FStar_Sealed.seal
         (Obj.magic
            (FStar_Range.mk_range "FStar.Tactics.SMT.fst" (Prims.of_int (29))
               (Prims.of_int (59)) (Prims.of_int (29)) (Prims.of_int (96)))))
      (FStar_Sealed.seal
         (Obj.magic
            (FStar_Range.mk_range "FStar.Tactics.SMT.fst" (Prims.of_int (29))
               (Prims.of_int (45)) (Prims.of_int (29)) (Prims.of_int (98)))))
      (Obj.magic uu___)
      (fun uu___1 ->
         (fun uu___1 ->
            Obj.magic (FStar_Tactics_V2_Builtins.set_vconfig uu___1)) uu___1)
let _ =
  FStar_Tactics_Native.register_tactic "FStar.Tactics.SMT.set_initial_ifuel"
    (Prims.of_int (2))
    (fun psc ->
       fun ncb ->
         fun us ->
           fun args ->
             FStar_Tactics_InterpFuns.mk_tactic_interpretation_1
               "FStar.Tactics.SMT.set_initial_ifuel (plugin)"
               (FStar_Tactics_Native.from_tactic_1 set_initial_ifuel)
               FStar_Syntax_Embeddings.e_int FStar_Syntax_Embeddings.e_unit
               psc ncb us args)
let (set_max_fuel : Prims.int -> (unit, unit) FStar_Tactics_Effect.tac_repr)
  =
  fun v ->
    let uu___ =
      let uu___1 = FStar_Tactics_V2_Builtins.get_vconfig () in
      FStar_Tactics_Effect.tac_bind
        (FStar_Sealed.seal
           (Obj.magic
              (FStar_Range.mk_range "FStar.Tactics.SMT.fst"
                 (Prims.of_int (30)) (Prims.of_int (59)) (Prims.of_int (30))
                 (Prims.of_int (73)))))
        (FStar_Sealed.seal
           (Obj.magic
              (FStar_Range.mk_range "FStar.Tactics.SMT.fst"
                 (Prims.of_int (30)) (Prims.of_int (59)) (Prims.of_int (30))
                 (Prims.of_int (96))))) (Obj.magic uu___1)
        (fun uu___2 ->
           FStar_Tactics_Effect.lift_div_tac
             (fun uu___3 ->
                {
                  FStar_VConfig.initial_fuel =
                    (uu___2.FStar_VConfig.initial_fuel);
                  FStar_VConfig.max_fuel = v;
                  FStar_VConfig.initial_ifuel =
                    (uu___2.FStar_VConfig.initial_ifuel);
                  FStar_VConfig.max_ifuel = (uu___2.FStar_VConfig.max_ifuel);
                  FStar_VConfig.detail_errors =
                    (uu___2.FStar_VConfig.detail_errors);
                  FStar_VConfig.detail_hint_replay =
                    (uu___2.FStar_VConfig.detail_hint_replay);
                  FStar_VConfig.no_smt = (uu___2.FStar_VConfig.no_smt);
                  FStar_VConfig.quake_lo = (uu___2.FStar_VConfig.quake_lo);
                  FStar_VConfig.quake_hi = (uu___2.FStar_VConfig.quake_hi);
                  FStar_VConfig.quake_keep =
                    (uu___2.FStar_VConfig.quake_keep);
                  FStar_VConfig.retry = (uu___2.FStar_VConfig.retry);
                  FStar_VConfig.smtencoding_elim_box =
                    (uu___2.FStar_VConfig.smtencoding_elim_box);
                  FStar_VConfig.smtencoding_nl_arith_repr =
                    (uu___2.FStar_VConfig.smtencoding_nl_arith_repr);
                  FStar_VConfig.smtencoding_l_arith_repr =
                    (uu___2.FStar_VConfig.smtencoding_l_arith_repr);
                  FStar_VConfig.smtencoding_valid_intro =
                    (uu___2.FStar_VConfig.smtencoding_valid_intro);
                  FStar_VConfig.smtencoding_valid_elim =
                    (uu___2.FStar_VConfig.smtencoding_valid_elim);
                  FStar_VConfig.tcnorm = (uu___2.FStar_VConfig.tcnorm);
                  FStar_VConfig.no_plugins =
                    (uu___2.FStar_VConfig.no_plugins);
                  FStar_VConfig.no_tactics =
                    (uu___2.FStar_VConfig.no_tactics);
                  FStar_VConfig.z3cliopt = (uu___2.FStar_VConfig.z3cliopt);
                  FStar_VConfig.z3smtopt = (uu___2.FStar_VConfig.z3smtopt);
                  FStar_VConfig.z3refresh = (uu___2.FStar_VConfig.z3refresh);
                  FStar_VConfig.z3rlimit = (uu___2.FStar_VConfig.z3rlimit);
                  FStar_VConfig.z3rlimit_factor =
                    (uu___2.FStar_VConfig.z3rlimit_factor);
                  FStar_VConfig.z3seed = (uu___2.FStar_VConfig.z3seed);
                  FStar_VConfig.z3version = (uu___2.FStar_VConfig.z3version);
                  FStar_VConfig.trivial_pre_for_unannotated_effectful_fns =
                    (uu___2.FStar_VConfig.trivial_pre_for_unannotated_effectful_fns);
                  FStar_VConfig.reuse_hint_for =
                    (uu___2.FStar_VConfig.reuse_hint_for)
                })) in
    FStar_Tactics_Effect.tac_bind
      (FStar_Sealed.seal
         (Obj.magic
            (FStar_Range.mk_range "FStar.Tactics.SMT.fst" (Prims.of_int (30))
               (Prims.of_int (59)) (Prims.of_int (30)) (Prims.of_int (96)))))
      (FStar_Sealed.seal
         (Obj.magic
            (FStar_Range.mk_range "FStar.Tactics.SMT.fst" (Prims.of_int (30))
               (Prims.of_int (45)) (Prims.of_int (30)) (Prims.of_int (98)))))
      (Obj.magic uu___)
      (fun uu___1 ->
         (fun uu___1 ->
            Obj.magic (FStar_Tactics_V2_Builtins.set_vconfig uu___1)) uu___1)
let _ =
  FStar_Tactics_Native.register_tactic "FStar.Tactics.SMT.set_max_fuel"
    (Prims.of_int (2))
    (fun psc ->
       fun ncb ->
         fun us ->
           fun args ->
             FStar_Tactics_InterpFuns.mk_tactic_interpretation_1
               "FStar.Tactics.SMT.set_max_fuel (plugin)"
               (FStar_Tactics_Native.from_tactic_1 set_max_fuel)
               FStar_Syntax_Embeddings.e_int FStar_Syntax_Embeddings.e_unit
               psc ncb us args)
let (set_max_ifuel : Prims.int -> (unit, unit) FStar_Tactics_Effect.tac_repr)
  =
  fun v ->
    let uu___ =
      let uu___1 = FStar_Tactics_V2_Builtins.get_vconfig () in
      FStar_Tactics_Effect.tac_bind
        (FStar_Sealed.seal
           (Obj.magic
              (FStar_Range.mk_range "FStar.Tactics.SMT.fst"
                 (Prims.of_int (31)) (Prims.of_int (59)) (Prims.of_int (31))
                 (Prims.of_int (73)))))
        (FStar_Sealed.seal
           (Obj.magic
              (FStar_Range.mk_range "FStar.Tactics.SMT.fst"
                 (Prims.of_int (31)) (Prims.of_int (59)) (Prims.of_int (31))
                 (Prims.of_int (96))))) (Obj.magic uu___1)
        (fun uu___2 ->
           FStar_Tactics_Effect.lift_div_tac
             (fun uu___3 ->
                {
                  FStar_VConfig.initial_fuel =
                    (uu___2.FStar_VConfig.initial_fuel);
                  FStar_VConfig.max_fuel = (uu___2.FStar_VConfig.max_fuel);
                  FStar_VConfig.initial_ifuel =
                    (uu___2.FStar_VConfig.initial_ifuel);
                  FStar_VConfig.max_ifuel = v;
                  FStar_VConfig.detail_errors =
                    (uu___2.FStar_VConfig.detail_errors);
                  FStar_VConfig.detail_hint_replay =
                    (uu___2.FStar_VConfig.detail_hint_replay);
                  FStar_VConfig.no_smt = (uu___2.FStar_VConfig.no_smt);
                  FStar_VConfig.quake_lo = (uu___2.FStar_VConfig.quake_lo);
                  FStar_VConfig.quake_hi = (uu___2.FStar_VConfig.quake_hi);
                  FStar_VConfig.quake_keep =
                    (uu___2.FStar_VConfig.quake_keep);
                  FStar_VConfig.retry = (uu___2.FStar_VConfig.retry);
                  FStar_VConfig.smtencoding_elim_box =
                    (uu___2.FStar_VConfig.smtencoding_elim_box);
                  FStar_VConfig.smtencoding_nl_arith_repr =
                    (uu___2.FStar_VConfig.smtencoding_nl_arith_repr);
                  FStar_VConfig.smtencoding_l_arith_repr =
                    (uu___2.FStar_VConfig.smtencoding_l_arith_repr);
                  FStar_VConfig.smtencoding_valid_intro =
                    (uu___2.FStar_VConfig.smtencoding_valid_intro);
                  FStar_VConfig.smtencoding_valid_elim =
                    (uu___2.FStar_VConfig.smtencoding_valid_elim);
                  FStar_VConfig.tcnorm = (uu___2.FStar_VConfig.tcnorm);
                  FStar_VConfig.no_plugins =
                    (uu___2.FStar_VConfig.no_plugins);
                  FStar_VConfig.no_tactics =
                    (uu___2.FStar_VConfig.no_tactics);
                  FStar_VConfig.z3cliopt = (uu___2.FStar_VConfig.z3cliopt);
                  FStar_VConfig.z3smtopt = (uu___2.FStar_VConfig.z3smtopt);
                  FStar_VConfig.z3refresh = (uu___2.FStar_VConfig.z3refresh);
                  FStar_VConfig.z3rlimit = (uu___2.FStar_VConfig.z3rlimit);
                  FStar_VConfig.z3rlimit_factor =
                    (uu___2.FStar_VConfig.z3rlimit_factor);
                  FStar_VConfig.z3seed = (uu___2.FStar_VConfig.z3seed);
                  FStar_VConfig.z3version = (uu___2.FStar_VConfig.z3version);
                  FStar_VConfig.trivial_pre_for_unannotated_effectful_fns =
                    (uu___2.FStar_VConfig.trivial_pre_for_unannotated_effectful_fns);
                  FStar_VConfig.reuse_hint_for =
                    (uu___2.FStar_VConfig.reuse_hint_for)
                })) in
    FStar_Tactics_Effect.tac_bind
      (FStar_Sealed.seal
         (Obj.magic
            (FStar_Range.mk_range "FStar.Tactics.SMT.fst" (Prims.of_int (31))
               (Prims.of_int (59)) (Prims.of_int (31)) (Prims.of_int (96)))))
      (FStar_Sealed.seal
         (Obj.magic
            (FStar_Range.mk_range "FStar.Tactics.SMT.fst" (Prims.of_int (31))
               (Prims.of_int (45)) (Prims.of_int (31)) (Prims.of_int (98)))))
      (Obj.magic uu___)
      (fun uu___1 ->
         (fun uu___1 ->
            Obj.magic (FStar_Tactics_V2_Builtins.set_vconfig uu___1)) uu___1)
let _ =
  FStar_Tactics_Native.register_tactic "FStar.Tactics.SMT.set_max_ifuel"
    (Prims.of_int (2))
    (fun psc ->
       fun ncb ->
         fun us ->
           fun args ->
             FStar_Tactics_InterpFuns.mk_tactic_interpretation_1
               "FStar.Tactics.SMT.set_max_ifuel (plugin)"
               (FStar_Tactics_Native.from_tactic_1 set_max_ifuel)
               FStar_Syntax_Embeddings.e_int FStar_Syntax_Embeddings.e_unit
               psc ncb us args)
let (set_fuel : Prims.int -> (unit, unit) FStar_Tactics_Effect.tac_repr) =
  fun v ->
    let uu___ =
      let uu___1 = FStar_Tactics_V2_Builtins.get_vconfig () in
      FStar_Tactics_Effect.tac_bind
        (FStar_Sealed.seal
           (Obj.magic
              (FStar_Range.mk_range "FStar.Tactics.SMT.fst"
                 (Prims.of_int (34)) (Prims.of_int (59)) (Prims.of_int (34))
                 (Prims.of_int (73)))))
        (FStar_Sealed.seal
           (Obj.magic
              (FStar_Range.mk_range "FStar.Tactics.SMT.fst"
                 (Prims.of_int (34)) (Prims.of_int (59)) (Prims.of_int (34))
                 (Prims.of_int (111))))) (Obj.magic uu___1)
        (fun uu___2 ->
           FStar_Tactics_Effect.lift_div_tac
             (fun uu___3 ->
                {
                  FStar_VConfig.initial_fuel = v;
                  FStar_VConfig.max_fuel = v;
                  FStar_VConfig.initial_ifuel =
                    (uu___2.FStar_VConfig.initial_ifuel);
                  FStar_VConfig.max_ifuel = (uu___2.FStar_VConfig.max_ifuel);
                  FStar_VConfig.detail_errors =
                    (uu___2.FStar_VConfig.detail_errors);
                  FStar_VConfig.detail_hint_replay =
                    (uu___2.FStar_VConfig.detail_hint_replay);
                  FStar_VConfig.no_smt = (uu___2.FStar_VConfig.no_smt);
                  FStar_VConfig.quake_lo = (uu___2.FStar_VConfig.quake_lo);
                  FStar_VConfig.quake_hi = (uu___2.FStar_VConfig.quake_hi);
                  FStar_VConfig.quake_keep =
                    (uu___2.FStar_VConfig.quake_keep);
                  FStar_VConfig.retry = (uu___2.FStar_VConfig.retry);
                  FStar_VConfig.smtencoding_elim_box =
                    (uu___2.FStar_VConfig.smtencoding_elim_box);
                  FStar_VConfig.smtencoding_nl_arith_repr =
                    (uu___2.FStar_VConfig.smtencoding_nl_arith_repr);
                  FStar_VConfig.smtencoding_l_arith_repr =
                    (uu___2.FStar_VConfig.smtencoding_l_arith_repr);
                  FStar_VConfig.smtencoding_valid_intro =
                    (uu___2.FStar_VConfig.smtencoding_valid_intro);
                  FStar_VConfig.smtencoding_valid_elim =
                    (uu___2.FStar_VConfig.smtencoding_valid_elim);
                  FStar_VConfig.tcnorm = (uu___2.FStar_VConfig.tcnorm);
                  FStar_VConfig.no_plugins =
                    (uu___2.FStar_VConfig.no_plugins);
                  FStar_VConfig.no_tactics =
                    (uu___2.FStar_VConfig.no_tactics);
                  FStar_VConfig.z3cliopt = (uu___2.FStar_VConfig.z3cliopt);
                  FStar_VConfig.z3smtopt = (uu___2.FStar_VConfig.z3smtopt);
                  FStar_VConfig.z3refresh = (uu___2.FStar_VConfig.z3refresh);
                  FStar_VConfig.z3rlimit = (uu___2.FStar_VConfig.z3rlimit);
                  FStar_VConfig.z3rlimit_factor =
                    (uu___2.FStar_VConfig.z3rlimit_factor);
                  FStar_VConfig.z3seed = (uu___2.FStar_VConfig.z3seed);
                  FStar_VConfig.z3version = (uu___2.FStar_VConfig.z3version);
                  FStar_VConfig.trivial_pre_for_unannotated_effectful_fns =
                    (uu___2.FStar_VConfig.trivial_pre_for_unannotated_effectful_fns);
                  FStar_VConfig.reuse_hint_for =
                    (uu___2.FStar_VConfig.reuse_hint_for)
                })) in
    FStar_Tactics_Effect.tac_bind
      (FStar_Sealed.seal
         (Obj.magic
            (FStar_Range.mk_range "FStar.Tactics.SMT.fst" (Prims.of_int (34))
               (Prims.of_int (59)) (Prims.of_int (34)) (Prims.of_int (111)))))
      (FStar_Sealed.seal
         (Obj.magic
            (FStar_Range.mk_range "FStar.Tactics.SMT.fst" (Prims.of_int (34))
               (Prims.of_int (45)) (Prims.of_int (34)) (Prims.of_int (113)))))
      (Obj.magic uu___)
      (fun uu___1 ->
         (fun uu___1 ->
            Obj.magic (FStar_Tactics_V2_Builtins.set_vconfig uu___1)) uu___1)
let _ =
  FStar_Tactics_Native.register_tactic "FStar.Tactics.SMT.set_fuel"
    (Prims.of_int (2))
    (fun psc ->
       fun ncb ->
         fun us ->
           fun args ->
             FStar_Tactics_InterpFuns.mk_tactic_interpretation_1
               "FStar.Tactics.SMT.set_fuel (plugin)"
               (FStar_Tactics_Native.from_tactic_1 set_fuel)
               FStar_Syntax_Embeddings.e_int FStar_Syntax_Embeddings.e_unit
               psc ncb us args)
let (set_ifuel : Prims.int -> (unit, unit) FStar_Tactics_Effect.tac_repr) =
  fun v ->
    let uu___ =
      let uu___1 = FStar_Tactics_V2_Builtins.get_vconfig () in
      FStar_Tactics_Effect.tac_bind
        (FStar_Sealed.seal
           (Obj.magic
              (FStar_Range.mk_range "FStar.Tactics.SMT.fst"
                 (Prims.of_int (35)) (Prims.of_int (59)) (Prims.of_int (35))
                 (Prims.of_int (73)))))
        (FStar_Sealed.seal
           (Obj.magic
              (FStar_Range.mk_range "FStar.Tactics.SMT.fst"
                 (Prims.of_int (35)) (Prims.of_int (59)) (Prims.of_int (35))
                 (Prims.of_int (111))))) (Obj.magic uu___1)
        (fun uu___2 ->
           FStar_Tactics_Effect.lift_div_tac
             (fun uu___3 ->
                {
                  FStar_VConfig.initial_fuel =
                    (uu___2.FStar_VConfig.initial_fuel);
                  FStar_VConfig.max_fuel = (uu___2.FStar_VConfig.max_fuel);
                  FStar_VConfig.initial_ifuel = v;
                  FStar_VConfig.max_ifuel = v;
                  FStar_VConfig.detail_errors =
                    (uu___2.FStar_VConfig.detail_errors);
                  FStar_VConfig.detail_hint_replay =
                    (uu___2.FStar_VConfig.detail_hint_replay);
                  FStar_VConfig.no_smt = (uu___2.FStar_VConfig.no_smt);
                  FStar_VConfig.quake_lo = (uu___2.FStar_VConfig.quake_lo);
                  FStar_VConfig.quake_hi = (uu___2.FStar_VConfig.quake_hi);
                  FStar_VConfig.quake_keep =
                    (uu___2.FStar_VConfig.quake_keep);
                  FStar_VConfig.retry = (uu___2.FStar_VConfig.retry);
                  FStar_VConfig.smtencoding_elim_box =
                    (uu___2.FStar_VConfig.smtencoding_elim_box);
                  FStar_VConfig.smtencoding_nl_arith_repr =
                    (uu___2.FStar_VConfig.smtencoding_nl_arith_repr);
                  FStar_VConfig.smtencoding_l_arith_repr =
                    (uu___2.FStar_VConfig.smtencoding_l_arith_repr);
                  FStar_VConfig.smtencoding_valid_intro =
                    (uu___2.FStar_VConfig.smtencoding_valid_intro);
                  FStar_VConfig.smtencoding_valid_elim =
                    (uu___2.FStar_VConfig.smtencoding_valid_elim);
                  FStar_VConfig.tcnorm = (uu___2.FStar_VConfig.tcnorm);
                  FStar_VConfig.no_plugins =
                    (uu___2.FStar_VConfig.no_plugins);
                  FStar_VConfig.no_tactics =
                    (uu___2.FStar_VConfig.no_tactics);
                  FStar_VConfig.z3cliopt = (uu___2.FStar_VConfig.z3cliopt);
                  FStar_VConfig.z3smtopt = (uu___2.FStar_VConfig.z3smtopt);
                  FStar_VConfig.z3refresh = (uu___2.FStar_VConfig.z3refresh);
                  FStar_VConfig.z3rlimit = (uu___2.FStar_VConfig.z3rlimit);
                  FStar_VConfig.z3rlimit_factor =
                    (uu___2.FStar_VConfig.z3rlimit_factor);
                  FStar_VConfig.z3seed = (uu___2.FStar_VConfig.z3seed);
                  FStar_VConfig.z3version = (uu___2.FStar_VConfig.z3version);
                  FStar_VConfig.trivial_pre_for_unannotated_effectful_fns =
                    (uu___2.FStar_VConfig.trivial_pre_for_unannotated_effectful_fns);
                  FStar_VConfig.reuse_hint_for =
                    (uu___2.FStar_VConfig.reuse_hint_for)
                })) in
    FStar_Tactics_Effect.tac_bind
      (FStar_Sealed.seal
         (Obj.magic
            (FStar_Range.mk_range "FStar.Tactics.SMT.fst" (Prims.of_int (35))
               (Prims.of_int (59)) (Prims.of_int (35)) (Prims.of_int (111)))))
      (FStar_Sealed.seal
         (Obj.magic
            (FStar_Range.mk_range "FStar.Tactics.SMT.fst" (Prims.of_int (35))
               (Prims.of_int (45)) (Prims.of_int (35)) (Prims.of_int (113)))))
      (Obj.magic uu___)
      (fun uu___1 ->
         (fun uu___1 ->
            Obj.magic (FStar_Tactics_V2_Builtins.set_vconfig uu___1)) uu___1)
let _ =
  FStar_Tactics_Native.register_tactic "FStar.Tactics.SMT.set_ifuel"
    (Prims.of_int (2))
    (fun psc ->
       fun ncb ->
         fun us ->
           fun args ->
             FStar_Tactics_InterpFuns.mk_tactic_interpretation_1
               "FStar.Tactics.SMT.set_ifuel (plugin)"
               (FStar_Tactics_Native.from_tactic_1 set_ifuel)
               FStar_Syntax_Embeddings.e_int FStar_Syntax_Embeddings.e_unit
               psc ncb us args)