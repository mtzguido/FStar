(*
   Copyright 2008-2016 Microsoft Research

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

module FStar.Tactics.InterpFuns

(* This module is awful, don't even look at it please. *)

open FStar open FStar.Compiler
open FStar.Compiler.Effect

open FStar.Syntax.Embeddings
open FStar.Tactics.Monad

module Cfg   = FStar.TypeChecker.Cfg
module NBET  = FStar.TypeChecker.NBETerm

(* These functions are suboptimal, since they need to take embeddings for both
 * the interpreter AND the NBE evaluator. Attempting to coallesce them
 * is not easy, since we have some parametric e_any embeddings of differing types
 * (Syntax.embedding term vs NBET.embedding NBET.t). So we pass both of them.
 * We also need to pass in two versions of the underlying primitive, since they
 * will be instantiated differently (again term vs NBET.t). Such is life
 * without higher-order polymorphism. *)

val mk_total_step_1_psc :
    int ->
    string ->
    (Cfg.psc -> 'a -> 'r) ->
    embedding 'a ->
    embedding 'r ->
    (Cfg.psc -> 'na -> 'nr) ->
    NBET.embedding 'na ->
    NBET.embedding 'nr ->
    Cfg.primitive_step

val max_tac_arity : int // = 20

(* NOTE: The rest of this file is autogenerated. See .scripts/mk_tac_interp.sh *)

val mk_tac_step_1 :
  int ->
  string ->
  ('t1 -> tac 'r) ->
  embedding 't1 ->
  er:embedding 'r ->
  ('nt1 -> tac 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_tac_step_2 :
  int ->
  string ->
  ('t1 -> 't2 -> tac 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  er:embedding 'r ->
  ('nt1 -> 'nt2 -> tac 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_tac_step_3 :
  int ->
  string ->
  ('t1 -> 't2 -> 't3 -> tac 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  er:embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> tac 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_tac_step_4 :
  int ->
  string ->
  ('t1 -> 't2 -> 't3 -> 't4 -> tac 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  embedding 't4 ->
  er:embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> 'nt4 -> tac 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nt4 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_tac_step_5 :
  int ->
  string ->
  ('t1 -> 't2 -> 't3 -> 't4 -> 't5 -> tac 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  embedding 't4 ->
  embedding 't5 ->
  er:embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> 'nt4 -> 'nt5 -> tac 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nt4 ->
  NBET.embedding 'nt5 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_tac_step_6 :
  int ->
  string ->
  ('t1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> tac 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  embedding 't4 ->
  embedding 't5 ->
  embedding 't6 ->
  er:embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> 'nt4 -> 'nt5 -> 'nt6 -> tac 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nt4 ->
  NBET.embedding 'nt5 ->
  NBET.embedding 'nt6 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_tac_step_7 :
  int ->
  string ->
  ('t1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> tac 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  embedding 't4 ->
  embedding 't5 ->
  embedding 't6 ->
  embedding 't7 ->
  er:embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> 'nt4 -> 'nt5 -> 'nt6 -> 'nt7 -> tac 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nt4 ->
  NBET.embedding 'nt5 ->
  NBET.embedding 'nt6 ->
  NBET.embedding 'nt7 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_tac_step_8 :
  int ->
  string ->
  ('t1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 't8 -> tac 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  embedding 't4 ->
  embedding 't5 ->
  embedding 't6 ->
  embedding 't7 ->
  embedding 't8 ->
  er:embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> 'nt4 -> 'nt5 -> 'nt6 -> 'nt7 -> 'nt8 -> tac 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nt4 ->
  NBET.embedding 'nt5 ->
  NBET.embedding 'nt6 ->
  NBET.embedding 'nt7 ->
  NBET.embedding 'nt8 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_tac_step_9 :
  int ->
  string ->
  ('t1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 't8 -> 't9 -> tac 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  embedding 't4 ->
  embedding 't5 ->
  embedding 't6 ->
  embedding 't7 ->
  embedding 't8 ->
  embedding 't9 ->
  er:embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> 'nt4 -> 'nt5 -> 'nt6 -> 'nt7 -> 'nt8 -> 'nt9 -> tac 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nt4 ->
  NBET.embedding 'nt5 ->
  NBET.embedding 'nt6 ->
  NBET.embedding 'nt7 ->
  NBET.embedding 'nt8 ->
  NBET.embedding 'nt9 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_tac_step_10 :
  int ->
  string ->
  ('t1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 't8 -> 't9 -> 't10 -> tac 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  embedding 't4 ->
  embedding 't5 ->
  embedding 't6 ->
  embedding 't7 ->
  embedding 't8 ->
  embedding 't9 ->
  embedding 't10 ->
  er:embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> 'nt4 -> 'nt5 -> 'nt6 -> 'nt7 -> 'nt8 -> 'nt9 -> 'nt10 -> tac 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nt4 ->
  NBET.embedding 'nt5 ->
  NBET.embedding 'nt6 ->
  NBET.embedding 'nt7 ->
  NBET.embedding 'nt8 ->
  NBET.embedding 'nt9 ->
  NBET.embedding 'nt10 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_tac_step_11 :
  int ->
  string ->
  ('t1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 't8 -> 't9 -> 't10 -> 't11 -> tac 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  embedding 't4 ->
  embedding 't5 ->
  embedding 't6 ->
  embedding 't7 ->
  embedding 't8 ->
  embedding 't9 ->
  embedding 't10 ->
  embedding 't11 ->
  er:embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> 'nt4 -> 'nt5 -> 'nt6 -> 'nt7 -> 'nt8 -> 'nt9 -> 'nt10 -> 'nt11 -> tac 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nt4 ->
  NBET.embedding 'nt5 ->
  NBET.embedding 'nt6 ->
  NBET.embedding 'nt7 ->
  NBET.embedding 'nt8 ->
  NBET.embedding 'nt9 ->
  NBET.embedding 'nt10 ->
  NBET.embedding 'nt11 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_tac_step_12 :
  int ->
  string ->
  ('t1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 't8 -> 't9 -> 't10 -> 't11 -> 't12 -> tac 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  embedding 't4 ->
  embedding 't5 ->
  embedding 't6 ->
  embedding 't7 ->
  embedding 't8 ->
  embedding 't9 ->
  embedding 't10 ->
  embedding 't11 ->
  embedding 't12 ->
  er:embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> 'nt4 -> 'nt5 -> 'nt6 -> 'nt7 -> 'nt8 -> 'nt9 -> 'nt10 -> 'nt11 -> 'nt12 -> tac 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nt4 ->
  NBET.embedding 'nt5 ->
  NBET.embedding 'nt6 ->
  NBET.embedding 'nt7 ->
  NBET.embedding 'nt8 ->
  NBET.embedding 'nt9 ->
  NBET.embedding 'nt10 ->
  NBET.embedding 'nt11 ->
  NBET.embedding 'nt12 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_tac_step_13 :
  int ->
  string ->
  ('t1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 't8 -> 't9 -> 't10 -> 't11 -> 't12 -> 't13 -> tac 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  embedding 't4 ->
  embedding 't5 ->
  embedding 't6 ->
  embedding 't7 ->
  embedding 't8 ->
  embedding 't9 ->
  embedding 't10 ->
  embedding 't11 ->
  embedding 't12 ->
  embedding 't13 ->
  er:embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> 'nt4 -> 'nt5 -> 'nt6 -> 'nt7 -> 'nt8 -> 'nt9 -> 'nt10 -> 'nt11 -> 'nt12 -> 'nt13 -> tac 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nt4 ->
  NBET.embedding 'nt5 ->
  NBET.embedding 'nt6 ->
  NBET.embedding 'nt7 ->
  NBET.embedding 'nt8 ->
  NBET.embedding 'nt9 ->
  NBET.embedding 'nt10 ->
  NBET.embedding 'nt11 ->
  NBET.embedding 'nt12 ->
  NBET.embedding 'nt13 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_tac_step_14 :
  int ->
  string ->
  ('t1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 't8 -> 't9 -> 't10 -> 't11 -> 't12 -> 't13 -> 't14 -> tac 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  embedding 't4 ->
  embedding 't5 ->
  embedding 't6 ->
  embedding 't7 ->
  embedding 't8 ->
  embedding 't9 ->
  embedding 't10 ->
  embedding 't11 ->
  embedding 't12 ->
  embedding 't13 ->
  embedding 't14 ->
  er:embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> 'nt4 -> 'nt5 -> 'nt6 -> 'nt7 -> 'nt8 -> 'nt9 -> 'nt10 -> 'nt11 -> 'nt12 -> 'nt13 -> 'nt14 -> tac 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nt4 ->
  NBET.embedding 'nt5 ->
  NBET.embedding 'nt6 ->
  NBET.embedding 'nt7 ->
  NBET.embedding 'nt8 ->
  NBET.embedding 'nt9 ->
  NBET.embedding 'nt10 ->
  NBET.embedding 'nt11 ->
  NBET.embedding 'nt12 ->
  NBET.embedding 'nt13 ->
  NBET.embedding 'nt14 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_tac_step_15 :
  int ->
  string ->
  ('t1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 't8 -> 't9 -> 't10 -> 't11 -> 't12 -> 't13 -> 't14 -> 't15 -> tac 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  embedding 't4 ->
  embedding 't5 ->
  embedding 't6 ->
  embedding 't7 ->
  embedding 't8 ->
  embedding 't9 ->
  embedding 't10 ->
  embedding 't11 ->
  embedding 't12 ->
  embedding 't13 ->
  embedding 't14 ->
  embedding 't15 ->
  er:embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> 'nt4 -> 'nt5 -> 'nt6 -> 'nt7 -> 'nt8 -> 'nt9 -> 'nt10 -> 'nt11 -> 'nt12 -> 'nt13 -> 'nt14 -> 'nt15 -> tac 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nt4 ->
  NBET.embedding 'nt5 ->
  NBET.embedding 'nt6 ->
  NBET.embedding 'nt7 ->
  NBET.embedding 'nt8 ->
  NBET.embedding 'nt9 ->
  NBET.embedding 'nt10 ->
  NBET.embedding 'nt11 ->
  NBET.embedding 'nt12 ->
  NBET.embedding 'nt13 ->
  NBET.embedding 'nt14 ->
  NBET.embedding 'nt15 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_tac_step_16 :
  int ->
  string ->
  ('t1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 't8 -> 't9 -> 't10 -> 't11 -> 't12 -> 't13 -> 't14 -> 't15 -> 't16 -> tac 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  embedding 't4 ->
  embedding 't5 ->
  embedding 't6 ->
  embedding 't7 ->
  embedding 't8 ->
  embedding 't9 ->
  embedding 't10 ->
  embedding 't11 ->
  embedding 't12 ->
  embedding 't13 ->
  embedding 't14 ->
  embedding 't15 ->
  embedding 't16 ->
  er:embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> 'nt4 -> 'nt5 -> 'nt6 -> 'nt7 -> 'nt8 -> 'nt9 -> 'nt10 -> 'nt11 -> 'nt12 -> 'nt13 -> 'nt14 -> 'nt15 -> 'nt16 -> tac 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nt4 ->
  NBET.embedding 'nt5 ->
  NBET.embedding 'nt6 ->
  NBET.embedding 'nt7 ->
  NBET.embedding 'nt8 ->
  NBET.embedding 'nt9 ->
  NBET.embedding 'nt10 ->
  NBET.embedding 'nt11 ->
  NBET.embedding 'nt12 ->
  NBET.embedding 'nt13 ->
  NBET.embedding 'nt14 ->
  NBET.embedding 'nt15 ->
  NBET.embedding 'nt16 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_tac_step_17 :
  int ->
  string ->
  ('t1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 't8 -> 't9 -> 't10 -> 't11 -> 't12 -> 't13 -> 't14 -> 't15 -> 't16 -> 't17 -> tac 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  embedding 't4 ->
  embedding 't5 ->
  embedding 't6 ->
  embedding 't7 ->
  embedding 't8 ->
  embedding 't9 ->
  embedding 't10 ->
  embedding 't11 ->
  embedding 't12 ->
  embedding 't13 ->
  embedding 't14 ->
  embedding 't15 ->
  embedding 't16 ->
  embedding 't17 ->
  er:embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> 'nt4 -> 'nt5 -> 'nt6 -> 'nt7 -> 'nt8 -> 'nt9 -> 'nt10 -> 'nt11 -> 'nt12 -> 'nt13 -> 'nt14 -> 'nt15 -> 'nt16 -> 'nt17 -> tac 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nt4 ->
  NBET.embedding 'nt5 ->
  NBET.embedding 'nt6 ->
  NBET.embedding 'nt7 ->
  NBET.embedding 'nt8 ->
  NBET.embedding 'nt9 ->
  NBET.embedding 'nt10 ->
  NBET.embedding 'nt11 ->
  NBET.embedding 'nt12 ->
  NBET.embedding 'nt13 ->
  NBET.embedding 'nt14 ->
  NBET.embedding 'nt15 ->
  NBET.embedding 'nt16 ->
  NBET.embedding 'nt17 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_tac_step_18 :
  int ->
  string ->
  ('t1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 't8 -> 't9 -> 't10 -> 't11 -> 't12 -> 't13 -> 't14 -> 't15 -> 't16 -> 't17 -> 't18 -> tac 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  embedding 't4 ->
  embedding 't5 ->
  embedding 't6 ->
  embedding 't7 ->
  embedding 't8 ->
  embedding 't9 ->
  embedding 't10 ->
  embedding 't11 ->
  embedding 't12 ->
  embedding 't13 ->
  embedding 't14 ->
  embedding 't15 ->
  embedding 't16 ->
  embedding 't17 ->
  embedding 't18 ->
  er:embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> 'nt4 -> 'nt5 -> 'nt6 -> 'nt7 -> 'nt8 -> 'nt9 -> 'nt10 -> 'nt11 -> 'nt12 -> 'nt13 -> 'nt14 -> 'nt15 -> 'nt16 -> 'nt17 -> 'nt18 -> tac 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nt4 ->
  NBET.embedding 'nt5 ->
  NBET.embedding 'nt6 ->
  NBET.embedding 'nt7 ->
  NBET.embedding 'nt8 ->
  NBET.embedding 'nt9 ->
  NBET.embedding 'nt10 ->
  NBET.embedding 'nt11 ->
  NBET.embedding 'nt12 ->
  NBET.embedding 'nt13 ->
  NBET.embedding 'nt14 ->
  NBET.embedding 'nt15 ->
  NBET.embedding 'nt16 ->
  NBET.embedding 'nt17 ->
  NBET.embedding 'nt18 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_tac_step_19 :
  int ->
  string ->
  ('t1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 't8 -> 't9 -> 't10 -> 't11 -> 't12 -> 't13 -> 't14 -> 't15 -> 't16 -> 't17 -> 't18 -> 't19 -> tac 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  embedding 't4 ->
  embedding 't5 ->
  embedding 't6 ->
  embedding 't7 ->
  embedding 't8 ->
  embedding 't9 ->
  embedding 't10 ->
  embedding 't11 ->
  embedding 't12 ->
  embedding 't13 ->
  embedding 't14 ->
  embedding 't15 ->
  embedding 't16 ->
  embedding 't17 ->
  embedding 't18 ->
  embedding 't19 ->
  er:embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> 'nt4 -> 'nt5 -> 'nt6 -> 'nt7 -> 'nt8 -> 'nt9 -> 'nt10 -> 'nt11 -> 'nt12 -> 'nt13 -> 'nt14 -> 'nt15 -> 'nt16 -> 'nt17 -> 'nt18 -> 'nt19 -> tac 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nt4 ->
  NBET.embedding 'nt5 ->
  NBET.embedding 'nt6 ->
  NBET.embedding 'nt7 ->
  NBET.embedding 'nt8 ->
  NBET.embedding 'nt9 ->
  NBET.embedding 'nt10 ->
  NBET.embedding 'nt11 ->
  NBET.embedding 'nt12 ->
  NBET.embedding 'nt13 ->
  NBET.embedding 'nt14 ->
  NBET.embedding 'nt15 ->
  NBET.embedding 'nt16 ->
  NBET.embedding 'nt17 ->
  NBET.embedding 'nt18 ->
  NBET.embedding 'nt19 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_tac_step_20 :
  int ->
  string ->
  ('t1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 't8 -> 't9 -> 't10 -> 't11 -> 't12 -> 't13 -> 't14 -> 't15 -> 't16 -> 't17 -> 't18 -> 't19 -> 't20 -> tac 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  embedding 't4 ->
  embedding 't5 ->
  embedding 't6 ->
  embedding 't7 ->
  embedding 't8 ->
  embedding 't9 ->
  embedding 't10 ->
  embedding 't11 ->
  embedding 't12 ->
  embedding 't13 ->
  embedding 't14 ->
  embedding 't15 ->
  embedding 't16 ->
  embedding 't17 ->
  embedding 't18 ->
  embedding 't19 ->
  embedding 't20 ->
  er:embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> 'nt4 -> 'nt5 -> 'nt6 -> 'nt7 -> 'nt8 -> 'nt9 -> 'nt10 -> 'nt11 -> 'nt12 -> 'nt13 -> 'nt14 -> 'nt15 -> 'nt16 -> 'nt17 -> 'nt18 -> 'nt19 -> 'nt20 -> tac 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nt4 ->
  NBET.embedding 'nt5 ->
  NBET.embedding 'nt6 ->
  NBET.embedding 'nt7 ->
  NBET.embedding 'nt8 ->
  NBET.embedding 'nt9 ->
  NBET.embedding 'nt10 ->
  NBET.embedding 'nt11 ->
  NBET.embedding 'nt12 ->
  NBET.embedding 'nt13 ->
  NBET.embedding 'nt14 ->
  NBET.embedding 'nt15 ->
  NBET.embedding 'nt16 ->
  NBET.embedding 'nt17 ->
  NBET.embedding 'nt18 ->
  NBET.embedding 'nt19 ->
  NBET.embedding 'nt20 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_total_step_1 :
  int ->
  string ->
  ('t1 -> 'r) ->
  embedding 't1 ->
  embedding 'r ->
  ('nt1 -> 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_total_step_2 :
  int ->
  string ->
  ('t1 -> 't2 -> 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 'r ->
  ('nt1 -> 'nt2 -> 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_total_step_3 :
  int ->
  string ->
  ('t1 -> 't2 -> 't3 -> 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_total_step_4 :
  int ->
  string ->
  ('t1 -> 't2 -> 't3 -> 't4 -> 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  embedding 't4 ->
  embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> 'nt4 -> 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nt4 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_total_step_5 :
  int ->
  string ->
  ('t1 -> 't2 -> 't3 -> 't4 -> 't5 -> 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  embedding 't4 ->
  embedding 't5 ->
  embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> 'nt4 -> 'nt5 -> 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nt4 ->
  NBET.embedding 'nt5 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_total_step_6 :
  int ->
  string ->
  ('t1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  embedding 't4 ->
  embedding 't5 ->
  embedding 't6 ->
  embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> 'nt4 -> 'nt5 -> 'nt6 -> 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nt4 ->
  NBET.embedding 'nt5 ->
  NBET.embedding 'nt6 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_total_step_7 :
  int ->
  string ->
  ('t1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  embedding 't4 ->
  embedding 't5 ->
  embedding 't6 ->
  embedding 't7 ->
  embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> 'nt4 -> 'nt5 -> 'nt6 -> 'nt7 -> 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nt4 ->
  NBET.embedding 'nt5 ->
  NBET.embedding 'nt6 ->
  NBET.embedding 'nt7 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_total_step_8 :
  int ->
  string ->
  ('t1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 't8 -> 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  embedding 't4 ->
  embedding 't5 ->
  embedding 't6 ->
  embedding 't7 ->
  embedding 't8 ->
  embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> 'nt4 -> 'nt5 -> 'nt6 -> 'nt7 -> 'nt8 -> 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nt4 ->
  NBET.embedding 'nt5 ->
  NBET.embedding 'nt6 ->
  NBET.embedding 'nt7 ->
  NBET.embedding 'nt8 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_total_step_9 :
  int ->
  string ->
  ('t1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 't8 -> 't9 -> 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  embedding 't4 ->
  embedding 't5 ->
  embedding 't6 ->
  embedding 't7 ->
  embedding 't8 ->
  embedding 't9 ->
  embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> 'nt4 -> 'nt5 -> 'nt6 -> 'nt7 -> 'nt8 -> 'nt9 -> 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nt4 ->
  NBET.embedding 'nt5 ->
  NBET.embedding 'nt6 ->
  NBET.embedding 'nt7 ->
  NBET.embedding 'nt8 ->
  NBET.embedding 'nt9 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_total_step_10 :
  int ->
  string ->
  ('t1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 't8 -> 't9 -> 't10 -> 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  embedding 't4 ->
  embedding 't5 ->
  embedding 't6 ->
  embedding 't7 ->
  embedding 't8 ->
  embedding 't9 ->
  embedding 't10 ->
  embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> 'nt4 -> 'nt5 -> 'nt6 -> 'nt7 -> 'nt8 -> 'nt9 -> 'nt10 -> 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nt4 ->
  NBET.embedding 'nt5 ->
  NBET.embedding 'nt6 ->
  NBET.embedding 'nt7 ->
  NBET.embedding 'nt8 ->
  NBET.embedding 'nt9 ->
  NBET.embedding 'nt10 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_total_step_11 :
  int ->
  string ->
  ('t1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 't8 -> 't9 -> 't10 -> 't11 -> 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  embedding 't4 ->
  embedding 't5 ->
  embedding 't6 ->
  embedding 't7 ->
  embedding 't8 ->
  embedding 't9 ->
  embedding 't10 ->
  embedding 't11 ->
  embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> 'nt4 -> 'nt5 -> 'nt6 -> 'nt7 -> 'nt8 -> 'nt9 -> 'nt10 -> 'nt11 -> 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nt4 ->
  NBET.embedding 'nt5 ->
  NBET.embedding 'nt6 ->
  NBET.embedding 'nt7 ->
  NBET.embedding 'nt8 ->
  NBET.embedding 'nt9 ->
  NBET.embedding 'nt10 ->
  NBET.embedding 'nt11 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_total_step_12 :
  int ->
  string ->
  ('t1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 't8 -> 't9 -> 't10 -> 't11 -> 't12 -> 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  embedding 't4 ->
  embedding 't5 ->
  embedding 't6 ->
  embedding 't7 ->
  embedding 't8 ->
  embedding 't9 ->
  embedding 't10 ->
  embedding 't11 ->
  embedding 't12 ->
  embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> 'nt4 -> 'nt5 -> 'nt6 -> 'nt7 -> 'nt8 -> 'nt9 -> 'nt10 -> 'nt11 -> 'nt12 -> 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nt4 ->
  NBET.embedding 'nt5 ->
  NBET.embedding 'nt6 ->
  NBET.embedding 'nt7 ->
  NBET.embedding 'nt8 ->
  NBET.embedding 'nt9 ->
  NBET.embedding 'nt10 ->
  NBET.embedding 'nt11 ->
  NBET.embedding 'nt12 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_total_step_13 :
  int ->
  string ->
  ('t1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 't8 -> 't9 -> 't10 -> 't11 -> 't12 -> 't13 -> 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  embedding 't4 ->
  embedding 't5 ->
  embedding 't6 ->
  embedding 't7 ->
  embedding 't8 ->
  embedding 't9 ->
  embedding 't10 ->
  embedding 't11 ->
  embedding 't12 ->
  embedding 't13 ->
  embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> 'nt4 -> 'nt5 -> 'nt6 -> 'nt7 -> 'nt8 -> 'nt9 -> 'nt10 -> 'nt11 -> 'nt12 -> 'nt13 -> 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nt4 ->
  NBET.embedding 'nt5 ->
  NBET.embedding 'nt6 ->
  NBET.embedding 'nt7 ->
  NBET.embedding 'nt8 ->
  NBET.embedding 'nt9 ->
  NBET.embedding 'nt10 ->
  NBET.embedding 'nt11 ->
  NBET.embedding 'nt12 ->
  NBET.embedding 'nt13 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_total_step_14 :
  int ->
  string ->
  ('t1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 't8 -> 't9 -> 't10 -> 't11 -> 't12 -> 't13 -> 't14 -> 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  embedding 't4 ->
  embedding 't5 ->
  embedding 't6 ->
  embedding 't7 ->
  embedding 't8 ->
  embedding 't9 ->
  embedding 't10 ->
  embedding 't11 ->
  embedding 't12 ->
  embedding 't13 ->
  embedding 't14 ->
  embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> 'nt4 -> 'nt5 -> 'nt6 -> 'nt7 -> 'nt8 -> 'nt9 -> 'nt10 -> 'nt11 -> 'nt12 -> 'nt13 -> 'nt14 -> 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nt4 ->
  NBET.embedding 'nt5 ->
  NBET.embedding 'nt6 ->
  NBET.embedding 'nt7 ->
  NBET.embedding 'nt8 ->
  NBET.embedding 'nt9 ->
  NBET.embedding 'nt10 ->
  NBET.embedding 'nt11 ->
  NBET.embedding 'nt12 ->
  NBET.embedding 'nt13 ->
  NBET.embedding 'nt14 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_total_step_15 :
  int ->
  string ->
  ('t1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 't8 -> 't9 -> 't10 -> 't11 -> 't12 -> 't13 -> 't14 -> 't15 -> 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  embedding 't4 ->
  embedding 't5 ->
  embedding 't6 ->
  embedding 't7 ->
  embedding 't8 ->
  embedding 't9 ->
  embedding 't10 ->
  embedding 't11 ->
  embedding 't12 ->
  embedding 't13 ->
  embedding 't14 ->
  embedding 't15 ->
  embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> 'nt4 -> 'nt5 -> 'nt6 -> 'nt7 -> 'nt8 -> 'nt9 -> 'nt10 -> 'nt11 -> 'nt12 -> 'nt13 -> 'nt14 -> 'nt15 -> 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nt4 ->
  NBET.embedding 'nt5 ->
  NBET.embedding 'nt6 ->
  NBET.embedding 'nt7 ->
  NBET.embedding 'nt8 ->
  NBET.embedding 'nt9 ->
  NBET.embedding 'nt10 ->
  NBET.embedding 'nt11 ->
  NBET.embedding 'nt12 ->
  NBET.embedding 'nt13 ->
  NBET.embedding 'nt14 ->
  NBET.embedding 'nt15 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_total_step_16 :
  int ->
  string ->
  ('t1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 't8 -> 't9 -> 't10 -> 't11 -> 't12 -> 't13 -> 't14 -> 't15 -> 't16 -> 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  embedding 't4 ->
  embedding 't5 ->
  embedding 't6 ->
  embedding 't7 ->
  embedding 't8 ->
  embedding 't9 ->
  embedding 't10 ->
  embedding 't11 ->
  embedding 't12 ->
  embedding 't13 ->
  embedding 't14 ->
  embedding 't15 ->
  embedding 't16 ->
  embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> 'nt4 -> 'nt5 -> 'nt6 -> 'nt7 -> 'nt8 -> 'nt9 -> 'nt10 -> 'nt11 -> 'nt12 -> 'nt13 -> 'nt14 -> 'nt15 -> 'nt16 -> 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nt4 ->
  NBET.embedding 'nt5 ->
  NBET.embedding 'nt6 ->
  NBET.embedding 'nt7 ->
  NBET.embedding 'nt8 ->
  NBET.embedding 'nt9 ->
  NBET.embedding 'nt10 ->
  NBET.embedding 'nt11 ->
  NBET.embedding 'nt12 ->
  NBET.embedding 'nt13 ->
  NBET.embedding 'nt14 ->
  NBET.embedding 'nt15 ->
  NBET.embedding 'nt16 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_total_step_17 :
  int ->
  string ->
  ('t1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 't8 -> 't9 -> 't10 -> 't11 -> 't12 -> 't13 -> 't14 -> 't15 -> 't16 -> 't17 -> 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  embedding 't4 ->
  embedding 't5 ->
  embedding 't6 ->
  embedding 't7 ->
  embedding 't8 ->
  embedding 't9 ->
  embedding 't10 ->
  embedding 't11 ->
  embedding 't12 ->
  embedding 't13 ->
  embedding 't14 ->
  embedding 't15 ->
  embedding 't16 ->
  embedding 't17 ->
  embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> 'nt4 -> 'nt5 -> 'nt6 -> 'nt7 -> 'nt8 -> 'nt9 -> 'nt10 -> 'nt11 -> 'nt12 -> 'nt13 -> 'nt14 -> 'nt15 -> 'nt16 -> 'nt17 -> 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nt4 ->
  NBET.embedding 'nt5 ->
  NBET.embedding 'nt6 ->
  NBET.embedding 'nt7 ->
  NBET.embedding 'nt8 ->
  NBET.embedding 'nt9 ->
  NBET.embedding 'nt10 ->
  NBET.embedding 'nt11 ->
  NBET.embedding 'nt12 ->
  NBET.embedding 'nt13 ->
  NBET.embedding 'nt14 ->
  NBET.embedding 'nt15 ->
  NBET.embedding 'nt16 ->
  NBET.embedding 'nt17 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_total_step_18 :
  int ->
  string ->
  ('t1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 't8 -> 't9 -> 't10 -> 't11 -> 't12 -> 't13 -> 't14 -> 't15 -> 't16 -> 't17 -> 't18 -> 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  embedding 't4 ->
  embedding 't5 ->
  embedding 't6 ->
  embedding 't7 ->
  embedding 't8 ->
  embedding 't9 ->
  embedding 't10 ->
  embedding 't11 ->
  embedding 't12 ->
  embedding 't13 ->
  embedding 't14 ->
  embedding 't15 ->
  embedding 't16 ->
  embedding 't17 ->
  embedding 't18 ->
  embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> 'nt4 -> 'nt5 -> 'nt6 -> 'nt7 -> 'nt8 -> 'nt9 -> 'nt10 -> 'nt11 -> 'nt12 -> 'nt13 -> 'nt14 -> 'nt15 -> 'nt16 -> 'nt17 -> 'nt18 -> 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nt4 ->
  NBET.embedding 'nt5 ->
  NBET.embedding 'nt6 ->
  NBET.embedding 'nt7 ->
  NBET.embedding 'nt8 ->
  NBET.embedding 'nt9 ->
  NBET.embedding 'nt10 ->
  NBET.embedding 'nt11 ->
  NBET.embedding 'nt12 ->
  NBET.embedding 'nt13 ->
  NBET.embedding 'nt14 ->
  NBET.embedding 'nt15 ->
  NBET.embedding 'nt16 ->
  NBET.embedding 'nt17 ->
  NBET.embedding 'nt18 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_total_step_19 :
  int ->
  string ->
  ('t1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 't8 -> 't9 -> 't10 -> 't11 -> 't12 -> 't13 -> 't14 -> 't15 -> 't16 -> 't17 -> 't18 -> 't19 -> 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  embedding 't4 ->
  embedding 't5 ->
  embedding 't6 ->
  embedding 't7 ->
  embedding 't8 ->
  embedding 't9 ->
  embedding 't10 ->
  embedding 't11 ->
  embedding 't12 ->
  embedding 't13 ->
  embedding 't14 ->
  embedding 't15 ->
  embedding 't16 ->
  embedding 't17 ->
  embedding 't18 ->
  embedding 't19 ->
  embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> 'nt4 -> 'nt5 -> 'nt6 -> 'nt7 -> 'nt8 -> 'nt9 -> 'nt10 -> 'nt11 -> 'nt12 -> 'nt13 -> 'nt14 -> 'nt15 -> 'nt16 -> 'nt17 -> 'nt18 -> 'nt19 -> 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nt4 ->
  NBET.embedding 'nt5 ->
  NBET.embedding 'nt6 ->
  NBET.embedding 'nt7 ->
  NBET.embedding 'nt8 ->
  NBET.embedding 'nt9 ->
  NBET.embedding 'nt10 ->
  NBET.embedding 'nt11 ->
  NBET.embedding 'nt12 ->
  NBET.embedding 'nt13 ->
  NBET.embedding 'nt14 ->
  NBET.embedding 'nt15 ->
  NBET.embedding 'nt16 ->
  NBET.embedding 'nt17 ->
  NBET.embedding 'nt18 ->
  NBET.embedding 'nt19 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_total_step_20 :
  int ->
  string ->
  ('t1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 't8 -> 't9 -> 't10 -> 't11 -> 't12 -> 't13 -> 't14 -> 't15 -> 't16 -> 't17 -> 't18 -> 't19 -> 't20 -> 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  embedding 't4 ->
  embedding 't5 ->
  embedding 't6 ->
  embedding 't7 ->
  embedding 't8 ->
  embedding 't9 ->
  embedding 't10 ->
  embedding 't11 ->
  embedding 't12 ->
  embedding 't13 ->
  embedding 't14 ->
  embedding 't15 ->
  embedding 't16 ->
  embedding 't17 ->
  embedding 't18 ->
  embedding 't19 ->
  embedding 't20 ->
  embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> 'nt4 -> 'nt5 -> 'nt6 -> 'nt7 -> 'nt8 -> 'nt9 -> 'nt10 -> 'nt11 -> 'nt12 -> 'nt13 -> 'nt14 -> 'nt15 -> 'nt16 -> 'nt17 -> 'nt18 -> 'nt19 -> 'nt20 -> 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nt4 ->
  NBET.embedding 'nt5 ->
  NBET.embedding 'nt6 ->
  NBET.embedding 'nt7 ->
  NBET.embedding 'nt8 ->
  NBET.embedding 'nt9 ->
  NBET.embedding 'nt10 ->
  NBET.embedding 'nt11 ->
  NBET.embedding 'nt12 ->
  NBET.embedding 'nt13 ->
  NBET.embedding 'nt14 ->
  NBET.embedding 'nt15 ->
  NBET.embedding 'nt16 ->
  NBET.embedding 'nt17 ->
  NBET.embedding 'nt18 ->
  NBET.embedding 'nt19 ->
  NBET.embedding 'nt20 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

(***** Refl typing *****)

val mk_refl_typing_step_2 :
  string ->
  ('t1 -> 't2 -> tac 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  er:embedding 'r ->
  ('nt1 -> 'nt2 -> tac 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step

val mk_refl_typing_step_3 :
  string ->
  ('t1 -> 't2 -> 't3 -> tac 'r) ->
  embedding 't1 ->
  embedding 't2 ->
  embedding 't3 ->
  er:embedding 'r ->
  ('nt1 -> 'nt2 -> 'nt3 -> tac 'nr) ->
  NBET.embedding 'nt1 ->
  NBET.embedding 'nt2 ->
  NBET.embedding 'nt3 ->
  NBET.embedding 'nr ->
  Cfg.primitive_step
