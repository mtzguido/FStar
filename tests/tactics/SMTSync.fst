module SMTSync

open FStar.Tactics

let test1 x = assert (1 + x == x + 1)
  by (try smt_sync () with |_ -> fail "")

[@@expect_failure]
let test2 x = assert (1 + x == x + 2)
  by (try smt_sync () with |_ -> fail "")

assume val badlem : x:int -> Lemma (1 + x == x + 2)

let test3 x = assert (1 + x == x + 2)
  by (try smt_sync () with |_ -> mapply (`badlem))

let test4 x = assert (let rec f x = 42 in f 10 == 42)
  by (try smt_sync () with |_ -> compute ())
