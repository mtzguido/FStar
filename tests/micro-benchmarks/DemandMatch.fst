module DemandMatch

noeq type box (a:Type) = | Box : value:a -> tag:int -> box a

let make_box #a (x:a) : box a = Box x 17

(* Universes, captured field environments, and the caller's environment. *)
let nested #a (x:a) : a =
  match make_box (Some x) with
  | Box (Some y) _ -> y
  | Box None _ -> x
let polymorphic #a (x:a) = assert_norm (nested x == x)

let apply_field (captured caller:int) : int =
  match make_box (Some (fun x -> captured + x)) with
  | Box (Some f) _ -> f caller
  | Box None _ -> 0
let _ = assert_norm (apply_field 40 2 == 42)
let _ = assert_norm ((fun x -> apply_field x 2) == (fun x -> x + 2))

(* Failed branches share the same nested scrutinee. *)
let choose b =
  match b with
  | Box (Some 0) _ -> 10
  | Box (Some 1) _ -> 20
  | Box (Some x) _ -> x
  | Box None _ -> 30
let _ = assert_norm (choose (Box (Some 42) 7) == 42)
let _ = assert_norm (choose (Box None 7) == 30)

(* The whole value must remain available after a failed nested pattern. *)
let whole b =
  match b with
  | Box (Some 0) _ -> b
  | x -> x
let _ = assert_norm (whole (Box (Some 42) 7) == Box (Some 42) 7)

(* Stuck nested tests must not skip to a later wildcard. *)
let blocked (x:option int) =
  match Box x 7 with
  | Box (Some y) _ -> y
  | _ -> 0
let blocked_equivalence (x:option int) =
  assert_norm (blocked x == (match Box x 7 with Box (Some y) _ -> y | _ -> 0))

(* Pattern-variable order across several levels. *)
let order b =
  match b with
  | Box (Some (x, y)) z -> (x, y, z)
  | Box None z -> (0, 0, z)
let _ = assert_norm (order (Box (Some (11, 22)) 33) == (11, 22, 33))

(* Recursive consumers exercise sharing through successive matches. *)
let rec length #a (xs:list a) : nat =
  match xs with [] -> 0 | _::tl -> 1 + length tl
let _ = assert_norm (length [1;2;3;4;5] == 5)

(* A wildcard may discard a total scrutinee even when its head is open. *)
let ignore_box #a (b:box a) = match b with _ -> 42
let wildcard_result #a (b:box a) = assert_norm (ignore_box b == 42)
