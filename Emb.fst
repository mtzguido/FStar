module Emb

[@@plugin]
type t =
  | A of int
  | B of int & bool
  | C : int -> string -> t

[@@plugin]
let flip (x:t) : t =
  match x with
  | A x -> C x ""
  | B (i, b) -> B (-i, not b)
  | C x _ -> A x
