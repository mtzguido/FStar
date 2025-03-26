module PrettifyType

open FStar.Tactics.PrettifyType { entry }
open FStar.Bijection

(* Note the parenthesis, we don't yet support tuple3, 4, etc. *)
type myty =
  either ((int & int) & (int & string)) bool

%splice[
  myty_pretty;
  myty_pretty_left;
  myty_pretty_right;
  Mkmyty_pretty0;
  Mkmyty_pretty1
] (entry "_pretty" (`%myty))

(* Sanity check *)
let right (x : myty) : r:myty_pretty{myty_pretty_right x == r} =
  match x with
  | Inl ((x, y), (z, s)) -> Mkmyty_pretty0 x y z s
  | Inr b -> Mkmyty_pretty1 b

let left (x : myty_pretty) : r:myty{myty_pretty_left x == r} =
  match x with
  | Mkmyty_pretty0 x y z s -> Inl ((x, y), (z, s))
  | Mkmyty_pretty1 b -> Inr b

type t2 = tuple2 int int
%splice[t2_pretty] (entry "_pretty" (`%t2))

type t3 = tuple2 int (either bool string)
%splice[t3_pretty] (entry "_pretty" (`%t3))

type t4 = either t3 (tuple2 int (either bool string))
%splice[t4_pretty; t4_pretty_left_right] (entry "_pretty" (`%t4))

let inv (x:t4) = t4_pretty_left_right x

type t5 =
  either (int -> int) <|
  either int <|
  string

[@@1]
noextract
noeq (* will only go to the generated type. *)
unfold
%splice[t5_quals; t5_quals_left_right] (entry "_quals" (`%t5))

type big =
  either int <|
  either int <|
  either int <|
  either int <|
  either int <|
  either int <|
  either int <|
  either int <|
  either int <|
  either int <|
  either int <|
  string

%splice[] (entry "_pretty" (`%big))
