module WhnfReadback

(* The environment shares p0 ... p8. A deep readback visits each previous
   value three times, even though every second field is discarded by the
   final projector. The resulting function should be the identity. *)
noeq type funcs (a:Type) =
  | Pack : first:(a -> Tot a) -> second:(a -> Tot a) -> funcs a

let nested (#a:Type) (f:a -> Tot a) (x:a) : a =
  [@@inline_let] let p0 = Pack (fun y -> y) f in
  [@@inline_let] let p1 =
    Pack (Pack?.first p0) (fun y -> Pack?.second p0 (Pack?.second p0 y)) in
  [@@inline_let] let p2 =
    Pack (Pack?.first p1) (fun y -> Pack?.second p1 (Pack?.second p1 y)) in
  [@@inline_let] let p3 =
    Pack (Pack?.first p2) (fun y -> Pack?.second p2 (Pack?.second p2 y)) in
  [@@inline_let] let p4 =
    Pack (Pack?.first p3) (fun y -> Pack?.second p3 (Pack?.second p3 y)) in
  [@@inline_let] let p5 =
    Pack (Pack?.first p4) (fun y -> Pack?.second p4 (Pack?.second p4 y)) in
  [@@inline_let] let p6 =
    Pack (Pack?.first p5) (fun y -> Pack?.second p5 (Pack?.second p5 y)) in
  [@@inline_let] let p7 =
    Pack (Pack?.first p6) (fun y -> Pack?.second p6 (Pack?.second p6 y)) in
  [@@inline_let] let p8 =
    Pack (Pack?.first p7) (fun y -> Pack?.second p7 (Pack?.second p7 y)) in
  Pack?.first p8 x

(* A stuck, over-applied projector must retain its argument. *)
let stuck (#a:Type) (p:funcs a) (x:a) : a = Pack?.first p x

(* Exercise closure readback underneath a binder. *)
let under_binder (#a:Type) (f:a -> Tot a) : a -> Tot a =
  [@@inline_let] let p = Pack (fun y -> y) f in
  fun x -> Pack?.first p x
