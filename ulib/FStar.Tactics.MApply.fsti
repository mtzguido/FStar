module FStar.Tactics.MApply

open FStar.Reflection.V2
open FStar.Tactics.Effect
open FStar.Tactics.Typeclasses
open FStar.Tactics.V2.SyntaxCoercions

include FStar.Tactics.MApply0

class termable (a : Type) = {
  to_term : a -> Tac term
}

instance termable_term : termable term = {
  to_term = (fun t -> t);
}

instance termable_binding : termable binding = {
  to_term = (fun b -> binding_to_term b);
}

let mapply (#ty:Type) {| termable ty |} (x : ty) : Tac unit =
  let t = to_term x in
  mapply0 t
