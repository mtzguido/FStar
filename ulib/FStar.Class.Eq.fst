(*
   Copyright 2008-2023 Microsoft Research

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
module FStar.Class.Eq

(* A class for decidable equality *)
class deq a = {
  eq    : a -> a -> bool;
}

let eq_instance_of_eqtype (#a:eqtype) : deq a = {
  eq = (fun x y -> x = y )
}

// CHECK
instance eqtype_has_eq (a:eqtype) : deq a = eq_instance_of_eqtype

let rec eqList #a {| deq a |} (xs ys : list a) : Tot bool =
  match xs, ys with
  | [], [] -> true
  | x::xs, y::ys -> eq x y && eqList xs ys
  | _, _ -> false

instance eq_list (_ : deq 'a) : deq (list 'a) = {
  eq = eqList;
}

instance eq_pair (_ : deq 'a) (_ : deq 'b) : deq ('a * 'b) = {
  eq = (fun (a,b) (c,d) -> eq a c && eq b d)
}

instance eq_option (_ : deq 'a) : deq (option 'a) = {
  eq = (fun o1 o2 ->
    match o1, o2 with
    | None, None -> true
    | Some x, Some y -> eq x y
    | _, _ -> false);
}

(* Redefine for clients *)
val (=) : #a:Type -> {| deq a |} -> a -> a -> bool
let (=) = eq
