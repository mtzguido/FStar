(*
   Copyright 2008-2025 Nikhil Swamy and Microsoft Research

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
module FStarC.Stats

open FStarC.Effect

type stat = {
  ns_point : int;
  ns_tree  : int;
  ncalls   : int;
}

let zero : stat = { ns_point = 0; ns_tree = 0; ncalls = 0 }

(* nanoseconds, ncalls *)
let st : SMap.t stat = SMap.create 10

let stack : ref (list string) = mk_ref []

let add k ns_point ns_tree calls =
  let s =
    match SMap.try_find st k with
    | None -> zero
    | Some r -> r
  in
  let s' = { s with ns_point = s.ns_point + ns_point; ns_tree = s.ns_tree + ns_tree; ncalls = s.ncalls + calls } in
  SMap.add st k s'

let record
  (key : string)
  (f : unit -> 'a)
  : 'a
= stack := key :: !stack;
  let r, ns = Timing.record_ns f in
  stack := List.tl !stack;
  (* Take time out of the parent. *)
  begin match !stack with
  | [] -> ()
  | k_par::_ -> add k_par (-ns) 0 0
  end;
  add key ns ns 1;
  r

let lpad (len:int) (s:string) : string =
  let l = String.length s in
  if l >= len then s else String.make (len - l) ' ' ^ s

let print_all () : string =
  let keys = SMap.keys st in
  let pr1 (k:string) : string =
    let st = Some?.v (SMap.try_find st k) in
    Util.format4 "  %s: %s %s ms %s ms"
      (lpad 30 k)
      (lpad 6 (string_of_int st.ncalls))
      (lpad 6 (string_of_int (st.ns_tree / 1000000)))
      (lpad 6 (string_of_int (st.ns_point / 1000000)))
  in
  let keys = Class.Ord.sort keys in
  Util.format4 "  %s %s %s %s" (lpad 31 "key") (lpad 6 "calls") (lpad 9 "tree") (lpad 9 "point") ^ "\n" ^
  (keys |> List.map pr1 |> String.concat "\n")
