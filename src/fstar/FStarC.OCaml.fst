(*
   Copyright 2008-2016 Nikhil Swamy and Microsoft Research

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
module FStarC.OCaml

open FStarC
open FStarC.Compiler
open FStarC.Compiler.Effect

let shellescape (s:string) : string =
  String.list_of_string s |>
  List.map (function
    | '\'' -> "'\"'\"'" // to escape single quotes we need to put them inside a double quote
    | c -> String.make 1 c
  ) |>
  String.concat ""

let new_ocamlpath () : string =
  let ocamldir = Find.locate_ocaml () in
  let old_ocamlpath = Util.dflt "" (Util.expand_environment_variable "OCAMLPATH") in
  let new_ocamlpath = ocamldir ^ ":" ^ old_ocamlpath in
  new_ocamlpath

let exec_in_ocamlenv #a (cmd : string) (args : list string) : a =
  let new_ocamlpath = new_ocamlpath () in
  if Platform.system = Platform.Windows then (
    Errors.raise_error0 Errors.Fatal_OptionsNotCompatible [
      Errors.text "--ocamlenv is not supported on Windows (yet?)"
    ]
  );
  (* Update OCAMLPATH and run (exec) the command *)
  Util.putenv "OCAMLPATH" new_ocamlpath;
  Util.execvp cmd (cmd :: args);
  failwith "execvp failed"

(* OCaml Warning 8: this pattern-matching is not exhaustive.
This is usually benign as we check for exhaustivenss via SMT. *)

let exec_ocamlc args =
  exec_in_ocamlenv "ocamlfind"
    ("c" :: "-w" :: "-8" :: "-linkpkg" :: "-package" :: "fstar.lib" :: args)

let exec_ocamlopt args =
  exec_in_ocamlenv "ocamlfind"
    ("opt" :: "-w" :: "-8" :: "-linkpkg" :: "-package" :: "fstar.lib" :: args)

let exec_ocamlopt_plugin args =
  exec_in_ocamlenv "ocamlfind"
    ("opt" :: "-w" :: "-8" :: "-shared" :: "-package" :: "fstar.lib" ::
    args)