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

module FStar.Compiler.Plugins

open FStar.Compiler.Effect
open FStar.Compiler.Plugins.Base
module BU = FStar.Compiler.Util

(* Tries to load a plugin named like the extension. Returns true
if it could find a plugin with the proper name. This will fail hard
if loading the plugin fails. *)
let autoload_plugin (ext:string) : bool =
  if Debug.any () then
    BU.print1 "Trying to find a plugin for extension %s\n" ext;
  match Options.find_file (ext ^ ".cmxs") with
  | Some fn ->
    if Debug.any () then
      BU.print1 "Autoloading plugin %s ...\n" fn;
    Plugins.Base.load_tactics [fn];
    true
  | None ->
    false
