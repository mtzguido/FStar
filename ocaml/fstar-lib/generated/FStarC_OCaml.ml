open Prims
let (shellescape : Prims.string -> Prims.string) =
  fun s ->
    let uu___ =
      let uu___1 = FStarC_Compiler_String.list_of_string s in
      FStarC_Compiler_List.map
        (fun uu___2 ->
           match uu___2 with
           | 39 -> "'\"'\"'"
           | c -> FStarC_Compiler_String.make Prims.int_one c) uu___1 in
    FStarC_Compiler_String.concat "" uu___
let (new_ocamlpath : unit -> Prims.string) =
  fun uu___ ->
    let ocamldir = FStarC_Find.locate_ocaml () in
    let old_ocamlpath =
      let uu___1 =
        FStarC_Compiler_Util.expand_environment_variable "OCAMLPATH" in
      FStarC_Compiler_Util.dflt "" uu___1 in
    let new_ocamlpath1 =
      Prims.strcat ocamldir (Prims.strcat ":" old_ocamlpath) in
    new_ocamlpath1
let exec_in_ocamlenv : 'a . Prims.string -> Prims.string Prims.list -> 'a =
  fun cmd ->
    fun args ->
      let new_ocamlpath1 = new_ocamlpath () in
      if FStarC_Platform.system = FStarC_Platform.Windows
      then
        (let uu___1 =
           let uu___2 =
             FStarC_Errors_Msg.text
               "--ocamlenv is not supported on Windows (yet?)" in
           [uu___2] in
         FStarC_Errors.raise_error0
           FStarC_Errors_Codes.Fatal_OptionsNotCompatible ()
           (Obj.magic FStarC_Errors_Msg.is_error_message_list_doc)
           (Obj.magic uu___1))
      else ();
      FStarC_Compiler_Util.putenv "OCAMLPATH" new_ocamlpath1;
      FStarC_Compiler_Util.execvp cmd (cmd :: args);
      failwith "execvp failed"
let exec_ocamlc : 'a . Prims.string Prims.list -> 'a =
  fun args ->
    exec_in_ocamlenv "ocamlfind" ("c" :: "-w" :: "-8" :: "-linkpkg" ::
      "-package" :: "fstar.lib" :: args)
let exec_ocamlopt : 'a . Prims.string Prims.list -> 'a =
  fun args ->
    exec_in_ocamlenv "ocamlfind" ("opt" :: "-w" :: "-8" :: "-linkpkg" ::
      "-package" :: "fstar.lib" :: args)
let exec_ocamlopt_plugin : 'a . Prims.string Prims.list -> 'a =
  fun args ->
    exec_in_ocamlenv "ocamlfind" ("opt" :: "-w" :: "-8" :: "-shared" ::
      "-package" :: "fstar.lib" :: args)