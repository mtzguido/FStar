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
module FStarC.Main
open FStarC
open FStarC.Compiler.Effect
open FStarC.Compiler.List
open FStarC.Compiler.Util
open FStarC.Getopt
open FStarC.Ident
open FStarC.CheckedFiles
open FStarC.Universal
open FStarC.Compiler

open FStarC.Class.Show

module E = FStarC.Errors
module UF = FStarC.Syntax.Unionfind
module RE = FStarC.Reflection.V2.Embeddings

let _ = Version.dummy ()

(* These modules only mentioned to put them in the dep graph
and hence compile and link them in. They do not export anything,
instead they register primitive steps in the normalizer during
initialization. *)
open FStarC.Reflection.V1.Interpreter {}
open FStarC.Reflection.V2.Interpreter {}

(* process_args:  parses command line arguments, setting FStarC.Options *)
(*                returns an error status and list of filenames        *)
let process_args () : parse_cmdline_res & list string =
  Options.parse_cmd_line ()

(* cleanup: kills background Z3 processes; relevant when --n_cores > 1 *)
(* GM: unclear if it's useful now? *)
let cleanup () = Util.kill_all ()

(* printing a finished message *)
let finished_message fmods errs =
  let print_to = if errs > 0 then Util.print_error else Util.print_string in
  if not (Options.silent()) then begin
    fmods |> List.iter (fun (iface, name) ->
                let tag = if iface then "i'face (or impl+i'face)" else "module" in
                if Options.should_print_message (string_of_lid name)
                then print_to (Util.format2 "Verified %s: %s\n" tag (Ident.string_of_lid name)));
    if errs > 0
    then if errs = 1
         then Util.print_error "1 error was reported (see above)\n"
         else Util.print1_error "%s errors were reported (see above)\n" (string_of_int errs)
    else print1 "%s\n" (Util.colorize_bold "All verification conditions discharged successfully")
  end

(* printing total error count *)
let report_errors fmods =
  FStarC.Errors.report_all () |> ignore;
  let nerrs = FStarC.Errors.get_err_count() in
  if nerrs > 0 then begin
    finished_message fmods nerrs;
    exit 1
  end

let load_native_tactics () =
    let modules_to_load = Options.load() |> List.map Ident.lid_of_str in
    let cmxs_to_load = Options.load_cmxs () |> List.map Ident.lid_of_str in
    let ml_module_name m = FStarC.Extraction.ML.Util.ml_module_name_of_lid m in
    let ml_file m = ml_module_name m ^ ".ml" in
    let cmxs_file m =
        let cmxs = ml_module_name m ^ ".cmxs" in
        match Find.find_file cmxs with
        | Some f -> f
        | None ->
          if List.contains m cmxs_to_load  //if this module comes from the cmxs list, fail hard
          then E.raise_error0 E.Fatal_FailToCompileNativeTactic (Util.format1 "Could not find %s to load" cmxs)
          else  //else try to find and compile the ml file
            match Find.find_file (ml_file m) with
            | None ->
              E.raise_error0 E.Fatal_FailToCompileNativeTactic
                (Util.format1 "Failed to compile native tactic; extracted module %s not found" (ml_file m))
            | Some ml ->
              let dir = Util.dirname ml in
              Plugins.compile_modules dir [ml_module_name m];
              begin match Find.find_file cmxs with
                | None ->
                  E.raise_error0 E.Fatal_FailToCompileNativeTactic
                    (Util.format1 "Failed to compile native tactic; compiled object %s not found" cmxs)
                | Some f -> f
              end
    in
    let cmxs_files = (modules_to_load@cmxs_to_load) |> List.map cmxs_file in
    if Debug.any () then
      Util.print1 "Will try to load cmxs files: [%s]\n" (String.concat ", " cmxs_files);
    Plugins.load_plugins cmxs_files;
    iter_opt (Options.use_native_tactics ())
      Plugins.load_plugins_dir;
    ()


(* Need to keep names of input files for a second pass when prettyprinting *)
(* This reference is set once in `go` and read in `main` if the print or *)
(* print_in_place options are passed *)
let fstar_files: ref (option (list string)) = Util.mk_ref None

(* ocamlenv mode: called whenever the *first* argument is exactly '--ocamlenv' *)
let go_ocamlenv rest_args =
  if Platform.system = Platform.Windows then (
    Errors.raise_error0 Errors.Fatal_OptionsNotCompatible [
      Errors.text "--ocamlenv is not supported on Windows (yet?)"
    ]
  );
  let shellescape (s:string) : string =
    String.list_of_string s |>
    List.map (function
      | '\'' -> "'\"'\"'" // to escape single quotes we need to put them inside a double quote
      | c -> String.make 1 c
    ) |>
    String.concat ""
  in
  let ocamldir = Find.locate_ocaml () in
  let old_ocamlpath = Util.dflt "" (Util.expand_environment_variable "OCAMLPATH") in
  let new_ocamlpath = ocamldir ^ ":" ^ old_ocamlpath in
  match rest_args with
  | [] ->
    Util.print1 "OCAMLPATH='%s'; export OCAMLPATH;\n" (shellescape new_ocamlpath);
    exit 0

  | cmd :: args ->
    (* Update OCAMLPATH and run (exec) the command *)
    Util.putenv "OCAMLPATH" new_ocamlpath;
    Util.execvp cmd (cmd :: args)

(* This is used to print a backtrace when F* is interrupted by SIGINT *)
let set_error_trap () =
  let h = get_sigint_handler () in
  let h' s =
    let open FStarC.Pprint in
    let open FStarC.Errors.Msg in
    Debug.enable (); (* make sure diag is printed *)
    Options.set_option "error_contexts" (Options.Bool true);
    (* ^ Print context. Stack trace will be added since we have trace_error. *)
    Errors.diag Range.dummyRange [
      text "GOT SIGINT! Exiting";
    ];
    exit 1
  in
  set_sigint_handler (sigint_handler_f h')

(* Normal mode with some flags, files, etc *)
let go_normal () =
  let res, filenames = process_args () in
  if Options.trace_error () then set_error_trap ();
  match res with
    | Empty     -> Options.display_usage(); exit 1
    | Help      -> Options.display_usage(); exit 0
    | Error msg -> Util.print_error msg; exit 1

    | Success when Options.print_cache_version () ->
      Util.print1 "F* cache version number: %s\n"
                   (string_of_int FStarC.CheckedFiles.cache_version_number);
      exit 0

    (* --dep: Just compute and print the transitive dependency graph;
              don't verify anything *)
    | Success when Options.dep () <> None ->
      let _, deps = Parser.Dep.collect filenames FStarC.CheckedFiles.load_parsing_data_from_cache in
      Parser.Dep.print deps;
      report_errors []

    (* --print: Emit files in canonical source syntax *)
    | Success when Options.print () || Options.print_in_place () ->
      if not Platform.is_fstar_compiler_using_ocaml then
        failwith "You seem to be using the F#-generated version of the compiler ; \o
                   reindenting is not known to work yet with this version";
      let printing_mode =
        if Options.print ()
        then Prettyprint.FromTempToStdout
        else Prettyprint.FromTempToFile
      in
      Prettyprint.generate printing_mode filenames

    (* --read_checked: read and print a checked file *)
    | Success when Some? (Options.read_checked_file ()) -> (
      let path = Some?.v <| Options.read_checked_file () in
      let env = Universal.init_env Parser.Dep.empty_deps in
      let res = CheckedFiles.load_tc_result path in
      match res with
      | None ->
        let open FStarC.Pprint in
        Errors.raise_error0 Errors.Fatal_ModuleOrFileNotFound [
          Errors.Msg.text "Could not read checked file:" ^/^ doc_of_string path
        ]

      | Some (_, tcr) ->
        print1 "%s\n" (show tcr.checked_module)
    )

    (* --read_krml_file: read and print a krml file *)
    | Success when Some? (Options.read_krml_file ()) -> (
      let path = Some?.v <| Options.read_krml_file () in
      match load_value_from_file path <: option Extraction.Krml.binary_format with
      | None ->
        let open FStarC.Pprint in
        Errors.raise_error0 Errors.Fatal_ModuleOrFileNotFound [
          Errors.Msg.text "Could not read krml file:" ^/^ doc_of_string path
        ]
      | Some (version, files) ->
        print1 "Karamel format version: %s\n" (show version);
        (* Just "show decls" would print it, we just format this a bit *)
        files |> List.iter (fun (name, decls) ->
          print1 "%s:\n" name;
          decls |> List.iter (fun d -> print1 "  %s\n" (show d))
        )
    )

    (* --list_plugins: emit a list of plugins and exit *)
    | Success when Options.list_plugins () ->
      let ps = TypeChecker.Cfg.list_plugins () in
      let ts = Tactics.Interpreter.native_tactics_steps () in
      Util.print1 "Registered plugins:\n%s\n" (String.concat "\n" (List.map (fun p -> "  " ^ show p.TypeChecker.Primops.Base.name) ps));
      Util.print1 "Registered tactic plugins:\n%s\n" (String.concat "\n" (List.map (fun p -> "  " ^ show p.TypeChecker.Primops.Base.name) ts));
      ()

    (* --locate, --locate_lib, --locate_ocaml *)
    | Success when Options.locate () ->
      Util.print1 "%s\n" (Find.locate ());
      exit 0
    | Success when Options.locate_lib () -> (
      match Find.locate_lib () with
      | None ->
        Util.print_error "No library found (is --no_default_includes set?)\n";
        exit 1
      | Some s ->
        Util.print1 "%s\n" s;
        exit 0
    )
    | Success when Options.locate_ocaml () ->
      Util.print1 "%s\n" (Find.locate_ocaml ());
      exit 0

    (* either batch or interactive mode *)
    | Success ->
      fstar_files := Some filenames;

      if Debug.any () then (
        Util.print1 "- F* executable: %s\n" (Util.exec_name);
        Util.print1 "- Library root: %s\n" ((Util.dflt "<none>" (Find.lib_root ())));
        Util.print1 "- Full include path: %s\n" (show (Find.include_path ()));
        Util.print_string "\n";
        ()
      );

      (* Set the unionfind graph to read-only mode.
       * This will be unset by the typechecker and other pieces
       * of code that intend to use it. It helps us catch errors. *)
      UF.set_ro ();

      (* Try to load the plugins that are specified in the command line *)
      load_native_tactics ();

      (* --lsp: interactive mode for Language Server Protocol *)
      if Options.lsp_server () then
        Interactive.Lsp.start_server ()
      (* --ide, --in: Interactive mode *)
      else if Options.interactive () then begin
        UF.set_rw ();
        match filenames with
        | [] -> (* input validation: move to process args? *)
          Errors.log_issue0 Errors.Error_MissingFileName
            "--ide: Name of current file missing in command line invocation\n";
          exit 1
        | _ :: _ :: _ -> (* input validation: move to process args? *)
          Errors.log_issue0 Errors.Error_TooManyFiles
            "--ide: Too many files in command line invocation\n";
          exit 1
        | [filename] ->
          if Options.legacy_interactive () then
            Interactive.Legacy.interactive_mode filename
          else
            Interactive.Ide.interactive_mode filename
        end

      (* Normal, batch mode compiler *)
      else if List.length filenames >= 1 then begin //normal batch mode
        if Nil? filenames then
          Errors.raise_error0 Errors.Error_MissingFileName "No file provided";

        let filenames, dep_graph = Dependencies.find_deps_if_needed filenames CheckedFiles.load_parsing_data_from_cache in
        let tcrs, env, cleanup = Universal.batch_mode_tc filenames dep_graph in
        ignore (cleanup env);
        let module_names =
          tcrs
          |> List.map (fun tcr ->
             Universal.module_or_interface_name tcr.checked_module)
        in
        report_errors module_names;
        finished_message module_names 0
      end //end batch mode

(****************************************************************************)
(* Main function                                                            *)
(****************************************************************************)

(* choose a main driver function and go *)
let go () =
  let args = Util.get_cmd_args () in
  match args with
  | _ :: "--ocamlenv" :: rest -> go_ocamlenv rest
  | _ -> go_normal ()

(* This is pretty awful. Now that we have Lazy_embedding, we can get rid of this table. *)
let lazy_chooser (k:Syntax.Syntax.lazy_kind) (i:Syntax.Syntax.lazyinfo) : Syntax.Syntax.term
  = match k with
    (* TODO: explain *)
    | FStarC.Syntax.Syntax.BadLazy               -> failwith "lazy chooser: got a BadLazy"
    | FStarC.Syntax.Syntax.Lazy_bv               -> RE.unfold_lazy_bv          i
    | FStarC.Syntax.Syntax.Lazy_namedv           -> RE.unfold_lazy_namedv      i
    | FStarC.Syntax.Syntax.Lazy_binder           -> RE.unfold_lazy_binder      i
    | FStarC.Syntax.Syntax.Lazy_letbinding       -> RE.unfold_lazy_letbinding  i
    | FStarC.Syntax.Syntax.Lazy_optionstate      -> RE.unfold_lazy_optionstate i
    | FStarC.Syntax.Syntax.Lazy_fvar             -> RE.unfold_lazy_fvar        i
    | FStarC.Syntax.Syntax.Lazy_comp             -> RE.unfold_lazy_comp        i
    | FStarC.Syntax.Syntax.Lazy_env              -> RE.unfold_lazy_env         i
    | FStarC.Syntax.Syntax.Lazy_sigelt           -> RE.unfold_lazy_sigelt      i
    | FStarC.Syntax.Syntax.Lazy_universe         -> RE.unfold_lazy_universe    i

    | FStarC.Syntax.Syntax.Lazy_proofstate       -> Tactics.Embedding.unfold_lazy_proofstate i
    | FStarC.Syntax.Syntax.Lazy_goal             -> Tactics.Embedding.unfold_lazy_goal i

    | FStarC.Syntax.Syntax.Lazy_doc              -> RE.unfold_lazy_doc i

    | FStarC.Syntax.Syntax.Lazy_uvar             -> FStarC.Syntax.Util.exp_string "((uvar))"
    | FStarC.Syntax.Syntax.Lazy_universe_uvar    -> FStarC.Syntax.Util.exp_string "((universe_uvar))"
    | FStarC.Syntax.Syntax.Lazy_issue            -> FStarC.Syntax.Util.exp_string "((issue))"
    | FStarC.Syntax.Syntax.Lazy_ident            -> FStarC.Syntax.Util.exp_string "((ident))"
    | FStarC.Syntax.Syntax.Lazy_tref             -> FStarC.Syntax.Util.exp_string "((tref))"

    | FStarC.Syntax.Syntax.Lazy_embedding (_, t) -> Thunk.force t
    | FStarC.Syntax.Syntax.Lazy_extension s      -> FStarC.Syntax.Util.exp_string (format1 "((extension %s))" s)

// This is called directly by the Javascript port (it doesn't call Main)
let setup_hooks () =
    FStarC.Syntax.DsEnv.ugly_sigelt_to_string_hook := show;
    FStarC.Errors.set_parse_warn_error FStarC.Parser.ParseIt.parse_warn_error;
    FStarC.Syntax.Syntax.lazy_chooser := Some lazy_chooser;
    FStarC.Syntax.Util.tts_f := Some show;
    FStarC.Syntax.Util.ttd_f := Some Class.PP.pp;
    FStarC.TypeChecker.Normalize.unembed_binder_knot := Some RE.e_binder;
    List.iter Tactics.Interpreter.register_tactic_primitive_step Tactics.V1.Primops.ops;
    List.iter Tactics.Interpreter.register_tactic_primitive_step Tactics.V2.Primops.ops;
    ()

let handle_error e =
    if FStarC.Errors.handleable e then
      FStarC.Errors.err_exn e;
    if Options.trace_error() then
      Util.print2_error "Unexpected error\n%s\n%s\n" (Util.message_of_exn e) (Util.trace_of_exn e)
    else if not (FStarC.Errors.handleable e) then
      Util.print1_error "Unexpected error; please file a bug report, ideally with a minimized version of the source program that triggered the error.\n%s\n" (Util.message_of_exn e);
    cleanup();
    report_errors []

let main () =
  try
    setup_hooks ();
    let _, time = Util.record_time go in
    if FStarC.Options.query_stats()
    then Util.print2_error "TOTAL TIME %s ms: %s\n"
              (FStarC.Compiler.Util.string_of_int time)
              (String.concat " " (FStarC.Getopt.cmdline()));
    cleanup ();
    exit 0
  with
  | e -> handle_error e;
        exit 1