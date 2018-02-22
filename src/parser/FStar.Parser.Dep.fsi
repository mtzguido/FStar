#light "off"
module FStar.Parser.Dep
open FStar.ST
open FStar.All
open FStar
open FStar.Parser
open FStar.Parser.AST
open FStar.Parser.Parse
open FStar.Util
open FStar.Const
open FStar.String
open FStar.Ident
open FStar.Errors
module Const = FStar.Parser.Const
module BU = FStar.Util

type open_kind = | Open_module | Open_namespace

val module_name_of_file : string -> string
val lowercase_module_name : string -> string

val build_inclusion_candidates_list : unit -> list<(string * string)>

(* Given a filename, returns the list of automatically opened modules
and namespaces *)
val hard_coded_dependencies : string -> list<(lident * open_kind)>

val is_interface: string -> bool
val is_implementation: string -> bool
val interface_filename: string -> string

type deps
val empty_deps : deps
val check_or_use_extracted_interface: list<string> -> string -> bool
val cache_file_name: list<string> -> string -> string
val all_cmd_line_files: deps -> list<string>
val collect: list<string> -> list<string> * deps
val deps_of : deps -> string -> list<string>
val print : deps -> unit
val hash_dependences: deps -> string -> option<(list<(string*string)>)>
val print_digest: list<(string * string)> -> string
