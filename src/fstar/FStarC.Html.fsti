module FStarC.Html

open FStarC.Effect

(** Generate HTML files for the given source files.
    [env] is the typechecker environment (with sigtab/dsenv populated from cache).
    [modul] is the checked module (for extracting opens/abbrevs).
    [filenames] is the list of source files to generate HTML for. *)
val generate_html : FStarC.TypeChecker.Env.env -> FStarC.Syntax.Syntax.modul -> list string -> ML unit
