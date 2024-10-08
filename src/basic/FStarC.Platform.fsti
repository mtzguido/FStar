module FStarC.Platform
open FStarC.Compiler.Effect

type sys =
| Windows
| Posix

val system : sys
val exe : string -> string

(* true if the fstar compiler is compiled from sources extracted to ocaml, false otherwise *)
val is_fstar_compiler_using_ocaml : bool
