module FStarC.Html.Tokenizer

open FStarC.Effect

(** CSS class for syntax highlighting *)
type css_class =
  | Kw   (* keyword *)
  | Op   (* operator / punctuation *)
  | Str  (* string literal *)
  | Num  (* numeric literal *)
  | Ch   (* character literal *)
  | Cm   (* comment *)
  | Cn   (* constructor / type name *)
  | At   (* attribute *)
  | Pp   (* pragma *)
  | Id   (* identifier *)

(** A classified span of source text *)
type token_span = {
  start_line : int;
  start_col  : int;
  end_line   : int;
  end_col    : int;
  cls        : css_class;
  text       : string;
}

(** Tokenize a source file, returning all token and comment spans
    sorted by position. Backed by OCaml implementation. *)
val tokenize_file : string -> list token_span
