(** Tokenizer for HTML source generation.

    Re-lexes F* source files and returns classified token spans
    for syntax highlighting. *)

(** CSS class for syntax highlighting *)
type css_class =
  | Kw   (** keyword *)
  | Op   (** operator / punctuation *)
  | Str  (** string literal *)
  | Num  (** numeric literal *)
  | Ch   (** character literal *)
  | Cm   (** comment *)
  | Cn   (** constructor / type name (NAME tokens) *)
  | At   (** attribute *)
  | Pp   (** pragma *)
  | Id   (** identifier *)

(** A classified span of source text *)
type token_span = {
  start_line : Prims.int;
  start_col  : Prims.int;
  end_line   : Prims.int;
  end_col    : Prims.int;
  cls        : css_class;
  text       : Prims.string;
}

(** Tokenize a source file, returning all token and comment spans. *)
val tokenize_file : Prims.string -> token_span list
