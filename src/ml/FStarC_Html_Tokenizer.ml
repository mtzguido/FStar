(** Tokenizer for HTML source generation.

    Re-lexes F* source files and returns classified token spans
    for syntax highlighting. Uses the F* lexer (FStarC_Parser_LexFStar)
    to get token positions. *)

open FStarC_Parser_Parse

type css_class =
  | Kw
  | Op
  | Str
  | Num
  | Ch
  | Cm
  | Cn
  | At
  | Pp
  | Id

type token_span = {
  start_line : Prims.int;
  start_col  : Prims.int;
  end_line   : Prims.int;
  end_col    : Prims.int;
  cls        : css_class;
  text       : Prims.string;
}

(** Classify a parser token into a CSS class *)
let classify_token = function
  (* Keywords *)
  | LET | AND | ASSERT | ASSUME | BEGIN | BY | CALC | CLASS | DECREASES
  | EFFECT | ELSE | END | ENSURES | EXCEPTION | EXISTS | FALSE
  | FORALL | FRIEND | FUN | FUNCTION | IF | IN | INCLUDE | INLINE
  | INLINE_FOR_EXTRACTION | INSTANCE | INTRO | ELIM | IRREDUCIBLE
  | LOGIC | MATCH | MODULE | NEW | NOEQUALITY | NOEXTRACT | OF | OPEN
  | OPAQUE | PRIVATE | REC | REIFIABLE | REFLECTABLE | REIFY
  | REQUIRES | RETURNS | RETURNS_EQ | SPLICE | SPLICET | SYNTH | THEN
  | TOTAL | TRUE | TRY | TYPE | UNFOLD | UNFOLDABLE | UNOPTEQUALITY
  | VAL | WHEN | AS | WITH
  | NEW_EFFECT | SUB_EFFECT | LAYERED_EFFECT | POLYMONADIC_BIND
  | POLYMONADIC_SUBCOMP
  | RANGE_OF | SET_RANGE_OF
  | LET_OP _ | AND_OP _ | MATCH_OP _ | IF_OP _
  | EXISTS_OP _ | FORALL_OP _ | SEMICOLON_OP _
    -> Kw
  (* Attributes *)
  | ATTRIBUTES
    -> At
  (* Pragmas *)
  | PRAGMA_SET_OPTIONS | PRAGMA_RESET_OPTIONS | PRAGMA_PUSH_OPTIONS
  | PRAGMA_POP_OPTIONS | PRAGMA_RESTART_SOLVER | PRAGMA_SHOW_OPTIONS
  | PRAGMA_PRINT_EFFECTS_GRAPH
    -> Pp
  (* String literals *)
  | STRING _
    -> Str
  (* Numeric literals *)
  | INT _ | INT8 _ | UINT8 _ | INT16 _ | UINT16 _
  | INT32 _ | UINT32 _ | INT64 _ | UINT64 _ | SIZET _ | REAL _
    -> Num
  (* Character literals *)
  | CHAR _
    -> Ch
  (* Constructors / type names *)
  | NAME _
    -> Cn
  (* Identifiers *)
  | IDENT _ | TILDE _
    -> Id
  (* Operators and punctuation *)
  | OPINFIX0a _ | OPINFIX0b _ | OPINFIX0c _ | OPINFIX0d _
  | OPINFIX1 _ | OPINFIX2 _ | OPINFIX3L _ | OPINFIX3R _ | OPINFIX4 _
  | OPPREFIX _ | OP_MIXFIX_ASSIGNMENT _ | OP_MIXFIX_ACCESS _
  | RARROW | LARROW | LONG_LEFT_ARROW | IFF | IMPLIES
  | CONJUNCTION | DISJUNCTION | PIPE_LEFT | PIPE_RIGHT
  | COLON | COLON_COLON | COLON_EQUALS | SEMICOLON | COMMA | DOT | DOT_DOT
  | EQUALS | BAR | HASH | AMP | DOLLAR | QMARK | QMARK_DOT | MINUS
  | SUBTYPE | EQUALTYPE | SUBKIND | SQUIGGLY_RARROW
  | LPAREN | RPAREN | LPAREN_RPAREN
  | LBRACK | RBRACK | LBRACK_BAR | BAR_RBRACK
  | LBRACE | RBRACE | LBRACE_BAR | BAR_RBRACE
  | LBRACK_AT | LBRACK_AT_AT | LBRACK_AT_AT_AT
  | PERCENT_LBRACK | DOT_LBRACK | DOT_LPAREN | DOT_LBRACK_BAR
  | DOT_LENS_PAREN_LEFT | LENS_PAREN_LEFT | LENS_PAREN_RIGHT
  | BANG_LBRACE | SEQ_BANG_LBRACK
  | LBRACE_COLON_PATTERN | LBRACE_COLON_WELL_FOUNDED
  | QUOTE | BACKTICK | BACKTICK_AT | BACKTICK_HASH | BACKTICK_PERC
  | UNIV_HASH | UNDERSCORE
    -> Op
  (* Skip these *)
  | EOF | BLOB _ | USE_LANG_BLOB _
    -> Op (* fallback *)

(** Read a file's contents as a string *)
let read_file (filename : string) : string =
  let ic = open_in_bin filename in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

(** Tokenize a source file, returning classified token spans sorted by position. *)
let tokenize_file (filename : string) : token_span list =
  let contents = read_file filename in
  let lexbuf = FStarC_Sedlexing.create contents filename 1 0 in
  (* Flush any stale comments *)
  ignore (FStarC_Parser_Util.flush_comments ());
  let spans = ref [] in
  let finished = ref false in
  while not !finished do
    try
      let tok = FStarC_Parser_LexFStar.token lexbuf in
      let sp = lexbuf.FStarC_Sedlexing.start_p in
      let ep = lexbuf.FStarC_Sedlexing.cur_p in
      begin match tok with
      | EOF -> finished := true
      | _ ->
        let start_line = Z.of_int sp.Lexing.pos_lnum in
        let start_col = Z.of_int (sp.Lexing.pos_cnum - sp.Lexing.pos_bol) in
        let end_line = Z.of_int ep.Lexing.pos_lnum in
        let end_col = Z.of_int (ep.Lexing.pos_cnum - ep.Lexing.pos_bol) in
        let text = FStarC_Sedlexing.lexeme lexbuf in
        let cls = classify_token tok in
        spans := { start_line; start_col; end_line; end_col; cls; text } :: !spans
      end
    with _ ->
      finished := true
  done;
  (* Collect comments captured during lexing *)
  let comments = FStarC_Parser_Util.flush_comments () in
  let comment_spans = List.map (fun (text, range) ->
    let s = FStarC_Range_Ops.start_of_range range in
    let e = FStarC_Range_Ops.end_of_range range in
    let start_line = s.FStarC_Range_Type.line in
    let start_col = s.FStarC_Range_Type.col in
    let end_line = e.FStarC_Range_Type.line in
    let end_col = e.FStarC_Range_Type.col in
    { start_line; start_col; end_line; end_col; cls = Cm; text }
  ) comments in
  (* Merge and sort by position *)
  let all_spans = List.rev_append !spans comment_spans in
  List.sort (fun a b ->
    let c = Z.compare a.start_line b.start_line in
    if c <> 0 then c else Z.compare a.start_col b.start_col
  ) all_spans
