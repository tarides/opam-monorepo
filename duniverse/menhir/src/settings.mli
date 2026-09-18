(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(** This module parses the command line. *)

open FrontAPI
open MiddleAPI

include API_SETTINGS
include BASE_SETTINGS
include CHECK_GRAMMAR_SETTINGS
include COMMENT_SETTINGS
include CONSTRUCTION_MODE_SETTINGS
include EXN_SETTINGS
include INFER_SETTINGS
include NOPREFIX_SETTINGS
include REPRESENT_SETTINGS
include STRATEGY_SETTINGS
include TOKEN_TYPE_SETTINGS
include TRACE_SETTINGS

(**The back-end that must be used.

   When the back-end is [Interpret show], Menhir behaves as an interactive
   interpreter of sentences provided by the user on the standard input
   channel. After each sentence is read, Menhir indicates whether this
   sentence is rejected or accepted. (The flag --trace is taken into account,
   so trace messages can be emitted if desired by the user.) The parameter
   [show] indicates whether a concrete syntax tree should be displayed after
   an input is accepted.

   When the back-end is [InterpretGLR show], the GLR reference interpreter is
   used.

   When the back-end is [InterpretError], Menhir expects just one input
   sentence, expects it to trigger an error at the very last token, and
   displays which state was reached. *)
val backend: [
  | `NoBackend
  | `CodeBackend
  | `TableBackend
  | `GLRBackend
  | `RocqBackend
  | `Interpret    of [`ShowCST | `DoNotShowCST]
  | `InterpretGLR of [`ShowCST | `DoNotShowCST]
  | `InterpretError
]

(**[enabled_GLR] is true if the backend if [`GLRBackend] or [`InterpretGLR _],
   that is, if and only if --GLR was selected by the user. *)
val enabled_GLR: bool

(** The list of file names that appear on the command line. *)
val filenames: string list

(**If [token_type_only] is true then Menhir stops after generating the
   definition of the type [token]. This corresponds to [--only-tokens]. *)
val token_type_only: bool

(** Whether conflicts should be explained. *)
val explain: bool

(** Whether the automaton should be dumped before conflict resolution. *)
val dump: bool

(** Whether the automaton should be dumped after conflict resolution. *)
val dump_resolved: bool

(** Whether the grammar's reference graph should be dumped. *)
val reference_graph: bool

(** Whether the automaton's graph should be dumped. *)
val automaton_graph: bool

type preprocess_mode =
  | PMNormal
      (** preprocess and continue *)
  | PMOnlyPreprocess of PlainSyntaxPrinter.mode
      (** preprocess, print grammar, stop *)

(** Whether one should stop and print the grammar after joining and
   expanding the grammar. *)
val preprocess_mode: preprocess_mode

type infer_mode =
  (* Perform no type inference. This is the default mode. *)
  | IMNone
  (* Perform type inference by invoking ocamlc directly. *)
  | IMInfer                (** --infer *)
  | IMDependRaw            (** --raw-depend *)
  | IMDependPostprocess    (** --depend *)
  (* Perform type inference by writing a mock .ml file and reading the
     corresponding inferred .mli file. *)
  | IMWriteQuery of string (** --infer-write-query <filename> *)
  | IMReadReply of string  (** --infer-read-reply <filename> *)

(** Whether and how OCaml type inference (for semantic actions and nonterminal
    symbols) should be performed. See the manual for details. *)
val infer: infer_mode

(** Whether one should inline the non terminal definitions marked
    with the %inline keyword. *)
val inline: bool

(** This undocumented flag causes the code to be transformed by
    [Inline] or [StackLangTransform.inline]. It is on by default. *)
val code_inlining: bool

(** How verbose we should be. *)

(** Whether the standard menhir library should be used. *)
val no_stdlib : bool

(** Whether to generate a version check for MenhirLib in the generated parser. *)
val rocq_no_version_check : bool

(** Whether the generated Rocq file must contain completeness proofs. *)
val rocq_no_complete : bool

(** Whether the Rocq back-end should ignore types and semantic actions. *)
val rocq_no_actions : bool

(** This flag suppresses all warnings about unused precedence levels. *)
val ignore_all_unused_precedence_levels: bool

(** This flag causes Menhir to produce a list of erroneous input sentences.
   Enough sentences are computed to produce exactly one error in every state
   where an error can occur. *)
val list_errors: bool

(** This flag causes Menhir to read the error message descriptions stored in
   [filename] and compile them to OCaml code. *)
val compile_errors: string option

(** If present, this is a pair of .messages files whose contents should
   be compared. *)
val compare_errors: (string * string) option

(** If present, this is a pair of .messages files whose contents should
   be merged. *)
val merge_errors: (string * string) option

(** This flag causes Menhir to read the error message descriptions stored in
   [filename] and re-generate the auto-generated comments, which begin with
   [##]. This allows bringing these comments up to date when the grammar
   evolves. *)
val update_errors: string option

(** This flag causes Menhir to read the error message descriptions stored in
   [filename] and echo the error sentences (and nothing else; no messages,
   no comments). *)
val echo_errors: string option

(** This flag causes Menhir to read the error message descriptions stored in
   [filename] and echo the error sentences, including the concrete syntax
   of each sentence, in an auto-comment. *)
val echo_errors_concrete: string option

(** This flag causes Menhir to produce a [.cmly] file, which contains a
   binary-format description of the grammar and automaton. *)
val cmly: bool

(** This name is used in --rocq mode. It appears in the generated Rocq file,
   and indicates under what name (or path) the Rocq library MenhirLib is
   known. Its default value is [Some "MenhirLib"]. *)
val rocq_lib_path: string option

(**When [random_sentence] is [Some (nt, goal, style)], the user is asking
   Menhir to show a random sentence. The sentence must be generated by the
   nonterminal symbol [nt]. Its goal length is [goal]. The [style] parameter
   indicates whether the sentence should be displayed in concrete syntax; if
   it is [`Concrete], then every token must have a token alias. *)
val random_sentence : (string * int * [`Abstract | `Concrete]) option

(**The setting -O allows choosing an optimization level. It is used only by
   the code back-end. *)
val optimization_level: int

(**The undocumented flag [--stacklang-dump] causes the StackLang program to be
   printed. *)
val stacklang_dump: bool

(**The undocumented flag [--stacklang-graph] causes the StackLang program to be
   dumped in the form of a control flow graph in the file [<basename>.dot]. *)
val stacklang_graph: bool

(**The undocumented flag [--stacklang-test] causes the StackLang program to be
   tested (by comparison with the reference interpreter). *)
val stacklang_test: bool

(**The undocumented flag [--specialize-token] causes the StackLang program to
   be specialized with respect to the current token. This guarantees that
   every token is case-analyzed exactly once, immediately after it has been
   read from the lexer. *)
val specialize_token: bool

(**The undocumented flag [--pack-classic] causes the table back-end to perform
   table compression using the classic row displacement algorithm. Without
   this flag, table compression is performed using the new row displacement
   algorithm. The new algorithm has been found to be significantly faster and
   to produce results of roughly comparable quality. The size of the resulting
   tables can vary, up or down, by up to 5% or 10%. This flag will be removed
   in the future. *)
val pack_classic: bool

(**The undocumented flags [--expand-nullable-symbols],
   [--expand-nullable-suffix-participants], and [--expand-nullable-suffixes]
   cause some or all occurrences of nullable symbols to be expanded away.
   See [ExpandNullable]. *)
val expand_nullable:
  [ `ExpandNullableSymbols
  | `ExpandNullableSuffixParticipants
  | `ExpandNullableSuffixes ]
  option

(**The undocumented flag [--test-GLR] requests a test of the GLR reference
   interpreter. *)
val test_GLR: bool

(**The undocumented flag [--kill-priorities] causes all priority
   declarations to be ignored. *)
val kill_priorities: bool

(**The undocumented flag [--avoid-eos] artificially extends the grammar with a
   new [EOF] terminal symbol so as to guarantee the absence of end-of-stream
   conflicts. *)
val avoid_eos: bool
