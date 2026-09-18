(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module offers a reference interpreter for LR(1) automata. It uses the
   LR engine in [MenhirLib.Engine], which it instantiates with the description
   of the grammar and automaton offered by the module [Lr1]. *)

open MiddleAPI

module Make (Lr1 : LR1) (X : sig
  include STRATEGY_SETTINGS
  include TRACE_SETTINGS
end) : sig
open Lr1.Lr0.G

  (**[interpret count nt lexer lexbuf] runs the parser, starting
     with the start non-terminal symbol [nt], with the lexer [lexer] and the
     lexing buffer [lexbuf]. It either succeeds and returns [Some cst], where
     [cst] is a concrete syntax tree, or fails and returns [None].

     The parameter [count] is incremented with the number of tracing
     messages that are emitted or that would be emitted if [trace] was
     [true]. *)
  val interpret:
    int ref ->
    Nonterminal.t ->
    (Lexing.lexbuf -> Terminal.t) ->
    Lexing.lexbuf ->
    CST.cst option

  (* [check_error_path], a variant of the reference interpreter, is used
     internally by us. We use it to debug [LRijkstra]. It checks that a
     sentence leads to a syntax error in the expected state. It is also used
     by several of the command line options [--interpret-error],
     [--compile-errors], etc. *)

  (**This type describes the outcome of {!check_error_path}. *)
  type check_error_path_outcome =
  | OInputReadPastEnd
    (**Bad: the input was read past its end. *)
  | OInputNotFullyConsumed
    (**Bad: a syntax error occurred before all of the input was read. *)
  | OUnexpectedAccept
    (**Bad: the parser unexpectedly accepted (part of) this input. *)
  | OK of target
    (**Good: a syntax error occurred after reading the last input token. We
       report in which state the error took place, as well as a list of
       spurious reductions. A non-default reduction that takes place after
       looking at the last input token (i.e., the erroneous token) is
       spurious. A spurious reduction can take place only in a non-canonical
       LR automaton. *)

  and target =
    Lr1.node * spurious_reduction list

  and spurious_reduction =
    Lr1.node * Production.t

  (**[check_error_path count nt input] runs the parser, starting with the
     start non-terminal symbol [nt], with the input stream [input]. We expect
     [input] to be a minimal invalid input: the parser is expected to fail
     after reading all of [input].

     The parameter [count] is incremented with the number of tracing messages
     that are emitted or that would be emitted if [trace] was [true]. *)
  val check_error_path:
    int ref ->
    Nonterminal.t ->
    Terminal.t list ->
    check_error_path_outcome

  (**[interpret_error_sentence sentence fail succeed] interprets the sentence
     [sentence], expecting it to end in an error. Failure or success is reported
     via the continuations [fail] and [succeed]. The continuation [fail] is
     invoked if the expected error is {i not} encountered; its an argument is an
     error message. The continuation [succeed] is invoked if the expected error is
     encountered: its argument is the target that has been reached. *)
  val interpret_error_sentence:
    (* sentence: *) Sentence.sentence ->
    (* failure:  *) (string -> 'a) ->
    (* success:  *) (target -> 'a) ->
                    'a

  (**This default error message is produced by [--list-errors] when it creates a
     [.messages] file, and is recognized by [--compare-errors] when it compares
     two such files. *)
  val default_message: string

  (**[print_messages_auto (sentence, target)] prints part of an item in the
     [.messages] file format. It prints the sentence [sentence], followed with
     auto-generated comments, indicating that this sentence leads to an error in
     the state [target], with spurious reductions also described by [target].
     The error message that is desired in this situation is {i not} printed. *)
  val print_messages_auto: Sentence.sentence * target -> unit

  (**[print_messages_item (sentence, target)] prints an item in the [.messages]
     file format. The first part of this item is printed by
     [print_messages_auto]. The second part is the default message
     {!default_message} between two blank lines. *)
  val print_messages_item: Sentence.sentence * target -> unit

end
