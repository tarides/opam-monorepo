(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Printf
open MiddleAPI

module Make (Lr1 : LR1) (X : sig
  include STRATEGY_SETTINGS
  include TRACE_SETTINGS
end) = struct

module Lr0 = Lr1.Lr0
module G = Lr0.G
open G

module R =
  ReferenceInterpreter.Make(Lr1)(X)

(* -------------------------------------------------------------------------- *)

(* [interpret_sentence] interprets a sentence and produces just one line of
   output. Thus, in case multiple sentences are supplied, it should be clear
   to the user which outcomes correspond to which sentences. *)

let interpret_sentence show_cst sentence : unit =
  let nt = Sentence.start sentence
  and _, toks = sentence in
  let module T = TerminalStream.Make(G) in

  (* Run the reference interpreter. This can produce a concrete syntax tree
     ([Some cst]), fail with a parser error ([None]), or fail with a lexer
     error ([EndOfStream]). *)

  let lexer, lexbuf = T.stream toks in
  let count = ref 0 (* unused *) in
  match R.interpret count nt lexer lexbuf with

  | Some cst ->
      (* Success. *)
      printf "ACCEPT";
      begin match show_cst with
      | `ShowCST ->
          print_newline();
          CST.show stdout cst
      | `DoNotShowCST ->
          ()
      end

  | None ->
      (* Parser failure. *)
      printf "REJECT"

  | exception T.EndOfStream ->
      (* Lexer failure. *)
      printf "OVERSHOOT"

(* -------------------------------------------------------------------------- *)

(* [setup c] returns a function [read] which reads one raw sentence from the
   standard input channel and immediately validates it, producing a sentence. *)

let setup c : unit -> Sentence.sentence option =

  let open Lexing in
  let lexbuf = from_channel stdin in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "(stdin)" };

  let read () =
    match RawSentenceParser.optional_sentence RawSentenceLexer.lex lexbuf with
    | exception RawSentenceLexer.Error ->
        Report.error c [Range.current lexbuf]
          "unexpected character.\n\
           (I believe I am reading a sentence, but may be off.)"
    | exception Parsing.Parse_error ->
        Report.error c [Range.current lexbuf]
          "ill-formed input sentence."
    | osentence ->
        Validate.option Sentence.validate_sentence c osentence
  in

  read

(* -------------------------------------------------------------------------- *)

(* The two commands. *)

let interpret show_cst =
  Report.monitor `Normal @@ fun c ->
  let read = setup c in
  printf "Ready!\n%!";
  while true do
    match read() with
    | None ->
        exit 0
    | Some sentence ->
        interpret_sentence show_cst sentence;
        print_newline()
  done

let interpret_error () =
  Report.monitor `Normal @@ fun c ->
  let read = setup c in
  match read() with
  | None ->
      exit 1 (* abnormal: no input *)
  | Some sentence ->
      let fail msg =
        Report.error c [] "%s" msg
      and succeed target =
        R.print_messages_item (sentence, target);
        exit 0
      in
      R.interpret_error_sentence sentence fail succeed
        (* never returns *)

end (* Make *)
