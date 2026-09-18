(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module defines a few concrete types for use by [RawSentenceParser]. *)

open Lexing

type raw_symbol =
  string * position * position

type raw_nonterminal =
  raw_symbol

type raw_terminal =
  raw_symbol

type raw_sentence =
  raw_nonterminal option * raw_terminal list

type raw_entry =
  raw_sentence OrComment.t list

let opening (nto, terminals) : position =
  match nto, terminals with
  | Some (_, opening, _), _
  | None, (_, opening, _) :: _ ->
      opening
  | None, [] ->
      dummy_pos (* cannot happen *)

let closing (nto, terminals) : position =
  match nto, List.rev terminals with
  | _, (_, _, closing) :: _
  | Some (_, _, closing), _ ->
      closing
  | None, [] ->
      dummy_pos (* cannot happen *)

let range sentence =
  Range.make (opening sentence, closing sentence)
