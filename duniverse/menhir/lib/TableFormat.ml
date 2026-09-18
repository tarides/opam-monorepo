(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(**This signature defines the format of the parse tables.
   It is used as an argument to [TableInterpreter.Make]. *)
module type TABLES = sig

  (**The type of tokens. *)
  type token

  (**[terminal_count] is the number of terminal symbols, without [#]. *)
  val terminal_count: int

  (**[token2terminal] maps a token to a terminal symbol, represented
     by its internal integer code. *)
  val token2terminal: token -> int

  (**[error_terminal] is the integer code of the special token [error]. *)
  val error_terminal: int

  (**[token2value] maps a token to its semantic value. *)
  val token2value: token -> Obj.t

  (**Traditionally, an LR automaton is described by two tables, namely, an
     action table and a goto table. See, for instance, the Dragon book.

     The action table is a two-dimensional matrix that maps a state and a
     lookahead token to an action. An action is one of: shift to a certain
     state, reduce a certain production, accept, or fail.

     The goto table is a two-dimensional matrix that maps a state and a
     non-terminal symbol to either a state or undefined. By construction, this
     table is sparse: its undefined entries are never looked up. A compression
     technique is free to overlap them with other entries.

     In Menhir, things are slightly different. If a state has a default
     reduction on token [#], then that reduction must be performed without
     consulting the lookahead token. As a result, we must first determine
     whether that is the case, before we can obtain a lookahead token and use
     it as an index in the action table.

     Thus, Menhir's tables are as follows. *)

  (**The default reduction table, a one-dimensional table, maps a state to
     either ``no default reduction'' (encoded as: 0) or ``by default, reduce
     prod'' (encoded as: 1 + prod). The action table is looked up only when
     there is no default reduction. *)
  val default_reduction: int -> int

  (**Menhir follows Dencker, Dürre and Heuft, who point out that, although the
     action table is not sparse by nature (i.e., the error entries are
     significant), it can be made sparse by first factoring out a binary error
     matrix, then replacing the error entries in the action table with
     undefined entries. Thus: *)

  (**The error bitmap, a two-dimensional table, maps a state and a terminal
     symbol to either "fail" (encoded as: 0) or "do not fail" (encoded as: 1).
     The action table is looked up only in the latter case.

     The function [error] offers read access to the error bitmap.

     The error bitmap does not contain a column for the [#] pseudo-terminal.
     Thus, its width is [terminal_count]. *)
  val error: int -> int -> int

  (**The action table, a two-dimensional table, maps a state and a terminal
     to one of ``shift to state s and discard the current token'' (encoded
     as: [s | 10]), ``shift to state s without discarding the current token''
     (encoded as: [es | 11]), or ``reduce prod'' (encoded as: [prod | 01]).

     Like the error bitmap, the action table does not contain a column for the
     [#] pseudo-terminal. *)
  val action: int -> int -> int

  (**A one-dimensional table, [lhs], maps a production to its left-hand side
     (a non-terminal symbol). *)
  val lhs: int -> int

  (**The goto table, a two-dimensional table, maps a state and a non-terminal
     symbol to either undefined (encoded as: 0) or a new state s (encoded as:
     1 + s).. *)
  val goto: int -> int -> int

  (**[start] is the number of start productions. A production [prod] is a
     start production if and only if [prod < start] holds. This is also the
     number of start symbols. A nonterminal symbol [nt] is a start symbol if
     and only if [nt < start] holds. *)
  val start: int

  (**The semantic action table, a one-dimensional table, maps productions to
     semantic actions. The calling convention for semantic actions is
     described in [EngineTypes]. This table contains ONLY NON-START
     PRODUCTIONS, so the indexing is off by [start]. Be careful. *)
  val semantic_action: ((int, Obj.t, token) EngineTypes.env ->
                        (int, Obj.t)        EngineTypes.stack) array

  (**The exception [Error] can be raised by semantic actions, caught by the
     engine, and raised again by the engine for the final user to observe. *)
  exception Error

  (**[trace] indicates whether a trace should be generated. Generating a trace
     requires two extra tables, which respectively map a terminal symbol and a
     production to a string. *)
  val trace: (string array * string array) option

end
