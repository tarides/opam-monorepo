(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

open Input

(**To find out how the values discussed below are encoded at the bit level,
   please see [TableInterpreter] and [GLRBackend]. *)

(**This signature defines the format of the parse tables.
   It is used as an argument to [TableInterpreter.Make]. *)
module type TABLES = sig

  (**The type of tokens. *)
  type token

  (**A syntax error is detected when the number of nodes in the top set drops
     to zero. In such a situation, the exception [Error] is raised. It carries
     the list of top nodes of the previous generation. This list is nonempty.
     None of these nodes is capable of shifting the next input symbol. *)
  exception Error of (int, Obj.t) GSS.node list

  (**[token2terminal] maps a token to a terminal symbol, represented
     by its internal integer code. *)
  val token2terminal: token -> int

  (**[token2value] maps a token to its semantic value. *)
  val token2value: token -> Obj.t

  (**[merge nt v1 v2 input date1 date2] merges the semantic values [v1] and
     [v2], both of which are associated with the nonterminal symbol [nt].
     [date1] and [date2] are the start date and end date of the input
     fragment with which the semantic values [v1] and [v2] are associated.
     The end user is expected to provide this operation. It might choose
     just one of the two values or somehow combine them, for example by
     building a disjunction node. *)
  val merge: int -> Obj.t -> Obj.t -> token input -> int -> int -> Obj.t

  (**[start] is the number of start productions. A production [prod] is a
     start production if and only if [prod < start] holds. This is also the
     number of start symbols. A nonterminal symbol [nt] is a start symbol if
     and only if [nt < start] holds. *)
  val start: int

  (**The one-dimensional table [lhs] maps a production to its left-hand side
     (a non-terminal symbol). *)
  val lhs: int -> int

  (**The one-dimensional table [length] maps a production to its length. *)
  val length: int -> int

  (**[n] is the number of states of the LR(1) automaton. *)
  val n: int

  (**The default reduction table, a one-dimensional table, maps a state to
     either [dr_NoDefRed] or [dr_DefRed prod]. The following tables, namely
     [unique_action], [shift], and [reductions] are looked up only if there is
     no default reduction. *)
  val default_reduction: int -> int

  (**The unique action bitmap, a two-dimensional table, maps a state and a
     terminal symbol to either [uab_Fail] or [uab_DoNotFail]. The unique
     action table is looked up only in the latter case.

     This bitmap does not contain a column for the terminal symbol [#]. *)
  val unique_action_bitmap: int -> int -> int

  (**The unique action table, a two-dimensional table, maps a state and a
     terminal to one of [ua_Shift s], [ua_Reduce prod], or [ua_Fork]. The values
     [ua_Shift s] and [ua_Reduce prod] can be used only if this is the unique
     enabled action.

     This table is looked up only if there is no default reduction.
     This table is looked up only after the unique action bitmap has
     been looked up and has returned [uab_DoNotFail].

     This table does not contain a column for the terminal symbol [#]. *)
  val unique_action: int -> int -> int

  (**The shift bitmap, a two-dimensional table, maps a state and a terminal
     symbol to either [sb_Fail] or [sb_DoNotFail]. The shift table is looked up
     only in the latter case.

     This bitmap does not contain a column for the terminal symbol [#]. *)
  val shift_bitmap: int -> int -> int

  (**The shift table, a two-dimensional table, maps a state and a terminal to
     [s_Shift s].

     This table is looked up only if there is no default reduction.
     This table is looked up only after the shift bitmap has
     been looked up and has returned [sb_DoNotFail].

     This table does not contain a column for the terminal symbol [#]. *)
  val shift: int -> int -> int

  (**The reductions table, a two-dimensional table, maps a state and a terminal
     to a sequence of productions.

     This table is looked up only if there is no default reduction.

     This table does not contain a column for the terminal symbol [#]. *)
  val reductions: int -> int -> int list

  (**The goto table, a two-dimensional table, maps a state and a non-terminal
     symbol to [encode_Goto s']. This table must be looked up only at
     well-defined entries: that is, it can be looked up at state [s] and symbol
     [nt] only if [s] has an outgoing transition labeled [nt]. *)
  val goto: int -> int -> int

  (**The semantic action table, a one-dimensional table, maps productions to
     semantic actions. The calling convention for semantic actions is described
     in [GLRAPI]. A state is encoded as an integer number, but this fact should
     be irrelevant, as a semantic action does not use any information about
     states. A semantic value has type [Obj.t]. This table contains ONLY
     NON-START PRODUCTIONS, so the indexing is off by [start]. Be careful. *)
  val semantic_action: (token input -> (int, Obj.t) Path.t -> Obj.t) array

end
