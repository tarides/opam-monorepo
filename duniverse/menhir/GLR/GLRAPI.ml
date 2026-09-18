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

(**The signature [DATA] describes the compile-time information that is needed
   by the GLR parsing algorithm. *)
module type DATA = sig

  (**We exploit the fact that LR(1) states are integers. *)
  type state = private int
  type token
  type semv
  type nonterminal

  (**A syntax error is detected when the number of nodes in the top set drops
     to zero. In such a situation, the exception [Error] is raised. It carries
     the list of top nodes of the previous generation. This list is nonempty.
     None of these nodes is capable of shifting the next input symbol. *)
  exception Error of (state, semv) GSS.node list

  (**We exploit the fact that productions are integers. This is not crucial,
     but lets us avoid the need for two operations on productions, namely
     [is_start] and [compare]. We rely on the following two conventions:

     - The productions whose index is less than [start] are the start
       productions.

     - If the productions [prod1] and [prod2] have left-hand-side symbols
       [nt1] and [nt2], and if [nt1 →+ nt2] holds, then [prod1 < prod2] must
       hold. (This requirement implies that the grammar must be acyclic.) *)
  type production = private int

  module Semv : sig

    (**[token2value] extracts a semantic value out of a token. *)
    val token2value: token -> semv

    (**[merge nt v1 v2 input date1 date2] merges the semantic values [v1] and
       [v2], both of which are associated with the nonterminal symbol [nt].
       [date1] and [date2] are the start date and end date of the input
       fragment with which the semantic values [v1] and [v2] are associated.
       The end user is expected to provide this operation. It might choose
       just one of the two values or somehow combine them, for example by
       building a disjunction node. *)
    val merge: nonterminal -> semv -> semv -> token input -> int -> int -> semv
      (* We choose to pass [input], [date1] and [date2] to the merge function,
         as opposed to values of type [Lexing.position], because it is cheaper.
         A conversion of dates to positions can be performed inside the merge
         function if needed. *)

  end

  module Production : sig

    (**The productions whose index is less than [start] are the start
       productions. *)
    val start: production

    (**[length prod] is the length of (the right-hand side of)
       the production [prod]. *)
    val length : production -> int

    (**[lhs prod] is the left-hand side of the production [prod]. *)
    val lhs : production -> nonterminal

    (**[action prod input path] executes the semantic action associated with
       the production [prod]. The path [path] matches the right-hand side of
       this production and allows the semantic action to access the semantic
       values of the constituents of the right-hand side, as well as to their
       start and end dates. The parameter [input] can be used to translate
       dates to positions. *)
    val action : production -> token input -> (state, semv) Path.t -> semv

    (**[print prod] converts the production [prod] to a string. It is
       used for debugging only. *)
    val print: production -> string

  end

  module State : sig

    (**[n] is the number of states in the LR(1) automaton. We exploit the
       fact that every state number lies in the semi-open interval [\[0, n)]. *)
    val n : int

    (**[foreach_shift state input] enumerates the shift transitions that can
       be taken, out of the state [state], considering the current lookahead
       symbol, which can be obtained via [input]. Because an LR automaton
       is deterministic, there is at most one such transition. *)
    val foreach_shift : state -> token input -> (state -> unit) -> unit

    (**[foreach_reduction state input] enumerates the productions that can
       be reduced, in the state [state], considering the current lookahead
       symbol, which can be obtained via [input]. There can be zero, one,
       or several such reductions. If there is a default reduction then
       this function does not need to query the input stream [input] to
       examine the lookahead symbol. *)
    val foreach_reduction : state -> token input -> (production -> unit) -> unit

    (**[goto state nt] returns the target state of the goto transition that
       leaves the state [state] and is labeled [nt]. This transition must
       exist. *)
    val goto : state -> nonterminal -> state

    (**[unique_action] offers information that is also accessible (in a less
       efficient way) through [foreach_shift] and [foreach_reduction].

       [unique_action state input] determines what shift and reduction actions
       are permitted, in the state [state], considering the current lookahead
       token, which can be obtained via [input]. If exactly one action is
       permitted, then a description of this action, [`Shift _] or [`Reduce _],
       is returned. If no action is permitted then [`Fail] is returned. If more
       than one action is permitted then [`Fork] is returned. *)
    val unique_action : state -> token input ->
      [`Shift of state | `Reduce of production | `Fail | `Fork ]

  end

end
