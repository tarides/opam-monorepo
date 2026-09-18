(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module drives the front-end. It opens and parses the input files,
   which yields a number of partial grammars. It joins these grammars, expands
   them to get rid of parameterized nonterminals, and performs reachability
   analysis. This yields a single unified grammar. It then performs type
   inference. This yields the grammar that the middle-end works with (often
   through the interface provided by module [Grammar]). *)

(**This is the grammar as it exists before parameterized nonterminal symbols
   have been expanded. *)
val grammar_before_expansion : Syntax.grammar

(**This is the grammar as it exists after parameterized nonterminal symbols
   have been expanded and before nonterminal symbols marked with [%inline]
   have been eliminated. *)
val grammar_before_inlining : PlainSyntax.grammar

(**This is the grammar that the middle-end works with. *)
val grammar: PlainSyntax.grammar
