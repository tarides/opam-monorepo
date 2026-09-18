(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This transformation expands away some or all of the parameterized
   nonterminal symbols in a grammar. *)

open Syntax
open Sort

type mode =
  | ExpandHigherSort
    (**The mode [ExpandHigherSort] causes a partial expansion: only the
       parameters of higher sort (i.e., of sort other than [*]) are
       expanded away. This mode is safe, in the sense that expansion
       always terminates. A proof sketch is as follows: 1- an application
       always has sort [*]; 2- therefore, only a variable can have higher
       sort; 3- therefore, only a finite number of terms can appear during
       expansion. *)

  | ExpandAll
    (**The mode [ExpandAll] causes a complete expansion: all parameters
       are expanded away. This process is potentially nonterminating. One
       must first run the termination test in [CheckSafeExpansion] (which
       itself is applicable only after the parameters of higher sort have
       been expanded away). *)

(**[expand mode sorts g] expands away some or all of the parameterized
   nonterminal symbols in the grammar [g], producing a new grammar.
   [sorts] is a mapping of symbols to sorts; it can be produced by
   [SortInference]. *)
val expand: mode -> sorts -> grammar -> grammar
