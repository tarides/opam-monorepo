(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Report
open PlainSyntax

(**This module emits warnings about the presence or the absence of %merge
   functions. *)

(* I believe that one cannot decide whether a nonterminal symbol is ambiguous
   (this is an undecidable problem); therefore we cannot statically tell when
   a default %merge function is needed and when it is superfluous. We require
   a default %merge function as soon as one nonterminal symbol does not have
   a %merge function. *)

(**[every_rule_has_merge_fun grammar] determines whether every rule (that is,
   every nonterminal symbol) in the grammar [grammar] has a merge function. *)
val every_rule_has_merge_fun: grammar -> bool

(**[check active c grammar] emits warnings on the channel [c] about the
   presence or the absence of %merge functions in the grammar [grammar]. It
   must be invoked only after %inline symbols and nullable symbols have been
   expanded away.

   The parameter [active] determines whether %merge functions are active. It
   should be [true] if and only if the GLR back-end is selected. When it is
   [false], the presence of a %merge function causes a warning. When it is
   [true], a default %merge function must be provided unless every nonterminal
   symbol has a %merge function. *)
val check: bool -> channel -> grammar -> unit
