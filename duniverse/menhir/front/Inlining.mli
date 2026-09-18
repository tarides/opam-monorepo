(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module implements the elimination of [%inline] nonterminal symbols. *)

open Report
open PlainSyntax

(**[inline c g] traverses the grammar [g] and inlines away the nonterminal
   symbols whose definitions are marked [%inline] or whose use sites are
   marked [%inlined]. The result is a grammar where no symbols are marked
   [%inline] and no use sites are marked [%inlined]. The channel [c] is used
   to report errors and emit warnings. *)
val inline: channel -> grammar -> grammar

(* Note: the syntax %inlined is not currently accessible to the end user. If
   it was then (at least) we should check against bad interactions of %inlined
   and %prec. Currently it is up to the caller of [inline] to check that such
   interactions cannot arise. *)
