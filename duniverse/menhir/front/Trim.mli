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

(**[trim main g] restricts the grammar [g] to the set of nonterminal symbols
   that are reachable, via the productions, from the start symbols. There must
   exist at least one start symbol; otherwise, an error is reported. The
   channel [main] is used to report error and emit warnings. *)
val trim: channel -> grammar -> grammar
