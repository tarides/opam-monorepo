(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module expands away token aliases. *)

(**Token aliases are quoted strings that are used to provide syntactic sugar
   for terminal symbols, for example, to allow "+" to be used in grammar rules
   instead of PLUS, or to allow ")" instead of RPAREN. *)

open Report
open Syntax

(**[transform main gs] eliminates all references to token aliases in the list
   of partial grammars [gs]. (An alias declared in one partial grammar can be
   used in another partial grammar.) Declarations of token aliases are
   preserved, and could be used if desired (e.g. for printing). The channel
   [main] is used to report errors. *)
val transform: channel -> partial_grammar list -> partial_grammar list
