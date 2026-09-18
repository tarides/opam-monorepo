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

(**[final_only c reason grammar] ensures that the [error] token is used only
   the end of a production. It returns a copy of the grammar [grammar] where
   the productions that violate this rule have been removed. The channel [c]
   is used to emit warnings. The string [reason] is used to explain why this
   policy is imposed. *)
val final_only: channel -> string -> grammar -> grammar

(* Why do we do this? The simplified strategy does not query the lexer for a
   new token after shifting the [error] token. This implies that the [error]
   token remains on the stream. If the [error] token is used at the end of a
   production, then this is not a problem; after we shift, a reduction must
   take place, whose semantic action will abort the parser. However, if the
   [error] token was used inside a production, then we could very well fall
   into an endless shift cycle. *)

(**[nowhere c reason grammar] ensures that the [error] token is used nowhere.
   It returns a copy of the grammar [grammar] where the productions that use
   the [error] token have been removed. The channel [c] is used to emit
   warnings. The string [reason] is used to explain why this policy is
   imposed. *)
val nowhere: channel -> string -> grammar -> grammar
