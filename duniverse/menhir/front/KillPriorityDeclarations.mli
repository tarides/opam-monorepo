(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This transformation removes all priority declarations in a grammar. *)

open PlainSyntax

(**[kill] transforms a grammar by removing all priority declarations (%left,
   %right, %nonassoc, %prec, %on_error_reduce). *)
val kill : grammar -> grammar
