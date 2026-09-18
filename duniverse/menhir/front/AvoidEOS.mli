(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open PlainSyntax

(**[transform grammar] transforms the grammar as follows: 1- a new terminal
   symbol [EOF] is introduced; 2- for each start symbol [S], a new start
   symbol [S'], defined by the production [S' := S EOF], is introduced. This
   transformation guarantees that the transformed grammar has no end-of-stream
   conflict. This can be useful when testing a parser with randomly-generated
   sentences. *)
val transform : grammar -> grammar
