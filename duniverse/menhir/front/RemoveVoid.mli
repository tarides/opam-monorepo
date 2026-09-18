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

(**[transform grammar] transforms the grammar by removing the nonterminal
   symbols that generate the empty language. If a start symbol generates
   the empty language, a fatal error takes place. *)
val transform: grammar -> grammar
