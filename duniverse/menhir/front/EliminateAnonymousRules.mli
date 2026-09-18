(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Syntax

(**[transform] eliminates all anonymous rules in a partial grammar. Thus, the
   result grammar does not use the constructor [ParameterAnonymous] any more. *)
val transform: partial_grammar -> partial_grammar
