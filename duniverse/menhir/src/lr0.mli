(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open MiddleAPI

(**This module builds the LR(0) automaton associated with a grammar
   and provides access to this automaton. Furthermore, it provides
   facilities for the efficient construction of LR(1) automata. *)

include LR0 with module G = Grammar
