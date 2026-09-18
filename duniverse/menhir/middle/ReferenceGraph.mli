(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module builds the forward reference graph of the grammar and prints it
   in [.dot] format. The vertices of this graph are the nonterminal symbols.
   There is an edge of a nonterminal symbol [nt1] to every nonterminal symbol
   [nt2] that occurs in the definition of [nt1]. *)

open MiddleAPI

module Make (G : GRAMMAR) : sig

  (**[print filename] writes a description of the reference graph of the
     grammar [G] to a new file named [filename]. *)
  val print: string -> unit

end
