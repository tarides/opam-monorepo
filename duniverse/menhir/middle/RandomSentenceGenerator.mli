(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module generates random sentences that are well-formed according
   to a grammar.

   If the grammar is not LR(1), these sentences are not necessarily accepted
   by the automaton.

   The distribution of sentences is not uniform.

   The goal length is not necessarily obeyed exactly; the generator normally
   produces a sentence whose length is at most [goal], unless [goal] is so
   small that no sentence of length [goal] exists.

   The time complexity is roughly linear with respect to the goal length.

   Because we do not wish to generate sentences that contain the [error]
   token, any production that contains this token is ignored. This can
   cause a problem if the goal can be achieved only via such a production.
   This is hopefully unlikely. *)

open MiddleAPI

module Make (G : GRAMMAR) : sig
  open G

  (**[nonterminal nt goal] returns a random sentence that is generated
     by the nonterminal symbol [nt] and whose length is close to the
     goal length [goal]. *)
  val nonterminal: Nonterminal.t -> int -> Terminal.t list

end
