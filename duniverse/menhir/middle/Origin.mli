(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module computes which entry states can reach each [run], [reduce],
   and [goto] function. This information is computed only on demand.

   This information is used in the code back-end to determine in which states
   we have static knowledge of the final result type of the parser, ['final].
   This information can be built into the GADT that describes the states; this
   in turn can be used to perform certain optimizations (such as removing case
   analyses that have only one branch) while preserving the well-typedness of
   the OCaml code. *)

open MiddleAPI

module Make (Lr1 : LR1_AUTOMATON) : sig
  open Lr1.Lr0.G

  type origin =
    | Dead
      (**[Dead] indicates that this point in unreachable.*)
    | SingleOrigin of Nonterminal.t
      (**[SingleOrigin nt] indicates that the point of interest is reachable
         only via the start symbol [nt]. *)
    | MultipleOrigins
      (**[MultipleOrigins] indicates that this point is reachable via several
         start symbols. *)

  (**[run node] determines via which start symbols the node [node] is
     reachable. *)
  val run: Lr1.node -> origin

  (**[reduce prod] determines via which start symbols a node where production
     [prod] can be reduced is reachable. *)
  val reduce: Production.t -> origin

  (**[goto nt] determines via which start symbols an edge labeled with the
     nonterminal symbol [nt] is reachable. *)
  val goto: Nonterminal.t -> origin

end
