(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* This is a copy of [Fix.GraphNumbering] where the traversal algorithm has
   been modified to use a recursive function instead of an explicit stack.
   This yields a different numbering. *)

open Fix

module Make
  (M : IMPERATIVE_MAPS)
  (G : GRAPH with type t = M.key)
= struct

  (* Set up a facility for numbering vertices. *)

  module N =
    Numbering.Make(M)

  (* Implement a depth-first search. The functions [N.has_been_encoded]
     and [N.encode] allow us not only to assign a unique number to each
     vertex, but also to mark a vertex and test whether a vertex has been
     marked. *)

  let rec visit x =
    if not (N.has_been_encoded x) then begin
      (* Assign a number to [x]. *)
      let (_ : int) = N.encode x in
      (* Visit its successors. *)
      G.foreach_successor x visit
    end

  (* Perform the depth-first search. *)

  let () =
    G.foreach_root visit

  (* We are done! This defines [n], [encode], [decode]. *)

  include N.Done()

end

module ForNumberedType (T : NUMBERING) =
  Make(Glue.ArraysAsImperativeMapsWithNumbering(T))

module ForOrderedType (T : OrderedType) =
  Make(Glue.PersistentMapsToImperativeMaps(Map.Make(T)))

module ForHashedType (T : HashedType) =
  Make(Glue.HashTablesAsImperativeMaps(T))

module ForType (T : TYPE) =
  ForHashedType(Glue.TrivialHashedType(T))
