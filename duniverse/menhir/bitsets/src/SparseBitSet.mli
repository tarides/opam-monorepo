(******************************************************************************)
(*                                                                            *)
(*                                  Bitsets                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*       Copyright 2025--2025 Inria. All rights reserved. This file is        *)
(*       distributed under the terms of the GNU Library General Public        *)
(*       License, with an exception, as described in the file LICENSE.        *)
(*                                                                            *)
(******************************************************************************)

(**This module offers immutable bit sets whose elements are not subject to
   a bound. These bit sets can store arbitrary nonnegative integer values.
   Their internal representation is sparse: a bit set is represented as a
   linked list of words. *)

(**A bit set represents a set of nonnegative integers. *)
include API.SET
  with type elt = int

(**/**)

(* The type [view] and the function [view] are currently not part of
   the signature [API.SET]. They are not publicly documented. *)

(**The type [view] offers a view of a set as a list of nonempty words.

   The constructor [N] represents the empty set.

   The constructor [C (o, w, s)] represents the disjoint union of the
   nonempty set [w], whose inhabitants must be shifted up by [o],
   and the set [s].

   An offset [o] is a nonnegative multiple of [WordBitSet.bound]. *)
type view =
  | N
  | C of offset * word * t

and offset =
  int

and word =
  WordBitSet.t

(**The function [view] offers a view of a sparse bit set as a list of words. *)
val view : t -> view

(**[check] is used only during testing. *)
val check : t -> unit
