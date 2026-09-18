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

(**This module implements a [partition] function, which, given a list [xs] of
   possibly-overlapping sets, computes the coarsest partition of the the set
   [U xs] that refines [xs]. In other words, it computes the coarsest family
   [ys] of non-overlapping sets such that each set in the list [xs] can be
   written as a disjoint union of members of [ys].

   This is useful when representing the transitions of a DFA whose alphabet is
   large. Rather than representing one transition per symbol, we group the
   symbols into classes whose elements are never distinguished. *)

(**The operations required by [Make] form a fragment of the signature
   {!API.SET}. *)
module Make (Set : sig
  type t
  val is_empty : t -> bool
  val compare : t -> t -> int
  val compare_minimum : t -> t -> int
  val big_union : t list -> t
  val extract_unique_prefix : t -> t -> t * t
  val extract_shared_prefix : t -> t -> t * (t * t)
end) : sig

  open Set

  (**[partition] is also documented in {!API.SET}. *)
  val partition : t list -> t list

end
