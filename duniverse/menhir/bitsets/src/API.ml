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

(**The signature [SET] describes the operations that must be offered by
   an implementation of sets. *)
module type SET = sig

  (**The elements of a set have type [elt].

     We assume that the elements of a set are equipped with a total order.
     This order is mentioned in the specification of several operations on
     sets. *)
  type elt

  (**The type of sets. *)
  type t

  (** {1 Construction} *)

  (**[empty] is the empty set. *)
  val empty: t

  (**[singleton x] is a singleton set containing just the element [x]. *)
  val singleton: elt -> t

  (**[add x s] is the union of the sets [singleton x] and [s].

     Sharing with [s], when possible, is guaranteed.
     That is, if the result is mathematically equal to [s]
     then it is physically equal to [s]. *)
  val add: elt -> t -> t

  (**[remove x s] is the set [s] deprived of the element [x].

     Sharing with [s], when possible, is guaranteed.
     That is, if the result is mathematically equal to [s]
     then it is physically equal to [s]. *)
  val remove: elt -> t -> t

  (**[union s1 s2] is the union of the sets [s1] and [s2].

     Sharing with [s2], when possible, is guaranteed.
     That is, if the result is mathematically equal to [s2]
     then it is physically equal to [s2]. *)
  val union: t -> t -> t

  (**[inter s1 s2] is the intersection of the sets [s1] and [s2].

     Sharing with [s2], when possible, is guaranteed.
     That is, if the result is mathematically equal to [s2]
     then it is physically equal to [s2]. *)
  val inter: t -> t -> t

  (** [diff s1 s2] is the set difference of the sets [s1] and [s2].

     Sharing with [s1], when possible, is guaranteed.
     That is, if the result is mathematically equal to [s1]
     then it is physically equal to [s1]. *)
  val diff: t -> t -> t

  (** [above x s] is the set of the elements of [s] that are greater
      than [x].

     Sharing with [s], when possible, is guaranteed.
     That is, if the result is mathematically equal to [s]
     then it is physically equal to [s]. *)
  val above: elt -> t -> t

  (** {1 Cardinality} *)

  (**[is_empty s] determines whether the [s] is empty. *)
  val is_empty: t -> bool

  (**[is_singleton s] tests whether [s] is a singleton set. *)
  val is_singleton: t -> bool

  (**[cardinal s] is the cardinal of the set [s]. *)
  val cardinal: t -> int

  (** {1 Tests} *)

  (**[mem x s] determines whether [x] is a member of the set [s]. *)
  val mem: elt -> t -> bool

  (**[equal s1 s2] determines whether the sets [s1] and [s2] are equal. *)
  val equal: t -> t -> bool

  (**[compare] is a total order on sets. *)
  val compare: t -> t -> int

  (**[disjoint s1 s2] determines whether the sets [s1] and [s2] are
     disjoint, that is, whether their intersection is empty. *)
  val disjoint: t -> t -> bool

  (**[subset s1 s2] determines whether the set [s1] is a subset of the set
     [s2]. *)
  val subset: t -> t -> bool

  (**[quick_subset s1 s2] is a fast test for the property [s1 <> ∅ ∧ s1 ⊆ s2].

     It must be the case that either [s1] is a subset of [s2] or [s1] and [s2]
     are disjoint: that is, [s1 ⊆ s2 ⋁ s1 ∩ s2 = ∅] must hold.

     Under this precondition, the property [s1 <> ∅ ∧ s1 ⊆ s2] is equivalent to
     [not (disjoint s1 s2)]. However, [quick_subset s1 s2] can be faster than
     [not (disjoint s1 s2)]. *)
  val quick_subset: t -> t -> bool

  (** {1 Extraction} *)

  (** [minimum s] returns the minimum element of the set [s].
      If the set [s] is empty, the exception [Not_found] is raised. *)
  val minimum: t -> elt

  (** [maximum s] returns the maximum element of the set [s].
      If the set [s] is empty, the exception [Not_found] is raised. *)
  val maximum: t -> elt

  (**If the set [s] is nonempty, then [choose s] returns an arbitrary
     element of this set. Otherwise, the exception [Not_found] is raised. *)
  val choose: t -> elt

  (** {1 Iteration} *)

  (**[iter yield s] enumerates the elements of the set [s], in increasing
     order, by presenting them to the function [yield]. That is, [yield x]
     is invoked in turn for each element [x] of the set [s], in increasing
     order. *)
  val iter: (elt -> unit) -> t -> unit

  (**[fold yield s b] enumerates the elements of the set [s], in increasing
     order, by presenting them to the function [yield]. That is, [yield x b]
     is invoked in turn for each element [x] of the set [s], in increasing
     order, where [b] is the current (loop-carried) state. The final state
     is returned. *)
  val fold: (elt -> 'b -> 'b) -> t -> 'b -> 'b

  (**[elements s] is a list of all elements in the set [s],
     in an unspecified order. *)
  val elements: t -> elt list

  (**[of_list xs] is the set of all elements in the list [xs]. *)
  val of_list: elt list -> t

  (** [find_first_opt p s] returns the least element [x] of [s] such that
      [p x] is true. It returns [None] if no such element exists. *)
  val find_first_opt: (elt -> bool) -> t -> elt option

  (** {1 Decomposition} *)

  (**[compare_minimum], a total order on sets, is defined as follows:

     - The empty set is less than any nonempty set.
     - If the sets [s1] and [s2] are nonempty, then [compare_minimum s1 s2]
       is [compare (minimum s1) (minimum s2)]. *)
  val compare_minimum : t -> t -> int

  (**[big_union ss] computes the union of the sets in the list [ss]. *)
  val big_union : t list -> t

  (**[extract_unique_prefix s1 s2] requires the set [s2] to be
     nonempty. It splits [s1] into two disjoint subsets [head1] and
     [tail1] such that [head1] is exactly the subset of [s1] whose
     elements are less than [minimum s2]. *)
  val extract_unique_prefix : t -> t -> t * t

  (**[extract_shared_prefix s1 s2] splits [s1] and [s2] into three subsets
     [head], [tail1], and [tail2], as follows:

     - [s1] is [head U tail1] and [s2] is [head U tail2].
       This implies that [head] is a subset of both [s1] and [s2].
     - An element of [head] is less than any element of [tail1] or [tail2].
     - [head] is maximal with respect to the previous two properties.

     In summary,
     [head] is the maximal shared prefix of the sets [s1] and [s2]. *)
  val extract_shared_prefix : t -> t -> t * (t * t)

  (**If [xs] is a list of sets then [partition xs] is the coarsest
     partition of the set [U xs] that refines [xs].

     The list [ys] is a partition of the set [U xs] iff:

     - Every set in the list [ys] is nonempty.
     - The sets in the list [ys] are pairwise disjoint.
     - The union of the sets in the list [ys] is [U xs].

     [ys] refines [xs] iff:

     - Every set in the list [xs] is a union of elements of [ys].

     The order of the list [partition xs] is unspecified. *)
  val partition : t list -> t list

end (* SET *)

(**The signature [MINI] describes a subset of operations that are needed
   by [Partition.Make]. *)
module type MINI = sig
  type elt
  type t
  val singleton: elt -> t
  val union: t -> t -> t
  val equal: t -> t -> bool
  val compare: t -> t -> int
end
