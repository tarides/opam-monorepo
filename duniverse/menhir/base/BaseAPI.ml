(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This signature is satisfied by the module [IntSet] and is used in Menhir to
   describe the modules [Item.Set]. *)
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

  (** {1 Extraction} *)

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

end (* SET *)

(**This signature is satisfied by the module [Patricia] and is used in Menhir
   to describe the modules [TerminalMap], [NonterminalMap], etc. *)
module type MAP = sig

  (**A key. *)
  type key

  (**A map. *)
  type 'a t

  (**The empty map. *)
  val empty: 'a t

  (**[is_empty m] determines whether the map [m] is empty, that is,
     whether its cardinal is 0. *)
  val is_empty: 'a t -> bool

  (**[singleton k v] returns a map containing just a binding of the key [k] to
     the value [v]. *)
  val singleton: key -> 'a -> 'a t

  (**[is_singleton m] determines whether the map [m] is a singleton map,
     that is, whether its cardinal is 1. *)
  val is_singleton: 'a t -> bool

  (**[add k v m] returns a map whose bindings are all bindings in the map [m]
     plus a binding of the key [k] to the value [v]. If a binding already exists
     for the key [k] then it is overridden. *)
  val add: key -> 'a -> 'a t -> 'a t

  (**[mem k m] determines whether the map [m] contains a binding of the key [k]
     to some value. *)
  val mem: key -> 'a t -> bool

  (**[find k m] finds the value associated to the key [k] in the map [m]. If the
     map [m] contains no binding for the key [k] then [Not_found] is raised. *)
  val find: key -> 'a t -> 'a

  (**[remove k m] is the map [m] deprived from any binding for [k]. *)
  val remove: key -> 'a t -> 'a t

  (**[find_and_remove k m] finds the value [v] associated to the key [k] in the
     map [m] and returns a pair of the value [v] and the map [remove k m]. If
     the map [m] contains no binding for the key [k] then [Not_found] is
     raised. *)
  val find_and_remove: key -> 'a t -> 'a * 'a t

  (**[union m1 m2] returns the union of the maps [m1] and [m2]. If a key [k]
     appears in [m1] and in [m2] then its binding in [m2] is retained. *)
  val union: 'a t -> 'a t -> 'a t

  (**If the map [m] is nonempty then [choose m] returns an arbitrarily chosen
     binding in [m]. Otherwise, [Not_found] is raised. *)
  val choose: 'a t -> key * 'a

  (**[cardinal m] returns the cardinal of the map [m], that is, the number of
     bindings in the map [m]. *)
  val cardinal: 'a t -> int

  (**[iter yield m] enumerates the bindings in the map [m] in increasing order
     of keys. *)
  val iter: (key -> 'a -> unit) -> 'a t -> unit

  (**[fold yield m s] enumerates the bindings in the map [m] in increasing order
     of keys. *)
  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  (**[fold_rev yield m s] enumerates the bindings in the map [m] in decreasing
     order of keys. *)
  val fold_rev: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  (**[exists f m] checks whether at least one binding in the map [m] satisfies
     the predicate [f]. *)
  val exists: (key -> 'a -> bool) -> 'a t -> bool

  (**[map f] applies the transformation [f] to the values contained in the map
     [m]. *)
  val map: ('a -> 'b) -> 'a t -> 'b t

  (**[filter f m] produces a map containing just the bindings of the map [m]
     that satisfy the predicate [f]. *)
  val filter: (key -> 'a -> bool) -> 'a t -> 'a t

end (* MAP *)

module type CONVERT = sig

  type key
  type set
  type 'a map

  (**[domain m] is the set of the keys for which a binding exists in
     the map [m]. *)
  val domain: 'a map -> set

  (**[lift f s] constructs a map whose domain is the set [s] and which
     contains a binding of every key [k] in the set [s] to the value
     [f k]. *)
  val lift: (key -> 'a) -> set -> 'a map

end

(**This signature is satisfied by the module [IntMap.Unbounded]. *)
module type MAP_AND_SET = sig

  (**Maps. *)
  include MAP

  (**Sets. *)
  module Domain : SET with type elt = key

  (**Conversions. *)
  include CONVERT
      with type key := key
      and type 'a map := 'a t
      and type set := Domain.t

end (* MAP_AND_SET *)

(**This signature is satisfied by the module [IntMap.Make(N)()]. *)
module type MAP_AND_BITSET = sig

  (**Maps. *)
  include MAP

  (**Sets. *)
  module Domain : Bitsets.API.SET with type elt = key

  (**Conversions. *)
  include CONVERT
      with type key := key
      and type 'a map := 'a t
      and type set := Domain.t

end (* MAP_AND_BITSET *)
