(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module offers maps whose keys are strings. *)

include Map.S
  with type key = string
   and type 'a t = 'a Map.Make(String).t

(**[of_list bs] adds the bindings of [bs] to the empty map, in list order. If
   a key is bound twice in [bs] then the last binding takes over. *)
val of_list : (key * 'a) list -> 'a t

(**[restrict s m] restricts the domain of the map [m] to (its
   intersection with) the set [s]. *)
val restrict: StringSet.t -> 'a t -> 'a t

(**[domain m] returns the domain of the map [m]. *)
val domain: 'a t -> StringSet.t

(**[support m] is the union of the domain and codomain of the map [m]. *)
val support: string t -> StringSet.t

(**[multiple_add k v m] adds the key-value pair [k, v] to the map [m],
   which maps keys to *lists* of values. The list currently associated
   with [k] is extended with the value [v]. *)
val multiple_add: key -> 'a -> 'a list t -> 'a list t
