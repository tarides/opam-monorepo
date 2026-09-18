(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(**This module is a stripped-down version of [Base.Vector]. It contains just
   the functionality that is needed by [PriorityQueue], which itself is used
   by [GLR]. *)

(**The type of a vector. *)
type 'a vector

(**A synonym for the type of a vector. *)
type 'a t = 'a vector

(**An integer value of type [length] represents the length of a sequence.
   For example, it can be the length of an array, the length of a vector,
   or the length of a segment of an array of vector. A length is nonnegative. *)
type length = int

(**An integer value of type [index] represents an index into a sequence of
   elements.*)
type index = int

(** {1:creating Creating} *)

(**[create()] creates a new vector of length 0 and capacity 0. *)
val create : unit -> 'a vector

(**[unsafe_get v i] fetches the element that lies at index [i] in the vector
   [v]. [i] must be comprised in the semi-open interval [\[0, length v)]. {b
   No bounds check is performed.} If the index [i] is out of bounds, memory
   safety can be compromised. Use at your own risk! *)
val unsafe_get : 'a vector -> index -> 'a

(**[unsafe_set v i x] overwrites the element that lies at index [i]
   in the vector [v] with the value [x].
   [i] must be comprised in the semi-open interval [\[0, length v)].
   {b No bounds check is performed.} If the index [i] is out of bounds,
   memory safety can be compromised. Use at your own risk! *)
val unsafe_set : 'a vector -> index -> 'a -> unit

(**[push v x] extends the vector [v] with the element [x]. The length of the
   vector [v] is increased by one. If necessary, the capacity of the vector
   [v] is increased. *)
val push : 'a vector -> 'a -> unit

(**If the vector [v] is nonempty, [pop v] removes and returns its last
   element. The length of the vector is decreased by one; its capacity is
   unchanged. If the vector [v] is empty, [pop v] raises [Not_found]. *)
val pop : 'a vector -> 'a

(**[length v] is the (logical) length of the vector [v]. *)
val length : 'a vector -> length
