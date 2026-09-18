(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module implements conceptually infinite mutable arrays, where all
   indices (except a finite number of indices) are mapped to the same value.

   An infinite array is represented internally as a finite array that grows
   transparently when needed. *)

(**The type of infinite arrays. **)
type 'a t

(**[make x] creates an infinite array, where every slot contains [x]. **)
val make: 'a -> 'a t

(**[get a i] returns the element found at index [i] in the array [a]. **)
val get: 'a t -> int -> 'a

(**[set a i x] sets the element at index [i] in the array [a] to [x]. **)
val set: 'a t -> int -> 'a -> unit

(**[extent a] is the length of an initial segment of the array [a] that is
   sufficiently large to contain all [set] operations ever performed. In other
   words, it must be the case that all indices greater than or equal to
   [extent a] are mapped to the default value that was passed as an argument
   to [make] when the array [a] was created. *)
val extent: 'a t -> int

(**[domain a] is a fresh copy of an initial segment of the array [a]
   whose length is [extent a]. *)
val domain: 'a t -> 'a array
