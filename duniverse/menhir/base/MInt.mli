(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module provides a number of utility functions that have something to
   do with integers. *)

(**[preincrement r] increments the integer reference [r] and returns its
   updated value. *)
val preincrement: int ref -> int

(**[postincrement r] increments the integer reference [r] and returns its
   original value. *)
val postincrement: int ref -> int

(**[predecrement r] decrements the integer reference [r] and returns its
   updated value. *)
val predecrement: int ref -> int

(**[postdecrement r] decrements the integer reference [r] and returns its
   original value. *)
val postdecrement: int ref -> int

(**[mkgensym()] returns a fresh generator of unique integers. *)
val mkgensym: unit -> (unit -> int)

(**[with_counter action] creates a fresh reference [c] whose
   initial value is zero, invokes [action c], and returns [!c]. *)
val with_counter: (int ref -> unit) -> int

(**[sum n f] computes the sum [f 0 + f 1 + ... + f (n-1)]. *)
val sum: int -> (int -> int) -> int

(**[interval i j] returns the semi-open interval \[[i, j)]. *)
val interval: int -> int -> int list

(**[iteri j] enumerates the semi-open interval \[[0, j)]. Thus
   [iteri j f]    is equivalent to [for x = 0 to j-1 do f x done]. *)
val iteri:         int -> (int -> unit) -> unit

(**[iterij i j] enumerates the semi-open interval \[[i, j)]. Thus
   [iterij i j f] is equivalent to [for x = i to j-1 do f x done]. *)
val iterij: int -> int -> (int -> unit) -> unit

(**[foldi j] enumerates the semi-open interval \[[0, j)]. *)
val foldi:              int -> (int ->          'a  -> 'a) -> 'a -> 'a

(**[foldij i j] enumerates the semi-open interval \[[i, j)]. *)
val foldij:      int -> int -> (int ->          'a  -> 'a) -> 'a -> 'a

(**[foldij_lazy i j] enumerates the semi-open interval \[[i, j)].

   It is interruptible: if at some point the function [f] does not
   demand its second argument, then iteration stops early.

   [foldij] and [foldij_lazy] iterate in the same direction, from left
   to right, but do not build the accumulator in the same way: the calls
   to [f] are associated differently. In that respect, [foldij] is a
   left fold, while [foldij_lazy] is a right fold. *)
val foldij_lazy: int -> int -> (int -> (unit -> 'a) -> 'a) -> 'a -> 'a

(**[mapi j f] produces the list [ f 0; ...; f (j-1) ]. *)
val mapi:         int -> (int -> 'a) -> 'a list

(**[mapij i j f] produces the list [ f i; ...; f (j-1) ]. *)
val mapij: int -> int -> (int -> 'a) -> 'a list

(**[initi j f] produces the array [ f 0; ...; f (j-1) ].
   It is the same as [Array.init] except it allows [j] to be negative;
   then it returns an empty array. *)
val initi:         int -> (int -> 'a) -> 'a array

(**[initij i j f] produces the array [ f i; ...; f (j-1) ]. *)
val initij: int -> int -> (int -> 'a) -> 'a array
