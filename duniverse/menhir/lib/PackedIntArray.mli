(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(**This module allows packing an array of (small) integers inside a string,
   using less than one word of memory per array element. *)

(**A packed integer array is represented as a pair of an integer [k] and
   a string [s]. The integer [k] is the number of bits per integer that we
   use. The string [s] is just an array of bits, which is read in 8-bit
   chunks.

   The OCaml programming language treats string literals and array literals
   in slightly different ways: the former are statically allocated, while
   the latter are dynamically allocated. (This is rather arbitrary.) In the
   context of Menhir's table-based back-end, where compact, immutable
   integer arrays are needed, ocaml strings are preferable to ocaml arrays. *)
type t =
  int * string

(**[pack a] turns an array of integers into a packed integer array.

   Because the sign bit is the most significant bit, the magnitude of any
   negative number is the word size. In other words, as soon as the array
   [a] contains any negative numbers, [pack] does not achieve any space
   savings. *)
val pack: int array -> t

(* [get] is now commented out, as it is no longer used.
(**[get t i] returns the integer stored in the packed array [t] at index [i].
   Together, [pack] and [get] satisfy the following property: if the index [i]
   is within bounds, then [get (pack a) i] equals [a.(i)]. *)
val get: t -> int -> int
 *)

(**[get1 s i] returns the integer stored in the packed array [s] at index [i].
   It assumes (and does not check) that the array's bit width is [1]. The
   packed array [s] is just a string; there is no bit width component.
   In other words, it is [get], specialized to the bit width 1. *)
val get1: string -> int -> int

(**[get2] is [get], specialized to the bit width 2. *)
val get2: string -> int -> int

(**[get4] is [get], specialized to the bit width 4. *)
val get4: string -> int -> int

(**[get8] is [get], specialized to the bit width 8. *)
val get8: string -> int -> int

(**[get16] is [get], specialized to the bit width 16. *)
val get16: string -> int -> int

(**[get32] is [get], specialized to the bit width 32. *)
val get32: string -> int -> int
