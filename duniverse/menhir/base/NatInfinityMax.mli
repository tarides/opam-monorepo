(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This is the lattice of the natural numbers, completed with [infinity], and
   ordered towards infinity: that is, {!bottom} is zero; [top] is infinity.

   This lattice has unbounded height, so a fixed point computation in
   this lattice does not necessarily terminate. *)

type property
type t = property

(**[bottom] is the number zero. *)
val bottom: property

(**[infinity] is the number [+∞]. It is the top element of the lattice. *)
val infinity: property

(**[finite n] is the number [n]. *)
val finite: int -> property

(**[equal] tests whether two numbers are equal. *)
val equal: property -> property -> bool

(**[is_maximal] is true of [infinity] and false of any other number. *)
val is_maximal: property -> bool

(**[max] computes the maximum of two numbers. It is the join operation
   of the lattice. *)
val max: property -> property -> property

(**[max_lazy] is a lazy variant of {!max}.
   If its first argument is {!infinity} then it returns {!infinity}
   without evaluating its second argument. *)
val max_lazy: property -> (unit -> property) -> property

(**[add] is the addition of two numbers. *)
val add: property -> property -> property

(**[add_lazy] is a lazy variant of {!add}.
   If its first argument is {!infinity} then it returns {!infinity}
   without evaluating its second argument.*)
val add_lazy: property -> (unit -> property) -> property

(**[print] converts a number to a string in a currently unspecified
   human-readable format. *)
val print: property -> string

(**[to_int] converts a number to an ordinary integer.
   {!infinity} is converted to [max_int]. *)
val to_int: property -> int
