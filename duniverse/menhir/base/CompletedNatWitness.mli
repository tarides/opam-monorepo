(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This is the lattice of the natural numbers, completed with [Infinity],
   ordered towards zero (i.e. [Infinity] is [bottom], [Finite 0] is [top]),
   and enriched with sequences of matching length. *)

(**A lattice element is either [Finite (n, xs)], where [n] is a natural number
   and [xs] is a sequence of length [n]; or [Infinity]. The sequence [xs] is
   ignored by the ordering but is nevertheless constructed: for example,
   {!add} concatenates two sequences. The sequence [xs] should be thought of
   as a witness, or proof, that explains why the number [n] was obtained. *)
type 'a t =
| Finite of int * 'a CatSeq.seq
| Infinity

(**[bottom] is the number [Infinity]. *)
val bottom: 'a t

(**[epsilon] is the number zero and the empty sequence.
   It is the top element of the lattice. *)
val epsilon: 'a t

(**[singleton x] is the number one and the singleton sequence [[x]]. *)
val singleton: 'a -> 'a t

(**[equal] compares two numbers. The sequences are ignored. *)
val equal: 'a t -> 'b t -> bool

(**[is_maximal] is true of {!epsilon} and false of other numbers. *)
val is_maximal: 'a t -> bool

(**[min] is minimum in the sense of the usual ordering on numbers.
   It is the join operation of the lattice. *)
val min: 'a t -> 'a t -> 'a t

(**[min_lazy] is a lazy variant of {!min}.
   If its first argument is {!epsilon} then it returns {!epsilon}
   without evaluating its second argument. *)
val min_lazy: 'a t -> (unit -> 'a t) -> 'a t

(**[add] performs the addition of two numbers
   and the concatenation of the corresponding sequences. *)
val add: 'a t -> 'a t -> 'a t

(**[add_lazy] is a lazy variant of {!add}.
   If its first argument is {!bottom} then it returns {!bottom}
   without evaluating its second argument. *)
val add_lazy: 'a t -> (unit -> 'a t) -> 'a t

(**[print] converts a number (and its sequence) to a string in a currently
   unspecified human-readable format. *)
val print: ('a -> string) -> 'a t -> string

(**[to_int] converts a number to an ordinary integer.
   [Infinity] is converted to [max_int]. *)
val to_int: 'a t -> int

(**[extract] converts a number to a list,
   by extracting the underlying sequence.
   This number must not be {!bottom}. *)
val extract: 'a t -> 'a list
