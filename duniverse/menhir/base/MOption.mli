(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**[hash] is a hash function for options. *)
val hash: ('a -> int) -> 'a option -> int

(**[split] transforms an optional pair into a pair of options. *)
val split: ('a * 'b) option -> 'a option * 'b option

(**If [(<=)] is an ordering on elements
   then [sub (<=)] is an ordering on optional elements.
   It is defined as follows: [None] is less than [Some x];
   and [Some x1 <= Some x2] holds iff [x1 <= x2] holds. *)
val sub: ('a -> 'b -> bool) -> 'a option -> 'b option -> bool

(** [fold none some o] is [none] if [o] is [None] and
    is [some v] if [o] is [Some v]. *)
val fold: ('a -> 'b -> 'b) -> 'a option -> 'b -> 'b
