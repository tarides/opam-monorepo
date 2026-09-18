(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Range

(**A value of type ['a located] is a pair of a value of type ['a]
   and a position of type [range]. In other words, it is a value
   of type ['a], decorated with a position of type [range]. *)
type 'a located

(**[locate p v] constructs a pair of the position [p] and value [v].
   In other words, it decorates the value [v] with the position [p]. *)
val locate: range -> 'a -> 'a located

(**[value] is the first pair projection.
   In other words, it maps a decorated value to a value. *)
val value: 'a located -> 'a

(**[position] is the second pair projection.
   In other words, it maps a decorated value to a position. *)
val position: 'a located -> range

(**[map f] applies the transformation [f] to a decorated value
   while preserving its decoration. *)
val map: ('a -> 'b) -> 'a located -> 'b located

(**[parenthesize s] surrounds the decorated string [s] with a pair
   of parentheses and decrements its range so that the pre-existing
   content is still printed at the same column. *)
val parenthesize: string located -> string located
