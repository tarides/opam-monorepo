(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module offers a simple implementation of persistent sequences with
   constant-time concatenation and linear-time conversion to a list. *)

(**The abstract type of sequences. *)
type 'a seq

(**[empty] is the empty sequence. *)
val empty: 'a seq

(**[singleton x] is the singleton sequence whose single element is [x]. *)
val singleton: 'a -> 'a seq

(**[append] concatenates two sequences, producing a sequence. *)
val append: 'a seq -> 'a seq -> 'a seq

(**[concat] concatenates a list of sequences, producing a sequence. *)
val concat: 'a seq list -> 'a seq

(**[elements] converts a sequence to a list. *)
val elements: 'a seq -> 'a list

(**[first xs] returns the first element of the sequence [xs]. This sequence
   must be nonempty. *)
val first: 'a seq -> 'a
