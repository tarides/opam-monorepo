(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This modules defines the syntax of sorts. *)

(**The syntax of sorts, on paper, would be [sort ::= (sort, ..., sort) -> *].
   The arity of the sort [arrow sorts] is the length of the list [sorts].
   This arity can be zero. *)
type sort

(**[star] is the base sort. It is the sort of arity zero, [arrow []]. *)
val star: sort

(**[arrow sorts] constructs a sort whose domain is the list [sorts]. *)
val arrow: sort list -> sort

(**The domain of the sort [arrow sorts] is the list [sorts]. *)
val domain: sort -> sort list

(**A sort assignment is a map of (terminal and nonterminal) symbols
   to sorts. *)
type sorts =
  sort StringMap.t
