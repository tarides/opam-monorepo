(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module offers linear derivation forests, a data structure that
   plays a role in the explanation of conflicts. *)

open MiddleAPI

module Make (G : GRAMMAR) : sig
open G

(**The type [t] represents a linear derivation forest.

   The word {i linear} suggests that this data structure is really list-like,
   as opposed to tree-like: that is, a single path into the data structure is
   of interest.

   The structure of forests can be described by defining {i trees} and
   {i forests} as two mutually inductive data types. Only forests appear
   in the public API.

   A {b tree} is either empty or formed of a non-terminal symbol at the root
   and a forest below the root.

   A {b forest} is an ordered list of elements. Exactly one element of this
   forest receives focus and is a tree. The other elements remain unexpanded,
   so they are just symbols. *)
type t

(**The type [context] represents a context in a derivation forest, that is, a
   derivation forest with a hole that expects to be filled with a derivation
   forest. *)
type context

(** {2 Construction} *)

(**The forest [empty] consists of a single empty tree. *)
val empty: t

(**[tail i rhs] is the forest whose first element is the empty tree and whose
   remaining elements are the symbols found at offsets greater than or equal
   to [i] in the array [rhs]. The offset [i] must be less than the length of
   the array [rhs], which means that it describes a suffix of length at least
   one. *)
val tail: int -> Symbol.t array -> t

(**[build i rhs forest comment] is the forest whose first element is the tree
   defined by the root symbol [rhs.(i)] and by the child forest [forest] and
   whose remaining elements are the symbols found at offsets greater than [i]
   in the array [rhs]. The symbol [rhs.(i)] must be a nonterminal symbol. The
   optional [comment] is displayed by the functions [print] and [printc] next
   to this forest. *)
val build: int -> Symbol.t array -> t -> string option -> t

(**[prepend symbol forest] is the forest whose first element is the symbol
   [symbol] and whose remaining elements form the forest [forest]. *)
val prepend: Symbol.t -> t -> t

(** {2 Factoring} *)

(**[factor] factors the maximal common derivation context out of a nonempty
   family of derivations. It produces a pair of the common context and of the
   residual derivations. *)
val factor: t Item.Map.t -> context * t Item.Map.t

(** {2 Display} *)

(**[print] prints a derivation forest. *)
val print: out_channel -> t -> unit

(**[printc] prints a derivation context. *)
val printc: out_channel -> context -> unit

end
