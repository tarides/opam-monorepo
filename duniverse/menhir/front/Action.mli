(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Keyword

(**This module defines the type of semantic actions and offers a number of
   operations on this type.*)

(**A semantic action is a piece of code together with information about the
   free variables and keywords that appear in this code.

   The code in a semantic action is normally OCaml code; however, when
   [--rocq] is used, it is Rocq code. This code is represented internally
   as an IL expression.

   An action carries a set of semantic variables [semvars]. These variables
   refer to semantic values and may appear free in the code of the semantic
   action.

   An action carries a set of keywords. These are the keywords that appear
   in the code of the semantic action. They are normally found by the lexer
   while scanning this code.

   An action has an integer priority. This priority influences the order in
   which semantic actions are presented to the OCaml type-checker. This can
   influence the type error messages that appear when an action is ill-typed. *)
type t

(* -------------------------------------------------------------------------- *)

(* Constructors. *)

(**[make priority semvars keywords expr] builds an action out of the
   expression [expr]. [priority] is this action's priority. [semvars] is the
   set of semantic value variables that this action is allowed to mention.
   [keywords] is the list of the keywords that appear in this action. *)
val make: int -> StringSet.t -> keyword list -> IL.expr -> t

(**[compose x a1 a2] builds the action [let x = a1 in a2]. This combinator
   is used during inlining (that is, while eliminating %inlined symbols). *)
val compose : string -> t -> t -> t

(**[bind bvp p x a] binds the OCaml pattern [p] to the OCaml variable [x] in
   the semantic action [a]. Therefore, it builds the action [let p = x in a].
   The list [bvp] must be a list of the variables bound by the pattern [p]. *)
val bind: string list -> IL.pattern -> string -> t -> t

(* -------------------------------------------------------------------------- *)

(* Accessors. *)

(**[expr a] is the code of the semantic action a. *)
val expr: t -> IL.expr

(**[semvars a] is the set of the semantic-value variables that the
   action [a] may mention. *)
val semvars: t -> StringSet.t

(**[keywords a] is the set of keywords used in the action [a]. *)
val keywords: t -> KeywordSet.t

(**[has_beforeend a] tests whether the keyword [$endpos($0)] appears in
   the set [keywords a]. *)
val has_beforeend: t -> bool

(**[posvars a] is the set of conventional position variables that
   correspond to the position keywords used in the action [a]. *)
val posvars: t -> StringSet.t

(**[vars a] is the union of [semvars a] and [posvars a]. *)
val vars: t -> StringSet.t

(**[priority a] is the priority of the action [a]. *)
val priority: t -> int

(* -------------------------------------------------------------------------- *)

(* Keyword expansion. *)

(**[define keyword keywords f a] defines away the keyword [keyword]. This
   keyword is removed from the set of keywords of the action [a]; the set
   [keywords] is added in its place. The code of the action [a] is transformed
   by the function [f], which typically wraps its argument in some new [let]
   bindings. *)
val define: keyword -> KeywordSet.t -> (IL.expr -> IL.expr) -> t -> t

(* -------------------------------------------------------------------------- *)

(* Variable renaming and keyword transformation. *)

(* Some keywords contains names: [$startpos(foo)] is an example. If for some
   reason one wishes to rename the variable [foo] to [bar], then this keyword
   must be renamed to [$startpos(bar)]. Furthermore, during inlining, it may
   be necessary to transform a keyword into another keyword: e.g., if [x] is
   inlined away and replaced with a sequence of [y] and [z], then
   [$startpos(x)] must be renamed to [$startpos(y)] and [$endpos(x)] must be
   renamed to [$endpos(z)]. *)

(**A variable-to-variable substitution is a finite map of variables to
   variables. It can be semantically interpreted as a simultaneous binding
   construct, that is, as a [let/and] construct. *)
type subst

(**The empty substitution. *)
val empty: subst

(**Extending a substitution. *)
val extend: string -> string -> subst -> subst

(**[rename f phi a] transforms the action [a] by applying the renaming [phi]
   as well as the keyword transformations determined by the function [f].

   The function [f] is applied to each (not-yet-renamed) keyword and may
   decide to transform it into another keyword, by returning [Some _], or to
   not transform it, by returning [None]. In the latter case, [phi] still
   applies to the keyword. *)
val rename:
  (subject * where -> (subject * where) option) ->
  subst -> t -> t
