(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module provides utilities that are shared by the two versions of the
   parser. *)

open Lexing
open Syntax

(* A few types used in the parser. *)

type early_producer =
  range *
  identifier located option *
  parameter *
  attributes

type early_producers =
  early_producer list

type early_production =
  early_producers *
  string located option * (* optional precedence *)
  production_level *
  range

type early_productions =
  early_production list

(**[new_precedence_level (pos1, pos2)] creates a new precendence level, which is
   stronger than any levels previously created by this function. It should be
   called every time a [%left], [%right], or [%nonassoc] declaration is found.
   The positions are the positions of this declaration in the source code. The
   precedence levels created by this function are attached to tokens and (via
   %prec) to productions. They are used to resolve shift/reduce conflicts. *)
val new_precedence_level: position * position -> precedence_level

(**[new_production_level()] creates a new production level, which is stronger
   than any levels previously created by this function. It should be called
   every time a new production is found. The production levels created by this
   function are attached to productions. They are used to resolve reduce/reduce
   conflicts: following ocamlyacc and bison, the production that appears first
   in the grammar receives preference. Note that %prec annotations do {i not}
   influence the resolution of reduce/reduce conflicts. *)
val new_production_level: unit -> production_level

(**[new_on_error_reduce_level()] creates a new level, which is attached to an
   [%on_error_reduce] declaration. *)
val new_on_error_reduce_level: unit -> on_error_reduce_level

(**[check_production_group] accepts a production group and checks that all
   productions in the group define the same set of identifiers. *)
val check_production_group: early_productions -> unit

(**[normalize_producers] accepts a list of producers where identifiers are
   optional and returns a list of producers where identifiers are mandatory.
   A missing identifier in the [i]-th position receives the conventional
   name [_i]. *)
val normalize_producers: early_producers -> producer list

(**[override range oprec1 oprec2] decides which of the two optional %prec
   declarations [oprec1] and [oprec2] applies to a production. If both are
   present then a fatal error occurs. *)
val override: range -> 'a option -> 'a option -> 'a option

(**[producer_names producers] returns an array [names] such that
   [names.(idx) = None] if the (idx + 1)-th producer is unnamed
   and [names.(idx) = Some id] if it is called [id]. *)
val producer_names: early_producers -> identifier option array

(**[validate_pointfree_action] checks that a text fragment represents valid
   content for a point-free semantic action. That is, this fragment must be
   either just whitespace, or an OCaml lowercase or uppercase identifier.
   The exception [Lexpointfree.InvalidPointFreeAction] is raised if the
   fragment is invalid. [None] is returned if the text fragment is empty;
   [Some _] is returned if it is nonempty. *)
val validate_pointfree_action: string located -> string located option

(**[valid_ocaml_identifier x] determines whether the string [x] is a valid
   OCaml lowercase identifier. *)
val valid_ocaml_identifier: identifier located -> bool

(**The flag [dollars] determines whether the notation [$i] in semantic actions
   is allowed or disallowed. It is mutable and can be set from the outside.
   This is dirty but convenient. A more principled approach would be to delay
   the well-formedness check for semantic actions (which is the place where
   this setting is needed) so that it takes place after parsing instead of
   during parsing. *)
val dollars : [`DollarsDisallowed | `DollarsAllowed] ref
