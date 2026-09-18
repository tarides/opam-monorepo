(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module offers sets of strings. *)

include Set.S
  with type elt = string
   and type t = Set.Make(String).t

(**[print] prints a set of strings as a comma-separated list,
   without opening and closing delimiters. *)
val print: t -> string

(**[big_union] computes the union of a list of sets. *)
val big_union: t list -> t
