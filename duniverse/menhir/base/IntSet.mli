(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module offers an implementation of sets based on Patricia trees.
   It is not super-efficient: sets are implemented in terms of maps, so
   one word of memory per node is wasted. *)

(**The set API is described by the signature [BaseAPI.SET]. *)
include BaseAPI.SET with type elt = int

(**The conversions between sets and maps are described by
   the signature [BaseAPI.CONVERT]. *)
include BaseAPI.CONVERT
  with type key := int
   and type 'a map := 'a Patricia.t
   and type set := t
