(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module lets us test the GLR reference interpreter. *)

(**[test()] tests the GLR reference interpreter with a number of randomly
   generated sentences of increasing sizes. *)
val test: unit -> unit

(**[test()] runs a benchmark of the GLR reference interpreter, using a number
   of randomly generated sentences of increasing sizes, and prints data on the
   standard output channel. *)
val benchmark: unit -> unit
