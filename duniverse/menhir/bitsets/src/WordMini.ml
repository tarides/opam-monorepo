(******************************************************************************)
(*                                                                            *)
(*                                  Bitsets                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*       Copyright 2025--2025 Inria. All rights reserved. This file is        *)
(*       distributed under the terms of the GNU Library General Public        *)
(*       License, with an exception, as described in the file LICENSE.        *)
(*                                                                            *)
(******************************************************************************)

type t =
  int

type elt =
  int

let bound =
  Sys.word_size - 1

let[@inline] bit i =
  assert (0 <= i && i < bound);
  1 lsl i

let singleton =
  bit

let[@inline] union s1 s2 =
  s1 lor s2

let[@inline] equal (s1 : int) (s2 : t) : bool =
  s1 = s2       (* generic equality, used at type [int] *)

let[@inline] compare (s1 : int) (s2 : t) : int =
  compare s1 s2 (* generic ordering, used at type [int] *)
