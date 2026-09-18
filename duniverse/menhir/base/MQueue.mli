(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**[repeat q yield] repeatedly extracts an element [x] out of the queue [q]
   and invokes [yield x] until [q] becomes empty. The function call [yield x]
   can insert new elements into [q]. *)
val repeat: 'a Queue.t -> ('a -> unit) -> unit
