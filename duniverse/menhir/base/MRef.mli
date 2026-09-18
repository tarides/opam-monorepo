(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**[with_state v action] allocates a fresh reference cell [s],
   initialized with the value [v], then runs [action s], and
   returns the final content of [s]. *)
val with_state : 'a -> ('a ref -> unit) -> 'a
