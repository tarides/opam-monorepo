(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

open Input
open GLRAPI

(**[reject()] can be invoked by a semantic action to forbid (cancel)
   the current reduction. *)
val reject: unit -> 'a

module Make (D : DATA) : sig
  open D
  type node = (state, semv) GSS.node

  (**[start state input] runs the parser out of the initial state [state] and
     with the input [input]. It either succeeds and returns a semantic value
     or fails by raising the exception [Error]. *)
  val start : state -> token input -> semv

end
