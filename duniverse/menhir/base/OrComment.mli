(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module defines a sum type: either something or a comment (a string).  *)

type comment =
  string

type 'a or_comment =
  | Thing of 'a
  | Comment of comment

type 'a t =
  'a or_comment

(**[iter] enumerates the things (of which there are zero or one). *)
val iter : ('a -> unit) -> 'a t -> unit

(**[fold] enumerates the things (of which there are zero or one). *)
val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

(**[fold] transforms the things (of which there are zero or one). *)
val map : ('a -> 'b) -> 'a t -> 'b t

(**[things xs] keeps the things in the list [xs], throwing away the
   comments. *)
val things: 'a t list -> 'a list

(**[count] counts the things in a list of things or comments. *)
val count : 'a t list -> int

(**[gather] turns a list of things and comments into a list of
   things-followed-with-comments. Any leading comments are silently lost. *)
val gather : 'a t list -> ('a * comment list) list

open Validate

(**[validate] builds a validator. *)
val validate : ('a, 'b) validator -> ('a t, 'b t) validator
