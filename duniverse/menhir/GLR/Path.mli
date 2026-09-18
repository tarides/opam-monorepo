(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

open GSS

(**A path is either [Empty top], where [top] is a top node,
   or [Edge (edge, path)], where [edge] is the last edge of
   the path. The last edge is also the leftmost edge. It is
   accessible in time O(1), whereas the rightmost edge is
   accessible in linear time. *)
type ('s, 'v) t =
  | Empty of ('s, 'v) node
  | Edge of ('s, 'v) edge * ('s, 'v) t

(**[empty top] is an empty path, starting at the top node [top]. *)
val empty: ('s, 'v) node -> ('s, 'v) t

(**[grow edge path] extends the path [path] with the edge [edge].
   This edge is the last (leftmost) edge of the new path. *)
val grow: ('s, 'v) edge -> ('s, 'v) t -> ('s, 'v) t

(**[leftmost path] is the leftmost node of the path [path]. *)
val leftmost: ('s, 'v) t -> ('s, 'v) node

(**[rightmost path] is the rightmost node of the path [path]. *)
val rightmost: ('s, 'v) t -> ('s, 'v) node (* currently unused *)

(**[extract] extracts the unique edge of a path of length 1. *)
val extract: ('s, 'v) t -> ('s, 'v) edge

(**[length path] returns the length of the path [path], measured in edges.
   It is used for debugging only. *)
val length: ('s, 'v) t -> int

(**[lock path] locks every edge along the path [path]. *)
val lock: ('s, 'v) t -> unit
