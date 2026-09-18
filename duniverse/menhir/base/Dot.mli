(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module displays graphs in graphviz dot format. It is much more
   basic than the one bundled with the ocamlgraph library, but offers
   the advantage of being stand-alone. *)

(* ------------------------------------------------------------------------- *)

(* Type definitions. *)

type size =
    float * float (* in inches *)

type orientation =
  | Portrait
  | Landscape

type rankdir =
  | LeftToRight
  | TopToBottom

type ratio =
  | Compress
  | Fill
  | Auto

type style =

    (* Both nodes and edges. *)

  | Solid
  | Dashed
  | Dotted
  | Bold
  | Invisible

    (* Nodes only. *)

  | Filled
  | Diagonals
  | Rounded

type shape =
  | Box
  | Oval
  | Circle
  | DoubleCircle
  | Record
      (* there are many others, let's stop here *)

(* ------------------------------------------------------------------------- *)

(* The graph printer. *)

module Print (G : sig

  type vertex

  (**[name] maps a vertex to an internal name. *)
  val name: vertex -> string

  (**[successors] enumerates the outgoing edges of a vertex. Each edge is
     optionally decorated with a style, and must carry a (visible) label. *)
  val successors:
    (?style:style -> label:string -> vertex -> unit) ->
    vertex -> unit

  (**[iter] enumerates the vertices. Each vertex is optionally decorated with
     a shape and a style, and must carry a (visible) label. *)
  val iter:
    (?shape:shape -> ?style:style -> label:string -> vertex -> unit) ->
    unit

end) : sig

  (**[print ... f] writes a description of the graph [G] to the output
     channel [f] in [.dot] format. *)
  val print:
      ?directed: bool ->
      ?size: size ->
      ?orientation: orientation ->
      ?rankdir: rankdir ->
      ?ratio: ratio ->
      out_channel ->
      unit

end
