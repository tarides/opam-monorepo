(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Printf

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

(* Basic printers. *)

let print_style style =
  match style with
  | Solid ->
      "solid"
  | Dashed ->
      "dashed"
  | Dotted ->
      "dotted"
  | Bold ->
      "bold"
  | Invisible ->
      "invis"
  | Filled ->
      "filled"
  | Diagonals ->
      "diagonals"
  | Rounded ->
      "rounded"

let print_style_option = function
  | None ->
      ""
  | Some style ->
      sprintf ", style = %s" (print_style style)

let print_shape shape =
  match shape with
  | Box ->
      "box"
  | Oval ->
      "oval"
  | Circle ->
      "circle"
  | DoubleCircle ->
      "doublecircle"
  | Record ->
      "record"

let print_shape_option = function
  | None ->
      ""
  | Some shape ->
      sprintf ", shape = %s" (print_shape shape)

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

end) = struct

  let print_size f size =
    size |> Option.iter @@ fun (hsize, vsize) ->
    fprintf f "size=\"%f, %f\";\n" hsize vsize

  let print_orientation f orientation =
    match orientation with
    | Portrait ->
        fprintf f "orientation = portrait;\n"
    | Landscape ->
        fprintf f "orientation = landscape;\n"

  let print_rankdir f rankdir =
    match rankdir with
    | LeftToRight ->
        fprintf f "rankdir = LR;\n"
    | TopToBottom ->
        fprintf f "rankdir = TB;\n"

  let print_ratio f ratio =
    match ratio with
    | Compress ->
        fprintf f "ratio = compress;\n"
    | Fill ->
        fprintf f "ratio = fill;\n"
    | Auto ->
        fprintf f "ratio = auto;\n"

  let print_vertices f =
    G.iter @@ fun ?shape ?style ~label vertex ->
    fprintf f "%s [ label=\"%s\"%s%s ] ;\n"
      (G.name vertex)
      label
      (print_style_option style)
      (print_shape_option shape)

  let print_edges f directed =
    G.iter @@ fun ?shape ?style ~label source ->
    ignore (shape, style, label);
    source |> G.successors @@ fun ?style ~label destination ->
    fprintf f "%s %s %s [ label=\"%s\"%s ] ;\n"
      (G.name source)
      (if directed then "->" else "--")
      (G.name destination)
      label
      (print_style_option style)

  let print ?(directed = true) ?size ?(orientation = Landscape)
            ?(rankdir = LeftToRight) ?(ratio = Compress) f =

    fprintf f "%s G {\n" (if directed then "digraph" else "graph");
    print_size f size;
    print_orientation f orientation;
    print_rankdir f rankdir;
    print_ratio f ratio;
    print_vertices f;
    print_edges f directed;
    fprintf f "\n}\n"

end
