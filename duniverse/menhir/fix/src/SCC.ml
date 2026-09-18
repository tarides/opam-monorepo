(******************************************************************************)
(*                                                                            *)
(*                                    Fix                                     *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Library General Public License version 2, with a         *)
(*  special exception on linking, as described in the file LICENSE.           *)
(*                                                                            *)
(******************************************************************************)

module Run (G : sig

  (**The type of vertices, or nodes. *)
    type node

  (**[n] is the number of nodes in the graph. *)
  val n: int

  (**Each node must have a unique integer index. These indices must range
     from 0 (included) to [n] (excluded). The function [index] maps a node
     to its integer index. *)
  val index: node -> int

  (**[iter] enumerates all nodes in the graph. That is, [iter yield]
     applies the function [yield] to every node in the graph. *)
  val iter: (node -> unit) -> unit

  (**[successors] enumerates the immediate successors of a node. That is, if
     [v] is a node then [successors yield v] applies the function [yield] to
     every node [w] such that there exists an edge from [v] to [w]. *)
  val successors: (node -> unit) -> node -> unit

end) = struct

  (* Define the internal data structure associated with each node. *)

  type data = {

      mutable stacked: bool;
      (**Each node carries a flag which tells whether it appears
         within the SCC stack (which is defined below). *)

      mutable number: int;
      (**Each node carries a number. Numbers represent the order in
         which nodes were discovered. *)

      mutable low: int;
      (**Each node [x] records the lowest number associated to a node
         already detected within [x]'s SCC. *)

      mutable representative: G.node;
      (**Each node carries a pointer to a representative element of
         its SCC. This field is used by the algorithm to store its
         results. *)

      mutable scc: G.node list
      (**Each representative node carries a list of the nodes in
         its SCC. This field is used by the algorithm to store its
         results. *)

    }

  (* Define a mapping from external nodes to internal ones. Here, we
     simply use each node's index as an entry into a global array. *)

  let table : G.node -> data =

    (* Create the array. We initially fill it with [None], of type
       [data option], because we have no meaningful initial value of
       type [data] at hand. *)

    let table = Array.make G.n None in

    (* Initialize the array. *)

    G.iter (fun x ->
      table.(G.index x) <- Some {
        stacked = false;
        number = 0;
        low = 0;
        representative = x;
        scc = []
      }
    );

    (* Define a function which gives easy access to the array. It maps
       each node to its associated piece of internal data. *)

    function x ->
      match table.(G.index x) with Some dx -> dx | None -> assert false

  (* Create an empty stack, used to record all nodes which belong to
     the current SCC. *)

  let scc_stack : data Stack.t =
    Stack.create()

  (* Initialize a function which allocates numbers for (internal) nodes.
     A new number is assigned to each node the first time it is visited.
     The numbers returned by this function start at 1 and increase.
     Initially, all nodes have number 0, so they are considered
     unvisited. *)

  let mark : data -> unit =
    let counter = ref 0 in
    fun dx ->
      incr counter;
      dx.number <- !counter;
      dx.low <- !counter

  (* This reference will hold a list of all representative nodes.
     The components that have been identified last appear at the
     head of the list. *)

  let representatives : G.node list ref =
    ref []

  (* [walk] performs the depth-first search traversal
     that forms the heart of the algorithm. *)

  let rec walk x =
    let dx = table x in

    G.successors (fun y ->
      let dy = table y in

      if dy.number = 0 then begin

        (* [y] hasn't been visited yet, so [(x, y)] is a regular
           edge, part of the search forest. *)

        mark dy;
        dy.stacked <- true;
        Stack.push dy scc_stack;

        (* Continue walking, depth-first. *)

        walk y;
        if dy.low < dx.low then
          dx.low <- dy.low

      end
      else if (dy.low < dx.low) && dy.stacked then begin

        (* The first condition above indicates that [y] has been visited
           before [x], so [(x, y)] is a backwards or transverse edge. The
           second condition indicates that [y] is inside the same SCC as
           [x]; indeed, if it belongs to another SCC, then the latter has
           already been identified and moved out of [scc_stack]. *)

        if dy.number < dx.low then
          dx.low <- dy.number

      end

    ) x;

    (* We are done visiting [x]'s neighbors. *)

    if dx.low = dx.number then begin

      (* [x] is the entry point of a SCC. The whole SCC is now available;
         move it out of the stack. We pop elements out of the SCC stack
         until [x] itself is found. *)

      let rec loop () =
        let element = Stack.pop scc_stack in
        element.stacked <- false;
        dx.scc <- element.representative :: dx.scc;
        element.representative <- x;
        if element != dx then
          loop() in

      loop();
      representatives := x :: !representatives

    end

  (* Enumerate all nodes of the graph. At every unvisited node, start
     a depth-first traversal. *)

  let () = G.iter @@ fun root ->
    let droot = table root in
    if droot.number = 0 then begin
      (* This node has not been visited yet. *)
      mark droot;
      droot.stacked <- true;
      Stack.push droot scc_stack;
      walk root
    end

  (* There only remains to make our results accessible to the outside. *)

  let representative x =
    (table x).representative

  let scc x =
    (table x).scc

  let representatives =
    Array.of_list !representatives

  (* The array [representatives] contains a representative for each component.
     The components that have been identified last appear first in this array.
     A component is identified only after its successors have been identified;
     therefore, this array is naturally in topological order. *)

  let[@inline] process yield x =
      let data = table x in
      assert (data.representative == x); (* a sanity check *)
      assert (data.scc <> []);           (* a sanity check *)
      yield x data.scc

  let iter yield =
    Array.iter (process yield) representatives

  let rev_topological_iter yield =
    for i = Array.length representatives - 1 downto 0 do
      process yield representatives.(i)
    done

  let map yield =
    Array.map (process yield) representatives |> Array.to_list

  let rev_map yield =
    let accu = ref [] in
    rev_topological_iter (fun repr labels ->
      accu := yield repr labels :: !accu
    );
    !accu

end
