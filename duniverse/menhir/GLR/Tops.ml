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

module Make (T : sig

  (**We exploit the fact that LR(1) states are integers. *)
  type state = private int

  (**The type of semantic values is arbitrary. *)
  type semv

end) = struct
open T
type node = (state, semv) GSS.node

(* The set of the top nodes. *)

(* A node in the most recent generation (that is, the current generation) is
   known as a top node. It can also be thought of as the root of an LR stack. *)

(* Two nodes in the same generation cannot correspond to the same state; that
   is, they cannot have the same [state] field. Therefore, in particular, two
   top nodes cannot have the same state. *)

(* A mutable data structure, [tops], keeps track of the set of the top nodes.
   This data structure must support the following operations:

   - iterating over all top nodes (in the current generation);
   - determining whether a top node with a certain state number exists;
   - inserting a new top node;
   - iterating over all top nodes in the previous generation;
   - switching to a fresh new generation. *)

(* To support iteration, we use an array of nodes. We could also use a linked
   list, but a vector is more compact. Because the number of nodes is bounded
   by the number of states, a dynamically resizable vector is not needed; the
   vector can be stored inside a fixed-size array.

   To support constant-time lookups by state number, we use an array, indexed
   by states. Inside each array slot, we do not store an optional node, as one
   might expect; instead we store a node. A current entry and an outdated
   entry can be distinguished by testing the field [date]. This implies that
   the array never needs to be reset.

   To support iterating over the previous generation, we keep a second array
   of nodes. To support fast switching to a new generation, we simply swap
   the two arrays. *)

type t = {

  mutable count: int;
  (**[count] is the number of top nodes in the current generation. This number
     can grow as new nodes as registered. *)

  mutable nodes: node array;
  (**[nodes] is the set of top nodes in the current generation. These nodes
     are stored in the array [nodes] in the interval [\[0, count)]. Together,
     the fields [count] and [nodes] form a vector, that is, a variable-size
     array. *)

  mutable now: int;
  (**The current date; that is, the number of the current generation. *)

  table: node array;
  (**[table] is a finite map of state numbers to nodes. If the node [node]
     found at offset [s] in the table is such that [node.date = now]
     then it is the top node that corresponds to the state [s]. Otherwise
     it is a stale node and should be ignored. *)

  mutable prev_count: int;
  (**[prev_count] is the number of top nodes in the previous generation. *)

  mutable prev_nodes: node array;
  (**[prev_nodes] is the set of top nodes in the previous generation. They are
     stored in the array [prev_nodes] in the interval [\[0, prev_count)]. *)

}

let check tops =
  let n = Array.length tops.nodes in
  assert (Array.length tops.table = n);
  assert (Array.length tops.prev_nodes = n);
  assert (0 <= tops.count && tops.count <= n);
  assert (0 <= tops.prev_count && tops.prev_count <= n);
  (* Check that every node in [tops.nodes] is current and is present in
     the table [states]. Mark which states are represented. Check that
     no state is represented twice. *)
  let mark = Array.make n None in
  for i = 0 to tops.count-1 do
    let node = tops.nodes.(i) in
    assert (node.date = tops.now);
    let s = (node.state :> int) in
    assert (tops.table.(s) == node);
    assert (mark.(s) = None);
    mark.(s) <- Some node
  done;
  (* Check that every node in [tops.table] is present in [tops.nodes]. *)
  for s = 0 to n-1 do
    let node = tops.table.(s) in
    assert (node.date <= tops.now);
    if node.date = tops.now then
      match mark.(s) with
      | None ->
          assert false
      | Some node' ->
          assert (node == node')
  done;
  (* Check that every node in [tops.prev_nodes] belongs in the previous
     generation. *)
  for i = 0 to tops.prev_count-1 do
    let node = tops.prev_nodes.(i) in
    assert (node.date = tops.now - 1)
  done

(* The node [sentinel] is used to fill the logically empty slots of the arrays
   [nodes], [table], and [prev_nodes]. The main thing that matters about this
   node is that its [date] field must contain a recognizably invalid value. We
   use [-1]. *)

(* Our use of [Obj.magic] to obtain a dummy state is not really dangerous,
   considering that states are really integers. *)

let sentinel = {
  state = (Obj.magic 0 : state);
  date = -1;
  edges = MiniBabySet.empty;
  ddepth = 0;
}

let create n =
  let count = 0
  and nodes = Array.make n sentinel
  and now = 0
  and table = Array.make n sentinel
  and prev_count = 0
  and prev_nodes = Array.make n sentinel in
  { count; nodes; now; table; prev_count; prev_nodes }

let[@inline] iter tops yield =
  for i = 0 to tops.count-1 do
    let node = tops.nodes.(i) in
    yield node
  done

let[@inline] for_all tops p =
  (* We cannot use [Array.for_all] because we are interested in a segment
     of the array, not in the whole array. *)
  let i = ref (tops.count - 1) in
  while 0 <= !i && p tops.nodes.(!i) do i := !i - 1 done;
  !i = -1

let[@inline] iter_prev tops yield =
  for i = 0 to tops.prev_count-1 do
    let node = tops.prev_nodes.(i) in
    yield node
  done

let elements_prev tops =
  let nodes = ref [] in
  iter_prev tops (fun node -> nodes := node :: !nodes);
  !nodes

let present tops (node : node) =
  let s = (node.state :> int) in
  assert (tops.table.(s).date <= tops.now);
  let present = (tops.table.(s).date = tops.now) in
  assert (if present then node == tops.table.(s) else true);
  present

let absent tops node =
  not (present tops node)

let[@inline] date tops =
  tops.now

let[@inline] cardinal tops =
  tops.count

let[@inline] extract_singleton tops =
  assert (cardinal tops = 1);
  let node = tops.nodes.(0) in
  tops.count <- 0;
  let s = (node.state :> int) in
  tops.table.(s) <- sentinel;
  (* The previous generation should not be consulted.
     Out of precaution, make it empty. *)
  tops.prev_count <- 0;
  node

let[@inline] register tops (node : node) =
  assert (node.date = tops.now);
  assert (absent tops node);
  (* Push this node into the vector. *)
  let count = tops.count in
  tops.nodes.(count) <- node;
  tops.count <- count + 1;
  (* Register this node in the table. *)
  let s = (node.state :> int) in
  tops.table.(s) <- node

let[@inline] find tops (state : state) =
  let s = (state :> int) in
  let node = tops.table.(s) in
  assert (node.date <= tops.now);
  if node.date = tops.now then Some node else None

let[@inline] bump tops =
  (* Bump the date. *)
  tops.now <- tops.now + 1;
  (* Swap the arrays [nodes] and [prev_nodes]. *)
  let nodes = tops.nodes
  and prev_nodes = tops.prev_nodes in
  tops.nodes <- prev_nodes;
  tops.prev_nodes <- nodes;
  (* Record the number of nodes in the previous generation. *)
  tops.prev_count <- tops.count;
  (* The current generation is fresh, therefore empty. *)
  tops.count <- 0

let[@inline] advance tops date =
  assert (cardinal tops = 0);
  assert (tops.now <= date);
  (* Set the date. *)
  tops.now <- date

end (* Make *)
