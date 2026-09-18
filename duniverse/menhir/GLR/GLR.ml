(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

open Printf
open GLRAPI
open GSS
let empty, singleton = MiniBabySet.(empty, singleton)

(* [debug] enables well-formedness checks. It should normally be [false]. *)
let debug   = false

(* [verbose] enables information messages. It should normally be [false]. *)
let verbose = false

(* [deterministic_mode_enabled] enables the deterministic mode. It should
   normally be [true]. *)
let deterministic_mode_enabled =
  true

(* See the long comment above the function [deterministic_mode]. *)
let no_ddepth = -1

(* -------------------------------------------------------------------------- *)

(* The exception [Reject] is raised by a semantic action to forbid (cancel) a
   reduction. We publish the function [reject], so we retain the possibility
   of using a different mechanism in the future. *)

exception Reject

let[@inline] reject () =
  raise Reject

(* -------------------------------------------------------------------------- *)

module Make (D : DATA) = struct
open D

type node = (state, semv) GSS.node
type path = (state, semv) Path.t

(* [is_start prod] determines whether the production [prod] is a start
   production. It is defined in terms of [Production.start] as follows. *)

let[@inline] is_start (prod : production) : bool =
  (prod :> int) < (Production.start :> int)

(* Somewhat remarkably, the fact that LR(1) states are integers in the range
   [\[0, n)] is exploited only inside [Tops]. In the present file, states are
   considered entirely opaque, except in [compare_nodes] below. *)

module Tops =
  Tops.Make(struct
    type state = D.state
    type semv = D.semv
  end)

(* -------------------------------------------------------------------------- *)

(* Operations on sets of edges. *)

(* An ordering on nodes; an ordering on edges. *)

let[@inline] compare_nodes (node1 : node) (node2 : node) =
  let c = node1.date - node2.date in
  if c <> 0 then c else (node1.state :> int) - (node2.state :> int)
    (* here, too, we use the fact that states are integers *)

let compare_edges edge1 edge2 =
  compare_nodes edge1.node edge2.node

let compare_node_edge node1 edge2 =
  compare_nodes node1 edge2.node

(* Installing and finding edges. *)

(**[install parent edge] installs the new edge [edge] on the node [parent].   *)
let[@inline] install parent edge : unit =
  (* This is not the first edge carried by the node [parent]. Therefore the
     node [parent] becomes a join point in the GSS. The node [parent] must
     be a top node. Its deterministic depth must be [no_ddepth], which means
     that its deterministic depth has not yet been computed. *)
  assert (parent.edges <> empty);
  assert (parent.ddepth = no_ddepth);
  parent.edges <- MiniBabySet.add_absent compare_edges edge parent.edges;
  if debug then MiniBabySet.check parent.edges

(**[foreach_edge parent] enumerates the outgoing edges of the node [parent].  *)
let[@inline] foreach_edge parent yield =
  MiniBabySet.iter yield parent.edges

(**[find_existing_edge parent child] determines whether an edge from
   [parent] to [child] already exists. If so, this edge is returned. *)
let[@inline] find_existing_edge parent child =
  MiniBabySet.find compare_node_edge child parent.edges

(**[has_edge parent edge] determines whether the edge [edge] appear among
   the outgoing edges of the node [parent]. It is used for debugging only. *)
let has_edge node edge =
  assert debug;
  MiniBabySet.mem compare_edges edge node.edges

(**[foreach_path node n path] enumerates all ways of growing the path [path],
   whose leftmost node is [node], with [n] edges. By design of an LR parser,
   this search process cannot encounter a dead end: all partial paths can be
   extended down to depth [n]. Therefore [foreach_path] produces a least one
   path, and possibly several paths. *)
let rec foreach_path node n path yield =
  assert (node == Path.leftmost path);
  if n = 0 then
    yield path
  else begin
    (* There should exist at least one edge. *)
    assert (node.edges <> empty);
    (* There is no need to select just the edges that carry a certain label.
       All edges carry the same label. *)
    foreach_edge node @@ fun edge ->
    foreach_path edge.node (n-1) (Path.grow edge path) yield
  end

(**[unique_path node n path] grows the path [path], whose leftmost node is
   [node], with [n] edges. This function is invoked when the parser runs in
   deterministic mode; it is known that there is a unique way of growing this
   path with [n] edges. *)
let rec unique_path node n path =
  assert (node == Path.leftmost path);
  if n = 0 then
    path
  else begin
    (* There should exist exactly one edge. *)
    assert (MiniBabySet.is_singleton node.edges);
    let edge = MiniBabySet.extract_singleton node.edges in
    unique_path edge.node (n-1) (Path.grow edge path)
  end

let[@inline] unique_path node n =
  assert (n <= node.ddepth);
  unique_path node n (Path.empty node)

(* -------------------------------------------------------------------------- *)

(* A linear-time validity check for the GSS. (Debugging only.) *)

module Node = struct
  type t = node
  let equal node1 node2 =
    assert debug;
    node1.state = node2.state && node1.date = node2.date
  let hash node =
    assert debug;
    Hashtbl.hash (node.state, node.date)
end

module NodeTable =
  Hashtbl.Make(Node)

(* [check_GSS tops] traverses the GSS, starting from the top nodes in
   the set [tops], and checks that the GSS seems well-formed. *)

let check_GSS (tops : Tops.t) =
  assert debug;
  let visited  = NodeTable.create 1024
  and visiting = NodeTable.create 1024 in
  let while_visiting node action =
    (* If this assertion fails, the GSS is cyclic. *)
    assert (not (NodeTable.mem visiting node));
    NodeTable.add visiting node ();
    action();
    NodeTable.remove visiting node
  in
  let rec visit node =
    match NodeTable.find visited node with
    | node' ->
        (* If this assertion fails, then there are two physically distinct
           nodes with the same [state] and [date] fields. *)
        assert (node == node')
    | exception Not_found ->
        discover node
  and discover node =
    NodeTable.add visited node node;
    while_visiting node @@ fun () ->
    (* This node is the initial node if and only if it has no edges. *)
    (* We assume that this fact is true; we cannot check it. We do not
       have a way of testing whether [node.state] is an initial state. *)
    let initial = (node.edges = empty) in
    (* If this is the initial node then [node.date] must be zero. The converse
       implication is false: there can exist several nodes at date zero. *)
    if initial then assert (node.date = 0);
    (* If this node's deterministic depth is nonzero then it must have
       exactly one child, whose deterministic depth is smaller by one.
       If this node's deterministic depth is zero then either it is the
       initial node or it must carry several edges. *)
    assert (node.ddepth >= 0);
    if node.ddepth > 0 then begin
      assert (MiniBabySet.is_singleton node.edges);
      let edge = MiniBabySet.extract_singleton node.edges in
      let child = edge.node in
      assert (child.ddepth = node.ddepth - 1)
    end
    else
      assert (initial || MiniBabySet.cardinal node.edges > 0);
    (* Along every outgoing edge, the generation index must be preserved or
       must decrease. *)
    foreach_edge node @@ fun edge ->
    let child = edge.node in
    assert (child.date <= node.date);
    visit child
  in
  Tops.iter tops visit

(* [check_path tops path] checks that the reduction path [path] seems
   well-formed. *)

let rec check_path tops path =
  assert debug;
  match path with
  | Path.Empty top ->
      (* [top] must be a top node. *)
      assert (Tops.present tops top)
  | Path.Edge (edge, path) ->
      let node = Path.leftmost path in
      (* [edge] must be an outgoing edge of the node [node]. *)
      assert (has_edge node edge);
      check_path tops path

(* -------------------------------------------------------------------------- *)

(* Verbosity. *)

(* [show_node node] shows one node. *)

let show_node (node : node) =
  assert verbose;
  sprintf "%d/%d" (node.state :> int) node.date

(* [show_path path] shows a path.
   The path, read from left to right,
   goes from top node to bottom node. *)

let rec show_path path =
  match path with
  | Path.Empty top ->
      show_node top
  | Path.Edge (edge, path) ->
      show_path path ^ " -> " ^ show_node edge.node

(* [show_tops tops] shows a list of the current top nodes. *)

let show_tops tops =
  assert verbose;
  eprintf "There are currently %d top nodes:" (Tops.cardinal tops);
  Tops.iter tops (fun node -> eprintf " %s" (show_node node));
  eprintf "\n%!"

(* [discovered_reduction top prod path] announces that a reduction path has
   been discovered and is about to be inserted into the reduction queue. *)

let discovered_reduction top prod path =
  assert verbose;
  eprintf "In node %s, " (show_node top);
  if is_start prod then
    eprintf "can accept via the path %s\n"
      (show_path path)
  else
    eprintf "can reduce %s via the path %s\n"
      (Production.print prod) (show_path path)

(* [creating_new_node_and_edge top edge] announces the creation of a new top
   node and a new edge. *)

let creating_new_node_and_edge top edge =
  assert verbose;
  eprintf "Creating a new node %s and a new edge %s -> %s.\n"
    (show_node top) (show_node top) (show_node edge.node)

(* [creating_new_edge top edge] announces the creation of a new edge out of an
   existing top node. *)

let creating_new_edge top edge =
  assert verbose;
  eprintf "Creating a new edge %s -> %s out of the existing node %s.\n"
    (show_node top) (show_node edge.node) (show_node top)

(* [merging top edge] announces that two semantic values are being merged
   on the edge [edge] out of the top node [top]. *)

let merging top edge =
  assert verbose;
  eprintf "Merging semantic values on existing edge %s -> %s...\n"
    (show_node top) (show_node edge.node)

(* [reducing prod path] announces that the production [prod] is being reduced
   along the path [path]. *)

let reducing prod path =
  assert verbose;
  eprintf "Now reducing %s along the path %s...\n"
    (Production.print prod) (show_path path)

(* -------------------------------------------------------------------------- *)

(* Following McPeak, we use a priority queue of reduction tasks. *)

(* McPeak calls it the reduction worklist. *)

module Task = struct

  (* A reduction task is a triple of 1- a production A → α, 2- a path whose
     rightmost node can reduce this production and whose length is the length
     of α, and 3- the start date of this path. The last component is redundant,
     as the start date of a path is just [(Path.leftmost path).date]. We
     include it so as to speed up [compare]. *)
  type t = production * path * int

  (* Following McPeak, we use a lexicographic order on reduction tasks. Tasks
     whose extent is smaller are treated first. In case of a tie, productions
     whose left-hand symbol lies deeper are treated first. (Assuming that the
     grammar is acyclic, there is a partial order on symbols: when B →+ A
     holds, we say that A lies deeper than B.) We assume that the numbering of
     productions reflect this partial order; so productions whose index is
     greater are treated first. *)
  let compare (prod1, path1, start1 : t) (prod2, path2, start2 : t) =
    assert ((Path.rightmost path1).date = (Path.rightmost path2).date);
    (* First, give stronger priority to smaller extents. Because all paths
       in the queue have the same end date, this amounts to giving stronger
       priority to later start dates. *)
    let c = start2 - start1 in
    if c <> 0 then c else
    (* If the extents are the same, give stronger priority to productions
       whose index is greater. *)
    (prod2 :> int) - (prod1 :> int)

end
type task = Task.t

module Q : sig
  type t
  val create  : unit -> t
  val insert  : t -> production -> path -> unit
  val extract : t -> task option
  val is_empty: t -> bool (* for debugging only *)
end = struct
  module V = struct
    include MiniVector
    let get = unsafe_get
    let set = unsafe_set
  end
  include PriorityQueue.Make(Task)(V)
  let[@inline] insert q prod path =
    add q (prod, path, (Path.leftmost path).date)
end

(* -------------------------------------------------------------------------- *)

(* Populating the priority queue [queue] with reduction tasks. *)

(* The parameter [input] is used to determine which reductions are enabled. *)

(* The treatment of default reductions is entirely implicit. The function
   [State.foreach_reduction state input], which enumerates the enabled
   reductions, does not expect the lookahead symbol as a parameter; instead
   it expects the input stream [input]. If the state [state] has a default
   reduction then this function does not query [input] to see the lookahead
   symbol. Thus, the next input symbol is requested only if and when needed.
   This is important, as a correct parser (without end-of-stream conflicts)
   should accept a segment of the input without requesting the input symbol
   that follows this segment. *)

(* [accumulate_reduction_paths'' input queue top edge] searches for all
   reduction paths that start at the node [top], whose first edge is [edge],
   and that are permitted by the current lookahead symbol [lookahead input].
   It inserts all such paths into the queue [queue]. *)

let accumulate_reduction_paths'' input queue top edge =
  if debug then assert (has_edge top edge);
  State.foreach_reduction top.state input @@ fun prod ->
  let n = Production.length prod in
  if 0 < n then
    let path = Path.grow edge (Path.empty top) in
    foreach_path edge.node (n-1) path @@ fun path ->
    if verbose then discovered_reduction top prod path;
    Q.insert queue prod path

(* [accumulate_reduction_paths' input queue top] searches for all reduction
   paths that start at the node [top] and are permitted by the current
   lookahead symbol. It inserts all such paths into the queue [queue]. *)

let accumulate_reduction_paths' input queue top =
  State.foreach_reduction top.state input @@ fun prod ->
  let n = Production.length prod in
  let path = Path.empty top in
  foreach_path top n path @@ fun path ->
  if verbose then discovered_reduction top prod path;
  Q.insert queue prod path

(* [accumulate_reduction_paths input queue tops] searches for all reduction
   paths that start at a top node in the set [tops] and are permitted by the
   current lookahead symbol. It inserts all such paths into the queue
   [queue]. *)

let[@inline] accumulate_reduction_paths input queue tops =
  Tops.iter tops @@ fun top ->
  accumulate_reduction_paths' input queue top

(* -------------------------------------------------------------------------- *)

(* [reduce ... prod path] deals with a reduction task that has just been
   extracted out of the priority queue. The task is to reduce the production
   [prod] along the path [path]. *)

(* The parameter [input] is used to determine which reductions are enabled. *)

(* The parameter [queue] is used to enqueue new reduction tasks. *)

(* The parameter [tops] is used to test for the existence of top nodes
   and to register new top nodes. *)

let reduce input queue tops prod path =
  if debug then assert (not (is_start prod));
  if debug then check_path tops path;

  (* Lock the edges of this path. *)
  if debug then Path.lock path;

  (* Execute the semantic action. Give it access to the path [path] so it can
     extract semantic values out of the path's edges. The semantic action can
     raise [Reject] to cancel this reduction. *)
  match Production.action prod input path with
  | exception Reject -> ()
  | semv ->

  (* Determine the target state of the goto transition that we want to take. *)
  let left = Path.leftmost path in
  let nt = Production.lhs prod in
  let state = State.goto left.state nt in

  (* Test whether a top node for this state already exists. *)
  match Tops.find tops state with

  | None ->
      (* A node that represents the desired state does not yet exist. *)
      (* Create a new edge and a new node. *)
      let edge = { node = left; semv; locked = false } in
      let date = Tops.date tops in
      let edges = singleton edge
      and ddepth = no_ddepth in
      let right = { state; date; edges; ddepth } in
      (* This node is a new top node. *)
      let top = right in
      Tops.register tops top;
      if debug then Tops.check tops;
      (* New reduction opportunities may exist at this new node,
         which we must insert into the queue. (NOTE A) *)
      if verbose then creating_new_node_and_edge top edge;
      if verbose then eprintf "Discovering paths out of this new node...\n";
      accumulate_reduction_paths' input queue top

  | Some right ->
      assert (right.date = Tops.date tops);
      (* There is already a node [right] which represents the desired state. *)
      (* Test whether there exists an edge from [right] down to [left]. *)
      match find_existing_edge right left with

      | Some edge ->
          (* There is already an edge. This edge carries a semantic value.
             We must merge it with the new semantic value [semv] that we
             have just computed. *)
          (* It must be the case that this edge is not yet locked; otherwise,
             this implies that McPeak's bottom-up strategy does not achieve
             the desired effect. *)
          assert (edge.node == left);
          if verbose then merging right edge;
          if debug then assert (not edge.locked);
          edge.semv <- Semv.merge nt edge.semv semv input left.date right.date

      | None ->
          (* There is no edge. We must create and install this edge. *)
          let edge = { node = left; semv; locked = false } in
          install right edge;
          if verbose then creating_new_edge right edge;
          (* This new edge may create new reduction opportunities. We must
             detect them and insert them into the queue. We limit our search
             to paths whose top node is [right] and whose first edge is
             [edge]. (NOTE B) (NOTE C) *)
          let top = right in
          if verbose then eprintf "Discovering paths out of this new edge...\n";
          accumulate_reduction_paths'' input queue top edge

(* NOTE A: This function call is missing in McPeak's pseudo-code
   (TR UCB/CSD-2-1214, Figure 8, reduceViaPath, last case).
   This has been acknowledged by McPeak:
   https://scottmcpeak.com/elkhound/reduceViaPath_bug.html *)

(* NOTE B. This is where McPeak calls [enqueueLimitedReductions]. McPeak
   himself follows Farshi and Rekers. In McPeak's code, this function searches
   *every top node* for paths that use the new edge, *not necessarily as their
   first edge*. This is a source of inefficiency. However, in our case, there
   cannot be a reduction path that uses the new edge, but not as its first
   edge. Such a path would start at an ancestor of [right], say [ancestor].
   (This ancestor could be [right] itself, if there is a cycle, but this is
   irrelevant.) Because [right] and all of its ancestors are top nodes, the
   (nonempty) path segment from [ancestor] to [right] would have to be labeled
   with nullable symbols. Thus, the production that we might hope to reduce
   would necessarily exhibit a nullable suffix. We assume that the grammar has
   no right nullable rules; therefore this situation cannot arise. *)

(* NOTE C. Scott and Johnstone ("Right Nulled GLR Parsers", 2006) seem to
   claim that if the grammar has no right nullable rules then a new child
   (a new edge) is never added to a node that already has a parent. They
   make this claim (in section 3.2 and on page 594) about their Algorithm
   1e, a variant of Tomita's Algorithm 1. If this property were true also
   of our algorithm, then we could assert that the node [right] has no
   parent. (That would not be useful; just good to know.) However, it is
   unclear to me why this property might be true. Scott and Johnstone do
   not give a proof. They cite a technical report which does not seem to
   be available online ("Tomita-Style generalized LR parsers"). *)

(* It is important to ensure that every reduction path is enqueued at most
   once. This is the case because [accumulate_reduction_paths] and friends
   are called in just three places, which cover distinct paths:

   - initially, in [accumulate_reduction_paths], we discover the reduction
     paths that start at existing top nodes;
   - when a new top node is created, we discover the reduction paths that
     start at this node (NOTE A), and only those;
   - when a new edge towards an existing node is created, we discover the
     reduction paths that begin with this edge (NOTE B), and only those.

   It seems easy to convince oneself that no path is discovered twice. *)

(* This property is important because it guarantees that two identical
   parse trees, spanning the same input segment, are never constructed.
   This in turn guarantees that among the semantic values that we merge
   using [merge], no two values can be identical. (That is, unless the
   user intentionally discards information -- but that is her concern.) *)

(* -------------------------------------------------------------------------- *)

(* Success (acceptance) is detected when a start production is reduced. *)

(* Our ordering, [Task.compare], is such that a reduction task that concerns
   a start production is always treated last. (Its extent is maximal, and
   the start symbol is shallowest.) Therefore, the reduction queue must be
   empty. This implies that this successful result is the only successful
   result that one might hope to find at this input offset. *)

(* This said, perhaps it would be possible to continue parsing and succeed
   at a greater input offset. *)

let accept queue prod path =
  assert (Q.is_empty queue);
  assert (is_start prod);
  assert (Production.length prod = 1);
  assert (Path.length path = 1);
  let edge = Path.extract path in
  edge.semv

(* -------------------------------------------------------------------------- *)

(* The following two mutually recursive functions form the main loop of the
   GLR algorithm. The algorithm alternates between reductions and shifts. *)

(* [exhaust_reductions ...] repeatedly performs reductions until the reduction
   queue [queue] becomes empty. A call [reduce ... path] can insert new
   reduction tasks into the queue. Once the queue becomes empty, control is
   transmitted to [perform_shifts]. *)

let rec exhaust_reductions input queue tops =
  match Q.extract queue with
  | Some (prod, path, _extent) ->
      if is_start prod then
        accept queue prod path
      else begin
        if verbose then reducing prod path;
        reduce input queue tops prod path;
        exhaust_reductions input queue tops
      end
  | None ->
      compute_ddepths tops;
      perform_shifts input queue tops

(* [perform_shifts ...] consumes one input symbol and performs all shifts,
   creating a new generation of top nodes. Then, it populates the reduction
   queue with the reductions that are enabled in these new nodes, and
   transfers control back to [exhaust_reductions]. *)

and perform_shifts input queue tops =
  if debug then check_GSS tops;
  if verbose then show_tops tops;
  let date = Tops.date tops in
  (* Before consuming the next input symbol, retrieve its semantic value. *)
  let semv = Semv.token2value (Input.lookahead input) in
  (* Increment the date. The set [tops] becomes empty and ready to be
     populated with the top nodes of the new generation. *)
  Tops.bump tops;
  if debug then Tops.check tops;
  (* Now perform the shifts. *)
  let () =
    (* For every top node [node] in the previous generation, *)
    Tops.iter_prev tops @@ fun node ->
    (* if this node can shift the next input symbol, *)
    State.foreach_shift node.state input @@ fun state ->
    (* construct an edge, *)
    let edge = { node; semv; locked = true } in
    (* leading either to an existing top node in the new generation
       or to a new top node that we create and register. *)
    match Tops.find tops state with
    | Some top ->
        if verbose then eprintf "Shifting from node %s to existing node %s.\n"
          (show_node node) (show_node top);
        install top edge
    | None ->
        let date = date + 1
        and edges = singleton edge
        and ddepth = no_ddepth in
        let top = { state; date; edges; ddepth } in
        Tops.register tops top;
        if verbose then eprintf "Shifting from node %s to new node %s.\n"
          (show_node node) (show_node top);
        if debug then Tops.check tops
  in
  (* Now declare the current input symbol consumed. This cannot be done
     earlier, because [State.foreach_shift] implicitly consults it. *)
  Input.consume input;
  (* If the number of shifts that have been performed is zero, fail. *)
  if Tops.cardinal tops = 0 then
    raise (Error (Tops.elements_prev tops));
  (* Populate the reduction queue and go back to performing reductions. *)
  if debug then assert (Q.is_empty queue);
  (* At this point, if there is only one node in the new generation,
     then we can switch to deterministic mode. *)
  if deterministic_mode_enabled && Tops.cardinal tops = 1 then
    let () = compute_ddepths tops in
    let top = Tops.extract_singleton tops in
    if verbose then eprintf "Entering deterministic mode.\n%!";
    deterministic_mode queue tops input top
  else
    normal_mode input queue tops

and normal_mode input queue tops =
  if verbose then eprintf "Discovering reduction paths...\n";
  accumulate_reduction_paths input queue tops;
  exhaust_reductions input queue tops

(* -------------------------------------------------------------------------- *)

(* The deterministic mode. *)

(* McPeak suggests that GLR is significantly slower than LR, so one can save
   significant time by switching to a "deterministic mode" when there is only
   one stack top. As far as I can see, the main reasons why the deterministc
   mode can be faster are:

   - there is no need to maintain the top set;
   - there is no need for reductions to travel through the reduction queue;
   - one can look up an LR action table, which combines shift and reduce
     actions, instead of separately looking up a shift table and a reduction
     table;
   - once a sequence of reductions is over, there is no need to test every
     node in the sequence to find out which nodes can shift.

   The main potential overhead introduced by the deterministic mode is the
   need to maintain each node's deterministic depth. Our measurements show
   that this overhead is very small.

   This said, our measurements suggest that the performance improvement
   permitted by the deterministic mode is not so great as one might expect.
   On a fully deterministic grammar, we measure it as roughly 1.6x, that is,
   a 60% performance improvement. *)

(* The manner in which we compute each node's deterministic depth is not the
   same as McPeak. He initializes a node's ddepth to its correct value when
   this node is first created; and, when an existing node receives a new edge,
   he resets this node's ddepth to zero. The problem, though, is that if this
   node has parents then the ddepth of every top node must be recomputed.
   (McPeak uses the reference counts to efficiently detect this situation.)
   (This problem arises only in the presence of ε productions.) McPeak does
   not indicate when and how he performs this recomputation. A look at his
   code (glr.cc, GLR::rwlShiftNonterminal, line 2061) shows that he performs
   it immediately and in a naïve way, by repeatedly iterating over all top
   nodes. Instead, we note that it is possible to perform it just once and in
   only one iteration over the top nodes. We proceed as follows:

   - When a new generation is opened, every newly created node in this
     generation temporarily receives ddepth [no_ddepth].

   - When a top node receives a new edge, its ddepth is [no_ddepth]
     and remains [no_ddepth].

   - When a generation is about to be closed,
     in one iteration over the top nodes,
     we compute the true ddepth of every top node.

   The computation described in the last point involves traversing the acyclic
   graph formed by the top nodes. (If the grammar has no ε productions then
   this graph has no edges.) We see two ways of performing this computation:

   - Traverse the graph in topological order, using, say, depth-first search.
     The [ddepth] field itself can be exploited to mark nodes.

   - Iterate over the top nodes in an arbitrary order. If a node has a single
     child, set its ddepth to the ddepth of its child plus one; otherwise set
     it to zero. This computation always yields a sound over-approximation of
     the true ddepth. If the grammar has no ε productions then it yields an
     exact result.

   The first approach seems preferable, as it always yields an exact result.
   The second approach might be cheaper by a constant factor, because it is
   linear scan versus depth-first search. If the grammar has no ε productions
   then the two approaches are the same. *)

(* When running in deterministic mode, the reduction queue [queue] and the
   top set [tops] are empty. The parameter [input] is the remaining input;
   the parameter [top] is the top node of the stack. *)

and deterministic_mode queue tops input top =
  assert (Q.is_empty queue);
  assert (Tops.cardinal tops = 0);
  assert (top.ddepth <> no_ddepth);
  match State.unique_action top.state input with
  | `Fail ->
      raise (Error [top])
  | `Fork ->
      if verbose then eprintf
        "Unable to remain in deterministic mode due to multiple actions.\n";
      leave_deterministic_mode queue tops input top
  | `Shift state ->
      let node = top in
      (* Construct an edge and a new node. *)
      let semv = Semv.token2value (Input.lookahead input) in
      let edge = { node; semv; locked = true } in
      let date = node.date + 1
      and edges = singleton edge
      and ddepth = node.ddepth + 1 in
      let top = { state; date; edges; ddepth } in
      if verbose then creating_new_node_and_edge top edge;
      (* Consume the current input symbol. *)
      Input.consume input;
      (* Continue. *)
      deterministic_mode queue tops input top
  | `Reduce prod ->
      (* Test whether we can remain in deterministic mode. This is the case if
         the deterministic depth of the current node is large enough. *)
      let n = Production.length prod in
      if top.ddepth < n then begin
        if verbose then eprintf
            "Unable to reduce down to depth %d in deterministic mode.\n" n;
        leave_deterministic_mode queue tops input top
      end
      else
        (* Construct the reduction path. *)
        let path = unique_path top n in
        if is_start prod then
          accept queue prod path
        else
          let () = if verbose then reducing prod path in
          (* Lock the edges of this path. *)
          if debug then Path.lock path;
          (* Execute the semantic action. *)
          match Production.action prod input path with
          | exception Reject -> raise (Error [top])
          | semv ->
          (* Take a goto transition. *)
          let left = Path.leftmost path in
          let nt = Production.lhs prod in
          let state = State.goto left.state nt in
          (* Create a new edge and a new node. *)
          let node = left in
          let edge = { node; semv; locked = false } in
          let date = top.date in
          let edges = singleton edge
          and ddepth = left.ddepth + 1 in
          let top = { state; date; edges; ddepth } in
          if verbose then creating_new_node_and_edge top edge;
          (* Continue. *)
          deterministic_mode queue tops input top

(* Leaving the deterministic mode requires restoring the invariant of the
   normal mode.  *)

and leave_deterministic_mode queue tops input top =
  assert (Q.is_empty queue);
  assert (Tops.cardinal tops = 0);
  if verbose then eprintf "Leaving deterministic mode.\n%!";
  (* Restore the set of top nodes. Its current date should be [top.date]. *)
  let now = top.date in
  Tops.advance tops now;
  (* In order to respect the invariant of the GLR mode, all nodes whose date
     is [top.date] should be top nodes, that is, members of the top set, and
     their ddepth should be reset to [no_ddepth]. We can find these nodes by
     following the unique path out of [top]. *)
  reset tops now top;
  (* We should be good. *)
  if debug then Tops.check tops;
  (* Start parsing in GLR mode again. *)
  normal_mode input queue tops

(* [reset] follows the unique path out of [top], as described above, and
   resets the ddepth of every node along the path to [no_ddepth]. *)

and reset tops now node =
  assert (node.date = now);
  if verbose then eprintf "Registering node %s as a top node.\n"
    (show_node node);
  Tops.register tops node;
  node.ddepth <- no_ddepth;
  let edges = node.edges in
  if MiniBabySet.is_singleton edges then begin
    assert (MiniBabySet.is_singleton edges);
    let edge = MiniBabySet.extract_singleton edges in
    let child = edge.node in
    if child.date = now then
      reset tops now child
  end
  else
    (* There are several edges. The children cannot be top nodes,
       I believe, because they must have been created in GLR mode,
       before deterministic mode was entered. Do not visit them. *)
    assert (MiniBabySet.for_all (fun edge -> edge.node.date < now) edges)

(* -------------------------------------------------------------------------- *)

(* [compute_ddepths tops] computes the deterministic depth of every top node,
   as described by the long comment above the function [deterministic_mode].  *)

and compute_ddepths tops =
  (* Every top node initially has ddepth [no_ddepth]. *)
  assert (Tops.for_all tops @@ fun node -> node.ddepth = no_ddepth);
  (* Traverse the graph of the top nodes via depth-first search. Because this
     graph is acyclic, we cannot possibly hit a node that is currently being
     visited. Thus, when we reach a node, either its ddepth is [no_ddepth],
     which means that this node has not yet been visited, or its ddepth has
     already been set to its correct value. *)
  let now = Tops.date tops in
  Tops.iter tops (compute now)

and compute now node =
  assert (node.date = now);
  if node.ddepth = no_ddepth then
    (* This node has not yet been visited. *)
    (* Visit every child that is also a top node. *)
    let edges = node.edges in
    MiniBabySet.iter (fun edge ->
      let child = edge.node in
      if child.date = now then
        compute now child
    ) edges;
    (* Every child now has a correct ddepth. *)
    (* If this node has just one child, set its ddepth to its child's ddepth
       plus one. Otherwise set it to zero. *)
    if MiniBabySet.is_singleton edges then
      let edge = MiniBabySet.extract_singleton edges in
      let child = edge.node in
      assert (child.ddepth <> no_ddepth);
      node.ddepth <- 1 + child.ddepth
    else
      node.ddepth <- 0;
    if verbose then
      eprintf "The deterministic depth of node %s is %d.\n"
        (show_node node) node.ddepth

(* -------------------------------------------------------------------------- *)

(* The entry point. *)

(* [start state input] starts the parser in the initial state [state]. *)

let start state input =
  (* assert (State.is_start state); *)
  (* Initialize a start node. *)
  let date = 0
  and edges = MiniBabySet.empty
  and ddepth = no_ddepth in
  let top = { state; date; edges; ddepth } in
  let tops = Tops.create State.n in
  if debug then Tops.check tops;
  (* Create the reduction queue. *)
  let queue = Q.create() in
  (* Start parsing in deterministic mode (if enabled). *)
  if deterministic_mode_enabled then begin
    if verbose then eprintf "Starting in deterministic mode.\n%!";
    top.ddepth <- 0;
    deterministic_mode queue tops input top
  end
  else begin
    Tops.register tops top;
    if debug then Tops.check tops;
    normal_mode input queue tops
  end

(* -------------------------------------------------------------------------- *)

end (* Make *)
