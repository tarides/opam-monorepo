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
open MiddleAPI

module Run
(A : LR1_AUTOMATON)
(X : CONSTRUCTION_MODE_SETTINGS)
= struct
module Lr0 = A.Lr0
module G = Lr0.G
open G
module Derivation = Derivation.Make(G)
include Fix.Numbering.Operations(A)

(* -------------------------------------------------------------------------- *)

(* [materialize table x] turns an implicit list, stored using pointers through
   the hash table [table], into an explicit list. The head [x] of the implicit
   list is not included in the explicit list. *)

let materialize (table : ('a, 'a option) Hashtbl.t) (x : 'a) : 'a list =
  let rec loop x =
    match Hashtbl.find table x with
    | None ->
        []
    | Some x ->
        x :: loop x
  in
  loop x

(* -------------------------------------------------------------------------- *)

(* Explaining shift actions. *)

(* The existence of a shift action stems from the existence of a shift item
   in the LR(0) core that underlies the LR(1) state of interest. That is,
   lookahead sets are not relevant. The existence of a shift item in the
   LR(0) core is explained by finding a path from a start item to the shift
   item in the LR(0) nondeterministic automaton, such that the symbols read
   along this path form the (previously fixed) symbol string that leads to
   the conflict state in the LR(1) automaton. There may be several such
   paths: a shortest one is chosen. There may also be several shift items in
   the conflict state: an arbitrary one is chosen. I believe it would not be
   interesting to display traces for several shift items: they would be
   identical except in their last line (where the desired shift item
   actually appears). *)

(* Symbolic execution of the nondeterministic LR(0) automaton. *)

(* A configuration is a pair of an LR(0) item and an offset into the input
   string, which indicates how much has been read so far. *)

type config0 =
    Item.t * int

type ancestor0 =
  config0 option

(* [follow] builds a derivation out of a (nonempty, reversed) sequence of
   configurations. The derivation is constructed from bottom to top, that
   is, beginning at the last configuration and moving back towards to the
   start configuration. *)

let rec follow derivation offset' configs =
  match configs with
  | [] ->
      assert (offset' = 0);
      derivation
  | (item, offset) :: configs ->
      let prod, pos = Item.export item in
      let rhs = Production.rhs prod in
      let derivation =
        if offset = offset' then
          (* This is an epsilon transition. Put a new root node on top of
             the existing derivation. *)
          Derivation.build pos rhs derivation None
        else
          (* This was a shift transition. Tack symbol in front of the
             forest. *)
          Derivation.prepend rhs.(pos) derivation
      in
      follow derivation offset configs

(* Symbolic execution begins with a start item [start], a string of input
   symbols [input], which is to be fully consumed, and a goal item [stop].
   The objective is to find a path through the automaton that leads from the
   start configuration [(start, 0)] to the goal configuration [(stop, n)],
   where [n] is the length of the input string. The automaton is explored
   via breadth-first search. A hash table records which configurations have
   been visited. This table also allows to build a spanning tree of shortest
   paths. *)

exception Done

module ExplainShiftItem (X : sig
  val start : Item.t
  val input : Symbol.t array
  val stop : Item.t
end) = struct
open X

  let n =
    Array.length input

  let table : (config0, ancestor0) Hashtbl.t =
    Hashtbl.create 1023

  let queue : config0 Queue.t =
    Queue.create()

  let enqueue ancestor config =
    if not (Hashtbl.mem table config) then begin
      Hashtbl.add table config ancestor;
      Queue.add config queue
    end

  let process config =
    let item, offset = config in

    (* If this item is the goal item and if we have read all
       of the input symbols, stop. *)
    if Item.equal item stop && offset = n then
      raise Done;

    (* Otherwise, explore the transitions out of this item. *)
    let prod, pos = Item.export item in
    let rhs = Production.rhs prod in
    let length = Array.length rhs in

    (* A shift transition is followed only if its symbol
       matches the symbol found in the input string. *)
    if pos < length
    && offset < n
    && Symbol.equal rhs.(pos) input.(offset) then begin
      let config' = (Item.import (prod, pos+1), offset+1) in
      enqueue (Some config) config'
    end;

    (* Epsilon transitions. *)
    if pos < length then
      match rhs.(pos) with
      | Symbol.T _  -> ()
      | Symbol.N nt ->
          Production.iternt nt @@ fun prod ->
          let config' = (Item.import (prod, 0), offset) in
          enqueue (Some config) config'

  let conclude () : Derivation.t =
    let configs = materialize table (stop, n) in
    let prod, pos = Item.export stop in
    let rhs = Production.rhs prod in
    let derivation = Derivation.tail pos rhs in
    let derivation = follow derivation n configs in
    derivation

  let derivation =
    enqueue None (start, 0);
    try
      MQueue.repeat queue process;
      assert false
    with Done ->
      (* We have found a (shortest) path from the start configuration to
         the goal configuration. Turn it into an explicit derivation. *)
      conclude()

end (* ExplainShiftItem *)

let explain_shift_item start input stop : Derivation.t =
  let module E = ExplainShiftItem(struct
    let start = start
    let input = input
    let stop = stop
  end) in
  E.derivation

(* -------------------------------------------------------------------------- *)

(* Explaining reduce actions. *)

(* The existence of a reduce action stems from the existence of a reduce
   item, whose lookahead set contains the terminal symbol of interest, in
   the state of interest. Here, lookahead sets are relevant only insofar as
   they contain or do not contain the terminal symbol of interest. In other
   words, lookahead sets can be abstracted by Boolean values. The existence
   of the reduce item is explained by finding a path from a start item to
   the reduce item in the LR(1) nondeterministic automaton, such that the
   symbols read along this path form the (previously fixed) symbol string
   that leads to the conflict state in the LR(1) automaton. There may be
   several such paths: a shortest one is chosen. *)

(* Symbolic execution of the nondeterministic LR(1) automaton. *)

(* A configuration is a pair of an LR(1) item and an offset into the input
   string, which indicates how much has been read so far. An LR(1) item is
   itself represented as the combination of an LR(0) item and a Boolean
   flag, which indicates whether the terminal symbol of interest appears or
   does not appear in the lookahead set. *)

type config1 =
  Item.t * bool * int

type ancestor1 =
  config1 option

let config1toconfig0 (item, _, offset) =
  (item, offset)

(* Textual helpers. *)

let because e =
  if e = "" then "" else " because " ^ e

let because_segment_can_vanish pos rhs =
  if pos < Array.length rhs then
    sprintf " because %s can vanish"
      (Symbol.print_subarray false pos rhs)
  else
    ""

(* [follow1] builds a derivation out of a sequence of configurations. The
   end of the sequence is dealt with specially: we want to explain how the
   lookahead symbol appears and is inherited. Once that is done, the rest
   (that is, the beginning) of the derivation is dealt with as above. *)

(* [z] is the lookahead symbol. *)

let rec follow1 z derivation offset' configs =
  match configs with
  | [] ->
      assert (Terminal.equal z Terminal.sharp);
      (* One could emit a comment saying that the lookahead symbol is
         initially [#]. That comment would have to be displayed above
         the derivation, though, and there is no support for that at
         the moment, so let's skip it. *)
      derivation
  | (item, _, offset) :: configs ->
      let prod, pos = Item.export item in
      let rhs = Production.rhs prod in
      if offset = offset' then

        (* This is an epsilon transition. Attack a new line and add a comment
           that explains why the lookahead symbol is produced or inherited. *)
        let nullable, first = Analysis.nullable_first prod (pos + 1) in
        if TerminalSet.mem z first then
          (* The lookahead symbol is produced (and perhaps also inherited). *)
          let e = Analysis.explain_first z prod (pos + 1) in
          let comment = "lookahead token appears" ^ because e in
          let derivation = Derivation.build pos rhs derivation (Some comment) in
          (* Print the rest of the derivation without paying attention to the
             lookahead symbols. *)
          follow derivation offset (List.map config1toconfig0 configs)
        else
          (* The lookahead symbol is not produced; it is inherited. *)
          let comment =
            assert nullable;
            "lookahead token is inherited" ^
            (because_segment_can_vanish (pos + 1) rhs)
          in
          let derivation = Derivation.build pos rhs derivation (Some comment) in
          follow1 z derivation offset configs

      else

        (* This is a shift transition. Tack symbol in front of forest. *)
        let derivation = Derivation.prepend rhs.(pos) derivation in
        follow1 z derivation offset configs

(* Symbolic execution is performed in the same manner as above. *)

(* [z] is the terminal symbol of interest. *)

module ExplainReduceItem (X : sig
  val z : Terminal.t
  val start : Item.t
  val input : Symbol.t array
  val stop : Item.t
end) = struct
open X

  let n =
    Array.length input

  let table : (config1, ancestor1) Hashtbl.t =
    Hashtbl.create 1023

  let queue : config1 Queue.t =
    Queue.create()

  let enqueue ancestor config =
    if not (Hashtbl.mem table config) then begin
      Hashtbl.add table config ancestor;
      Queue.add config queue
    end

  let process config =
    let (item, lookahead, offset) = config in

    (* If this item is the goal item and if we have read all
       of the input symbols, stop. *)
    if Item.equal item stop && lookahead && offset = n then
      raise Done;

    (* Otherwise, explore the transitions out of this item. *)
    let prod, pos = Item.export item in
    let rhs = Production.rhs prod in
    let length = Array.length rhs in

    (* A shift transition is followed only if its symbol
       matches the symbol found in the input string. *)
    if pos < length
    && offset < n
    && Symbol.equal rhs.(pos) input.(offset) then begin
      let config' = (Item.import (prod, pos+1), lookahead, offset+1) in
      enqueue (Some config) config'
    end;

    (* Epsilon transitions. *)
    if pos < length then
      match rhs.(pos) with
      | Symbol.T _  -> ()
      | Symbol.N nt ->
          let nullable, first = Analysis.nullable_first prod (pos + 1) in
          let first : bool = TerminalSet.mem z first in
          let lookahead' = if nullable then first || lookahead else first in
          Production.iternt nt @@ fun prod ->
          let config' = (Item.import (prod, 0), lookahead', offset) in
          enqueue (Some config) config'

  let conclude () : Derivation.t =
    let configs = materialize table (stop, true, n) in
    let derivation = Derivation.empty in
    let derivation = follow1 z derivation n configs in
    derivation

  let derivation =
    (* The initial lookahead set is the singleton [#]. Therefore, If the
       terminal symbol of interest is [#], then it appears in the initial
       lookahead set; otherwise it doesn't. *)
    let lookahead = Terminal.equal z Terminal.sharp in
    enqueue None (start, lookahead, 0);
    try
      MQueue.repeat queue process;
      assert false
    with Done ->
      (* We have found a (shortest) path from the start configuration to
         the goal configuration. Turn it into an explicit derivation. *)
      conclude()

end (* ExplainReduceItem *)

let explain_reduce_item z start input stop =
  let module E = ExplainReduceItem(struct
    let z = z
    let start = start
    let input = input
    let stop = stop
  end) in
  E.derivation

(* -------------------------------------------------------------------------- *)

(* A counter of how many conflicts could *not* be explained. *)

(* When this counter is nonzero, we display a message on the standard output
   channel. This can help us detect regressions via [make test]. *)

let unexplainable =
  ref 0

let log_unexplainable c =
  if !unexplainable > 0 then
    Report.log c
      "%d conflict%s could not be explained."
      !unexplainable
      (if !unexplainable > 1 then "s" else "")

(* -------------------------------------------------------------------------- *)

(* [foreach_conflict_node] enumerates every node [node] that has a conflict,
   together with the set [ts] of conflict symbols at this node. *)

let foreach_conflict_node yield =
  iter @@ fun node ->
  let transitions = A.transitions node
  and reductions = A.reductions node in
  let ts = Reductions.conflict_symbols transitions reductions in
  if not (TerminalSet.is_empty ts) then
    yield node ts

(* -------------------------------------------------------------------------- *)

(* [count z state] counts the shift and reduce items
   that involve the lookahead symbol [z]
   in the closed LR(1) state [state]. *)

let count z (state : Lr0.CLR1.t) : int * int =
  Item.Map.fold (fun item ts (shift, reduce) ->
    match Item.classify item with
    | Item.Shift (Symbol.T t, _)
      when Terminal.equal t z ->
        shift + 1, reduce
    | Item.Reduce _
      when TerminalSet.mem z ts ->
        shift, reduce + 1
    | _ ->
        shift, reduce
  ) state (0, 0)

(* [derivations z start input state] builds a map of items to derivations.
   [z] is the terminal symbol of interest. [start], [input], and [state] are
   the start item, sequence of input symbols, and closed goal state. *)

(* We build a map that contains at most one shift item and as many reduce
   items as possible. Including multiple shift items would not help produce
   a better explanation. *)

let derivations z start input (state : Lr0.CLR1.t) : Derivation.t Item.Map.t =
  let still_looking_for_shift_item = ref true in
  Item.Map.fold (fun item ts derivations ->
    match Item.classify item with
    | Item.Shift (Symbol.T t, _)
      when !still_looking_for_shift_item && Terminal.equal t z ->
        still_looking_for_shift_item := false;
        let derivation = explain_shift_item start input item in
        Item.Map.add item derivation derivations
    | Item.Reduce _
      when TerminalSet.mem z ts ->
        let derivation = explain_reduce_item z start input item in
        Item.Map.add item derivation derivations
    | _ ->
        derivations
  ) state Item.Map.empty

(* -------------------------------------------------------------------------- *)

(* Textual helpers. *)

let conflict_kind shift reduce =
  if shift > 0 && reduce > 1 then
    "shift/reduce/reduce"
  else if shift > 0 then
    "shift/reduce"
  else
    "reduce/reduce"

let if_plural ts text =
  if TerminalSet.cardinal ts > 1 then text else ""

(* -------------------------------------------------------------------------- *)

(* [try_explain out node ts] explains a conflict at [node] on the symbols
   [ts]. The explanation is written to the output channel [out]. In rare
   cases, discussed further on, this attempt can fail; then [Oops] is
   raised. *)

(* To explain a conflict at [node], we construct a partial canonical LR(1)
   automaton, looking for a conflict in a state of the canonical automaton
   that corresponds to [node]. *)

(* If Pager's original compatibility criterion is used, then Pager's
   algorithm can merge two states as soon as one of them has a conflict.
   Therefore we cannot be too specific about the conflict that we expect to
   find in the canonical automaton. So, we must supply a set [ts] of
   conflict symbols and accept any conflict that involves one of them. *)

(* Our variant of Pager's compatibility criterion is less aggressive, and
   can create a new conflict on the symbol [t], by merging two states, only
   if one of these states already has a conflict on the symbol [t].
   Therefore, if desired, we would be able to explain a conflict on every
   symbol in the set [ts]. Nevertheless, to avoid drowning the user with
   information, we prefer to focus on a single arbitrary symbol in [ts]. *)

let try_explain out node ts =

  (* Construct a partial canonical automaton. *)
  let module P = LR1Partial.Run(A)(struct
    let goal = node
    let conflicts = ts
  end) in
  assert (Item.is_start P.source);
  let closure = Lr0.CLR1.closure P.goal in

  (* Determine what kind of conflict was found. *)
  let shift, reduce = count P.t closure in
  let kind = conflict_kind shift reduce in

  (* Explain how the conflict state is reached. *)
  fprintf out "\n\
    ** Conflict (%s) in state %d.\n\
    ** Token%s involved: %s\n%s\
    ** This state is reached from %s after reading:\n\n%s\n"
    kind (A.encode node)
    (if_plural ts "s")
    (TerminalSet.print ts)
    (if_plural ts @@
      sprintf "** The following explanations concentrate on token %s.\n"
        (Terminal.print P.t)
    )
    (Nonterminal.print false (Item.get_start P.source))
    (Symbol.print_array false P.path)
  ;

  (* Examine the items in that state, focusing on one particular symbol.
     Out of the shift items, we explain just one -- this seems enough.
     We explain each of the reduce items. *)

  (* Build a mapping of items to derivations. *)
  let derivations = derivations P.t P.source P.path closure in

  (* Factor out the common context among all derivations, so as to avoid
     repeating it. This helps prevent derivation trees from drifting too
     far away towards the right. It also helps produce sub-derivations
     that are quite compact. *)
  let context, derivations = Derivation.factor derivations in

  (* Display the common context. *)
  fprintf out
    "\n** The derivations that appear below have the following common factor:\
     \n** (The question mark symbol (?) represents the spot where the derivations begin to differ.)\n\n";
  Derivation.printc out context;

  (* Then, display the sub-derivations. *)
  derivations |> Item.Map.iter @@ fun item derivation ->

    fprintf out
      "\n** In state %d, looking ahead at %s, "
      (A.encode node)
      (Terminal.print P.t);

    begin match Item.classify item with
    | Item.Shift _ ->
        fprintf out
          "shifting is permitted\n** because of the following sub-derivation:\n\n"
    | Item.Reduce prod ->
        fprintf out
          "reducing production\n** %s\n** is permitted because of the following sub-derivation:\n\n"
          (Production.print prod)
    end;

    Derivation.print out derivation

(* This comment is emitted when we are unable to explain a conflict. *)

let construction_mode_comment () =
  match X.construction_mode with
  | `LALR ->
      "This may be an artificial conflict caused by your use of --lalr"
  | `Canonical
  | `InclusionOnly
  | `Pager ->
      "Please send your grammar to Menhir's developers"

(* [cannot_explain out node ts] is invoked when we are unable to explain a
   conflict at [node] on the symbols [ts]. This can happen either because the
   automaton has been butchered by conflict resolution directives or because
   [--lalr] is enabled, giving rise to so-called mysterious (unexplainable)
   LALR conflicts. We write an error message to the .conflicts file and
   continue. *)

let cannot_explain out node ts =
  incr unexplainable;
  fprintf out "\n\
    ** Conflict (unexplainable) in state %d.\n\
    ** Token%s involved: %s\n\
    ** %s.\n%!"
    (A.encode node)
    (if_plural ts "s")
    (TerminalSet.print ts)
    (construction_mode_comment())

let explain out node ts =
  try
    try_explain out node ts;
    flush out
  with LR1Partial.Oops ->
    cannot_explain out node ts

(* -------------------------------------------------------------------------- *)

(* The main loop. *)

(* 2018/09/05: when [--explain] is enabled, always create a fresh .conflicts
   file (wiping out any pre-existing file), even if there are in fact no
   conflicts. This should avoid confusion with outdated .conflicts files. *)

let write filename =
  IO.write filename @@ fun out ->
  foreach_conflict_node @@ fun node ts ->
  explain out node ts

let write c filename =
  unexplainable := 0;
  write filename;
  log_unexplainable c

end (* Run *)
