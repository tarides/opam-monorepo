(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open MLazy
open MiddleAPI

module Make
(Lr1 : LR1)
(X : REPRESENT_SETTINGS)
(E : sig
  open Report

  (**This channel is used to log information messages. *)
  val info: channel

end)
= struct
open Lr1.Lr0.G
open E

(* -------------------------------------------------------------------------- *)

(* This functor transforms an analysis where a stack shape is an array of
   symbols into a simpler analysis where a stack shape is just a height. *)

module Height (S : SHAPE with type shape = Symbol.t array) = struct
  include S
  let show_shape height =
    Printf.sprintf "%d" height
  let node_shape node =
    Array.length (node_shape node)
  let production_shape node =
    Array.length (production_shape node)
  let goto_shape node =
    Array.length (goto_shape node)
end

(* -------------------------------------------------------------------------- *)

(* This functor prints the results of an analysis to an output channel. *)

module Print (S : SHAPE with type node := Lr1.node) = struct

  open S
  open G

  let print_node_shape c =
    Lr1.iter @@ fun node ->
    Report.log c "%s_node_shape(%s) = %s"
      (match variant with `Short -> "short" | `Long -> "long")
      (Lr1.print node)
      (show_shape (node_shape node))

  let print_production_shape c =
    Production.iterx @@ fun prod ->
    Report.log c "%s_production_shape(%s) = %s"
      (match variant with `Short -> "short" | `Long -> "long")
      (Production.print prod)
      (show_shape (production_shape prod))

  let print_goto_shape _c =
    (* At the moment, this information is not printed. *)
    ()

  let print c =
    if Report.live c then begin
      print_node_shape c;
      print_production_shape c;
      print_goto_shape c
    end

end

(* -------------------------------------------------------------------------- *)

(* [foreach_source symbol] enumerates the nodes that have an outgoing edge
   labeled [symbol]. *)

let foreach_source symbol yield =
  Lr1.targets symbol |> Lr1.NodeSet.iter @@ fun target ->
  Lr1.predecessors target |> Lr1.NodeSet.iter yield

(* -------------------------------------------------------------------------- *)

(* Obtain the shape of the stack, as a vector of symbols, at every node. *)

(* This information is part of the short invariant. In particular, it
   determines the height of the stack (everywhere) in the short invariant. *)

module StackSymbolsShort =
  StackSymbols.Short(Lr1)()

(* Then, compute the shape of the stack, as a vector of sets of nodes,
   at every node. *)

(* This information is exploited for several purposes:

   1. Inside this module, if [represent_states] is [false] (which is the
      default), then this information is used to determine which states
      must be represented.

   2. Outside this module, the Rocq back-end needs this information.

   For large automata, this computation can be somewhat costly; e.g., about
   1.5 seconds for cca_cpp, a 10,000-state automaton.

   Fortunately, our analyses are written in a lazy style, so they are
   performed only if their result is demanded. If they are not used,
   they are skipped. *)

module StackStatesShort =
  StackStates.Run(Lr1)(Height(StackSymbolsShort))

(* -------------------------------------------------------------------------- *)

(* Determine which states must be represented, that is, explicitly pushed
   onto the stack. For simplicity, a state is either always represented or
   never represented. More fine-grained strategies, where a single state is
   sometimes pushed onto the stack and sometimes not pushed, depending on
   which outgoing transition is being taken, are conceivable, but quite
   tricky, and probably not worth the trouble.

   The conditions listed here are designed to allow the code back-end
   to perform a case analysis on a state, when needed. Thus, it is
   necessary to have detailed knowledge of the code back-end in order
   to understand why these conditions are necessary and sufficient.
   This is not ideal, but the alternative (which would be to generate
   code first and deduce, by analysis of the code, which states must
   be represented) would be complex as well.

   (1) If two states are liable to appear within a single stack cell,
   then one is represented if and only if the other is represented.
   This ensures that the structure of stacks is known everywhere and
   that we can propose types for stacks.

   (2a) If a state [s] has an outgoing transition along nonterminal
   symbol [nt], and if the [goto] table for symbol [nt] has more than
   one target, then state [s] is represented. (When the [goto] table
   has only one target, this means that all [goto] transitions labeled
   [nt] lead to the same state, so the [goto] function can jump to
   this state without performing a case analysis.)

   (2b) If a state [s] has an outgoing transition labeled [nt], and if
   one of the productions associated with [nt] is non-epsilon and needs
   [beforeendp], then [s] must be represented. This caters for the (rare)
   situation where the code back-end must perform a case analysis of
   the current state in order to peek at the top stack cell. *)

module RepresentedStates () : sig

  (**[represented s] determines whether the state [s] is represented. *)
  val represented : Lr1.node -> bool

end = struct

  (* This functor is invoked only if [represent_states] is false. *)

  let () =
    assert (not X.represent_states)

  (* Mutable data. *)

  let rep : bool UnionFind.point array =
    Lr1.init @@ fun _ ->
    UnionFind.fresh false

  (* Getter. *)

  let represented state : bool UnionFind.point =
    rep.(Lr1.encode state)

  (* Setter. *)

  let represent state =
    UnionFind.set (represented state) true

  (* Enforce condition (1) above. *)

  (* We say that a set of states is equi-represented when either all of them
     are represented or all of them are not represented. *)

  (* [share shape] ensures that in each cell in the shape [shape], the states
     are equi-represented. *)

  let share shape =
    shape |> Array.iter @@ fun states ->
    let point = UnionFind.fresh false in
    states |> Lr1.NodeSet.iter @@ fun state ->
    UnionFind.union point (represented state)

  let () =
    Lr1.iter @@ fun node ->
    share (StackStatesShort.node_shape node)

  let () =
    Production.iter @@ fun prod ->
    share (StackStatesShort.production_shape prod)

  (* Enforce condition (2a) above. *)

  let () =
    Nonterminal.iter @@ fun nt ->
    let symbol = Symbol.N nt in
    if Lr1.NodeSet.cardinal (Lr1.targets symbol) > 1 then
      foreach_source symbol represent

  (* Enforce condition (2b) above. *)

  let () =
    Production.iterx @@ fun prod ->
    if Production.length prod > 0
    && Action.has_beforeend (Production.action prod)
    then
      let nt = Production.nt prod in
      let symbol = Symbol.N nt in
      foreach_source symbol represent

  (* Define an accessor. *)

  let represented state : bool =
    UnionFind.get (represented state)

  (* Statistics. *)

  let count =
    Lr1.fold (fun node count ->
      if represented node then count + 1 else count
    ) 0

  let () =
    Report.log info "%d out of %d states are represented." count Lr1.n

end (* RepresentedStates *)

(* This is the final definition of the function [represented]. *)

(* If [represent_states] is [true] then every state is represented
   and the above computation is skipped. *)

let always _ =
  true

let represented : Lr1.node -> bool =
  if X.represent_states then
    always
  else
    let module R = RepresentedStates() in
    R.represented

let[@inline] unrepresented node =
  not (represented node)

(* [representeds] is analogous to [represented], except that its argument
   is a (lazy) set of states, all of which must be equi-represented. *)

(* If [represent_states] is [true] then every state is represented so we
   can return [true] without forcing the suspension [states]. *)

let representeds (states : Lr1.NodeSet.t Lazy.t) =
  X.represent_states ||
  let states = Lazy.force states in
  not (Lr1.NodeSet.is_empty states) &&
  represented (Lr1.NodeSet.choose states)

(* Printing. *)

let print_represented c =
  if Report.live c then
    Lr1.iter @@ fun node ->
    Report.log c "represented(%s) = %b"
      (Lr1.print node) (represented node)

(* -------------------------------------------------------------------------- *)

(* Determine which symbols must keep track of their start or end positions.   *)

open Keyword

(* A variable is a pair [(symbol, WhereStart)] or [(symbol, WhereEnd)]. *)

type variable =
  Symbol.t * where (* WhereStart or WhereEnd *)

(* An implementation of maps whose keys are variables. *)

module M : Fix.IMPERATIVE_MAPS with type key = variable
= struct
  type key = variable
  type 'data t = {
    mutable startp: 'data SymbolMap.t;
    mutable endp:   'data SymbolMap.t;
  }
  open SymbolMap
  let create() =
    { startp = empty; endp = empty }
  let clear m =
    m.startp <- empty; m.endp <- empty
  let add (sym, where) data m =
    match where with
    | WhereStart ->
        m.startp <- add sym data m.startp
    | WhereEnd ->
        m.endp <- add sym data m.endp
    | WhereSymbolStart ->
        assert false
  let find (sym, where) m =
    match where with
    | WhereStart ->
        find sym m.startp
    | WhereEnd ->
        find sym m.endp
    | WhereSymbolStart ->
        assert false
  let iter f m =
    iter (fun sym -> f (sym, WhereStart)) m.startp;
    iter (fun sym -> f (sym, WhereEnd)) m.endp
end

(* -------------------------------------------------------------------------- *)

(* We now determine which positions must be kept track of. For simplicity, we
   do this on a per-variable basis. That is, for each variable (as defined
   above), either we never keep track of position information, or we always
   do. In other words, we compute two sets of symbols: those that keep track
   of their start position and those that keep track of their end position.

   A symbol on the right-hand side of a production must keep track of its
   (start or end) position if that position is explicitly requested by a
   semantic action.

   Furthermore, if the left-hand symbol of a production must keep track of its
   start (resp. end) position, then the first (resp. last) symbol of its
   right-hand side (if there is one) must do so as well. That is, unless the
   right-hand side is empty. *)

(* 2015/11/11. When a production [prod] is reduced, the cell that lies at
   the top of the stack (after the cells that correspond to the production's
   right-hand side have been popped, and before a new cell is pushed onto the
   stack) may be consulted for its end position. This implies that this cell
   must exist and must store an end position! Now, when does this happen?

   In the code back-end, this happens only if [prod] is not an epsilon
   production and only if the current state is not an initial state. Indeed,
   for epsilon productions, we read the register [endp] instead of peeking at
   the top stack cell, and at initial states, we read the register [initp].
   Furthermore, this happens only if the semantic action explicitly mentions
   the keyword [$endpos($0)].

   Now, if this happens, what should we do? If this happens in a state [s]
   whose incoming symbol is [sym], then [sym] must keep track of its end
   position. *)

(* I will say that this is a more sophisticated than I would like. The code
   back-end has been known for its efficiency and I am trying to maintain this
   property -- in particular, I would like to keep track of no positions at all,
   if the user doesn't use any position keyword. But I am suffering. *)

(* If [represent_positions] is true then this computation is skipped and every
   position is tracked. *)

(* This analysis is based on the grammar; the automaton is not inspected. *)

module TrackPositions () : sig

  (**[track_startp symbol] determines whether the symbol [symbol]
     must keep track of a start position. *)
  val track_startp: Symbol.t -> bool

  (**[track_startp symbol] determines whether the symbol [symbol]
     must keep track of an end position. *)
  val track_endp: Symbol.t -> bool

end = struct

  let () =
    assert (not X.represent_positions)

  module F =
    LatticeInequalities.Make(M)(Fix.Prop.Boolean)()

  let examine_non_epsilon_production prod =
    let nt, rhs = Production.def prod in
    let length = Array.length rhs in
    assert (length > 0);
    (* If [nt] keeps track of its start position, then the first symbol
       in the right-hand side must do so as well. *)
    F.edge (Symbol.N nt, WhereStart) (rhs.(0), WhereStart);
    (* If [nt] keeps track of its end position, then the last symbol
       in the right-hand side must do so as well. *)
    F.edge (Symbol.N nt, WhereEnd) (rhs.(length - 1), WhereEnd)

  let examine_position_keywords prod =
    let nt, rhs = Production.def prod
    and action = Production.action prod in
    let length = Array.length rhs in
    Action.keywords action |> KeywordSet.iter @@ function
    | Position (Before, _, _) ->
        (* See the long comment above (2015/11/11). The condition is as
           follows: if [prod] refers to [$endpos($0)], if the node [node]
           carries an outgoing transition labeled [nt], if the incoming
           symbol of [node] is [sym], and if [prod] is not an epsilon
           production, then [sym] must keep track of its end position. *)
        if length > 0 then
          foreach_source (Symbol.N nt) @@ fun node ->
          Lr1.incoming_symbol node |> Option.iter @@ fun sym ->
          F.source true (sym, WhereEnd)
    | Position (Left, _, _) ->
        (* [$startpos] and [$endpos] have been expanded away. *)
        assert false
    | Position (_, _, FlavorLocation) ->
        (* [$loc] and [$sloc] have been expanded away. *)
        assert false
    | Position (RightNamed _, WhereSymbolStart, _) ->
        (* [$symbolstartpos(x)] does not exist. *)
        assert false
    | Position (RightNamed id, where, _) ->
        (* If the semantic action mentions [$startpos($i)], then the
           [i]-th symbol in the right-hand side must keep track of
           its start position. Similarly for end positions. *)
        Production.identifiers prod |> Array.iteri @@ fun i id' ->
        if id = id' then
          F.source true (rhs.(i), where)

  (* Gather constraints in a loop over every (non-start) production. *)

  let () =
    Production.iterx @@ fun prod ->
    (* Handle non-epsilon productions. *)
    if Production.length prod > 0 then
      examine_non_epsilon_production prod;
    (* Examine every production's position keywords. *)
    examine_position_keywords prod

  let track : variable -> bool option =
    let module S = F.Solve() in
    S.solution

  let track : variable -> bool =
    fun x -> Option.value (track x) ~default:false

  let track_startp symbol =
    track (symbol, WhereStart)

  let track_endp symbol =
    track (symbol, WhereEnd)

  let sum_over_every_symbol (f : Symbol.t -> bool) : int =
    MRef.with_state 0 @@ fun c ->
    Symbol.iter @@ fun sym ->
    if f sym then incr c

  let () =
    Report.log info
      "%d out of %d symbols keep track of their start position.\n\
       %d out of %d symbols keep track of their end position."
      (sum_over_every_symbol track_startp) (Terminal.n + Nonterminal.n)
      (sum_over_every_symbol track_endp) (Terminal.n + Nonterminal.n)

end (* TrackPositions *)

let track_startp, track_endp =
  if X.represent_positions then
    always, always
  else
    let module T = TrackPositions() in
    T.track_startp, T.track_endp

(* -------------------------------------------------------------------------- *)

(* Constructors and accessors for the types [shape] and [cell]. *)

(* See the signature [KINDERGARTEN]. *)

(* A cell is a tuple of everything one might ever wish to know about a stack
   cell. The field [states] is lazy, so that its computation is performed only
   on demand. *)

type cell = {
  symbol: Symbol.t;
  states: Lr1.NodeSet.t Lazy.t;
  holds_semv: bool;
  holds_state: bool;
  holds_startp: bool;
  holds_endp: bool;
}

(* A shape is an immutable array of cells. *)

type shape =
  cell array

(* Constructors. *)

let empty : shape =
  [||]

(* If [represent_values] is true then every semantic value is stored. *)

let has_semv symbol =
  X.represent_values ||
  match symbol with
  | Symbol.N _nt ->
      true
  | Symbol.T tok ->
      match Terminal.ocamltype tok with
      | None ->
          (* This terminal symbol has a semantic value of type [unit].
             This semantic value is omitted in every stack cell. *)
          false
      | Some _ocamltype ->
          true

let cell symbol states =
  let holds_semv = has_semv symbol in
  let holds_state = representeds states in
  let holds_startp, holds_endp = track_startp symbol, track_endp symbol in
  { symbol; states; holds_semv; holds_state; holds_startp; holds_endp }

(* Accessors. *)

let symbol cell =
  cell.symbol

let holds_semv cell =
  cell.holds_semv

let holds_state cell =
  cell.holds_state

let holds_startp cell =
  cell.holds_startp

let holds_endp cell =
  cell.holds_endp

let present cell =
  cell.holds_state || cell.holds_semv || cell.holds_startp || cell.holds_endp

let states cell =
  let states = Lazy.force cell.states in
  assert (
     holds_state cell && Lr1.NodeSet.for_all represented states ||
     not (holds_state cell) && Lr1.NodeSet.for_all unrepresented states
  );
  states

let similar cell1 cell2 =
  Symbol.equal cell1.symbol cell2.symbol &&
  cell1.holds_state = cell2.holds_state
    (* The fields [holds_semv], [holds_startp] and [holds_endp]
       do not need to be compared, because they are determined
       by the field [symbol]. The field [states] does not need
       to be compared because it does not influence the layout
       of the cell; comparing the field [holds_state] suffices. *)

(* [clobber] creates cell(s) whose [states] field must not be read. *)

let clobber cell =
  let states = lazy (assert false) in
  { cell with states }

let clobber shape =
  Array.map clobber shape

(* [meet sh1 sh2] is the meet of the two shapes [sh1] and [sh2], that is, the
   logical conjunction of the information contained in [sh1] and [sh2]. It is
   computed as follows:

   1- If the shapes [sh1] and [sh2] agree (that is, are pointwise similar) on
      a suffix of length [min (length sh1) (length s2)], then they are
      compatible. Their meet has length [max (length sh1) (length sh2)].

      We could or should compute the intersection of their [states] fields,
      but we happen to know that this information is not needed by the
      StackLang type-checker, so we omit this computation.

      One might imagine that if the intersection of two [states] fields is
      empty then the meet of the two shapes should be bottom. This is
      semantically true, but we do not want to (and cannot) exploit this idea,
      for the following reason. When [meet] returns [None], the StackLang
      type-checker declares a branch dead. The OCaml type-checker later checks
      that the branch is indeed dead, based on the type information that we
      provide. But the OCaml types that we provide do not contain information
      about sets of states. Therefore this information must not be exploited
      to declare that a branch is dead.

   2- Otherwise, [sh1] and [sh2] are incompatible: they contradict each other.
      Then, their meet is bottom: [meet sh1 sh2] is [None]. *)

let meet sh1 sh2 =
  let n1, n2 = Array.length sh1, Array.length sh2 in
  let n = min n1 n2 in
  let suffix1, suffix2 = MArray.suffix sh1 n, MArray.suffix sh2 n in
  if MArray.for_all2 similar suffix1 suffix2 then
    (* [sh1] and [sh2] agree on a suffix of length [n]. The meet is then the
       longest of the two shapes, up to the [states] fields, which we do not
       compute. *)
    Some (clobber (if n1 < n2 then sh2 else sh1))
  else
    (* [sh1] and [sh2] disagree on a suffix of length [n]. *)
    None

let push, pop, top, filter =
  MArray.(push, pop, last, filter)

let get, length, fold_left, append, to_list =
  Array.(get, length, fold_left, append, to_list)

let split shape k =
  if k = 0 then
    (* A fast path for the most common case. *)
    shape, [||]
  else
    let n = Array.length shape in
    assert (0 <= k && k <= n);
    let lower = Array.sub shape 0 (n - k) in
    let upper = MArray.suffix shape k in
    lower, upper

let show_shape shape =
  if length shape > 0 then
    Symbol.print_array' false (Array.map symbol shape)
  else
    " <empty>"

(* -------------------------------------------------------------------------- *)

(* Suppose we have a function [symbols] that maps keys to vectors of symbols
   and a function [states] that maps keys to vectors of sets of states. We
   wish to have a function that maps keys to shapes (vectors of cells). *)

(* We allow [states] to produce a shorter vector than the one produced by
   [symbols]. (The long invariant produces this situation, because [validate]
   rejects sets of states that are not equi-represented.) We tolerate this and
   truncate [symbols] so as to match [states]. *)

let combine
    (symbols : 'key ->      Symbol.t array)
    (states  : 'key -> Lr1.NodeSet.t array)
  :            'key -> shape
  =
fun key ->
let symbols = symbols key in
  if X.represent_states then
    (* If all states are represented then the vectors [states] and [symbols]
       must have the same length; no truncation is necessary. Furthermore, we
       want to avoid eagerly evaluating [states] because that would cause the
       on-demand analysis to run. *)
    let k = Array.length symbols in
    Array.init k @@ fun i ->
    cell symbols.(i) (lazy (states key).(i))
  else
    (* [states] may be shorter than [symbols]. *)
    let states = states key in
    assert (Array.length symbols >= Array.length states);
    (* Truncate [symbols] so that its length matches [states]. *)
    let k = Array.length states in
    let symbols = MArray.suffix symbols k in
    Array.init k @@ fun i ->
    cell symbols.(i) (lazy states.(i))

let[@inline] publish tabulate symbols states =
  lazily tabulate (combine symbols states)

(* -------------------------------------------------------------------------- *)

(* Publish the short invariant. *)

module Short = struct

  module G = Lr1.Lr0.G

  let variant = `Short

  let node_shape : Lr1.node -> shape =
    publish Lr1.tabulate
      StackSymbolsShort.node_shape
      StackStatesShort.node_shape

  let production_shape : Production.t -> shape =
    publish Production.tabulate
      StackSymbolsShort.production_shape
      StackStatesShort.production_shape

  let goto_shape : Nonterminal.t -> shape =
    publish Nonterminal.tabulate
      StackSymbolsShort.goto_shape
      StackStatesShort.goto_shape

  let show_shape =
    show_shape

  let print_stack_states c =
    let module P = Print(StackStatesShort) in
    P.print c

end

(* -------------------------------------------------------------------------- *)

(* Compute and publish the long invariant. *)

(* Fortunately, all of the building blocks are at hand, so this is easy. *)

(* It is not obvious that the sets of states that are computed here are
   equi-represented. Yet, we need this property. It is necessary in the
   translation of the invariant to an OCaml GADT.

   One might think that this property is likely true, because every set of
   states that appears somewhere in the long invariant must also appear
   somewhere in the short invariant, and we know that every set of states in
   the short invariant is equi-represented, because we have explicitly imposed
   this requirement. However, this is incorrect: testing shows that not every
   set of states in the long invariant is equi-represented.

   To work around this problem, we truncate the long invariant so as to forget
   about any stack cells that are not equi-represented. This makes the long
   invariant shorter (in some places) that it could otherwise be. This seems
   to work well. *)

module StackSymbolsLong =
  StackSymbols.Long(Lr1)()

module StackStatesLong =
  StackStates.Run(Lr1)(Height(StackSymbolsLong))

module Long = struct

  module G = Lr1.Lr0.G

  (* Validation: we truncate shapes so as to ensure that every set of states
     that is exposed to the client is equi-represented. *)

  let equi_represented nodes =
    Lr1.NodeSet.for_all represented nodes ||
    Lr1.NodeSet.for_all unrepresented nodes

  let validate (shape : StackStatesLong.shape) =
    MArray.greatest_suffix_forall equi_represented shape

  let validate (shape : StackStatesLong.shape) =
    if X.represent_states then
      (* If every state must be represented, then every set of states is
         equi-represented, so validation always succeeds. *)
      shape
    else
      validate shape

  let node_shape s =
    validate @@ StackStatesLong.node_shape s

  let production_shape prod =
    validate @@ StackStatesLong.production_shape prod

  let goto_shape nt =
    validate @@ StackStatesLong.goto_shape nt

  (* Publish. *)

  let variant = `Long

  let node_shape : Lr1.node -> shape =
    publish Lr1.tabulate
      StackSymbolsLong.node_shape
      node_shape

  let production_shape : Production.t -> shape =
    publish Production.tabulate
      StackSymbolsLong.production_shape
      production_shape

  let goto_shape : Nonterminal.t -> shape =
    publish Nonterminal.tabulate
      StackSymbolsLong.goto_shape
      goto_shape

  let show_shape =
    show_shape

  let print_stack_states c =
    let module P = Print(StackStatesLong) in
    P.print c

end (* Long *)

(* -------------------------------------------------------------------------- *)

end (* Run *)
