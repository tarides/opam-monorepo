(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open PlainSyntax
open PlainSyntaxAccessors

module Make
(G : sig
  val grammar: grammar
end)
(X : sig
  val topological_numbering: bool
end)
() = struct

open G

(* -------------------------------------------------------------------------- *)

(* [index] indexes a list of (distinct) strings, that is, assigns an
   integer index to each string and builds mappings both ways between
   strings and indices. *)

(* The order of the list is respected: the [i]-th element of the list
   [strings] receives the index [i]. *)

let index (strings : string list) : int * string array * int StringMap.t =
  let name = Array.of_list strings
  and n, map = List.fold_left (fun (n, map) s ->
    n+1, StringMap.add s n map
  ) (0, StringMap.empty) strings in
  assert (StringMap.cardinal map = n);
  n, name, map

(* -------------------------------------------------------------------------- *)

(* Nonterminal symbols. *)

module Nonterminal = struct

  type t = int

  (* Determine how many nonterminals we have, and build mappings both ways
     between names and indices. For each user start symbol [S], an internal
     start symbol [S'] is created. No name collision is possible because the
     quote character ['] cannot occur in the name of a symbol. *)

  let new_start_nonterminals =
    StringSet.fold (fun symbol ss -> (symbol ^ "'") :: ss) grammar.start_symbols []

  let original_nonterminals =
    nonterminals grammar

  let start =
    List.length new_start_nonterminals

  let (n : int), (name : string array), (map : int StringMap.t) =
    index (new_start_nonterminals @ original_nonterminals)

  let is_internal_start nt =
    nt < start

  let is_user_start nt =
    StringSet.mem name.(nt) grammar.start_symbols

  let lookup symbol =
    StringMap.find symbol map

  let print normalize nt =
    if normalize then
      MString.normalize name.(nt)
    else
      name.(nt)

  include Fix.Numbering.OperationsForIntSegment(struct let n = n end)

  let[@inline] iterx f =
    MInt.iterij start n f

  let[@inline] foldx f accu =
    MInt.foldij start n f accu

  let[@inline] mapx f =
    MInt.mapij start n f

  let ocamltype : t -> ocamltype option =
    tabulate @@ fun nt ->
    try
      Some (StringMap.find name.(nt) grammar.types)
    with Not_found ->
      None

  let ocamltype nt =
    assert (not (is_internal_start nt));
    ocamltype nt

  let ocamltype_of_start_symbol nt =
    Option.get (ocamltype nt)

  let positions : t -> ranges =
    tabulate @@ fun nt ->
    if is_internal_start nt then []
    else (StringMap.find name.(nt) grammar.rules).positions

  let attributes : attributes array =
    Array.make n []

  let merge_function : merge_fun option array =
    Array.make n None

  let () =
    grammar.rules |> StringMap.iter @@ fun symbol rule ->
    let nt = lookup symbol in
    attributes.(nt) <- rule.attributes;
    merge_function.(nt) <- rule.merge

  let attributes nt =
    attributes.(nt)

  let merge_function nt =
    merge_function.(nt)

  let[@inline] iter_internal_start f =
    MInt.iteri start f

  (* Build a list of the user start symbols and sort it, so that
     elements appear in the correct order. *)

  let user_start_symbols =
    grammar.start_symbols
    |> StringSet.elements
    |> List.map lookup
    |> List.sort Int.compare

  (* Generating the [state] GADT (in the code back-end) or generating the
     inspection API (in the table back-end) requires that the type of every
     nonterminal symbol be known. *)

  let symbols_without_ocaml_type () =
    foldx (fun nt accu ->
      match ocamltype nt with
      | Some _ ->
          accu
      | None ->
          nt :: accu
    ) []

  let check_every_symbol_has_ocaml_type reason =
    let nts = symbols_without_ocaml_type() in
    if nts <> [] then
      let b = Buffer.create 1024 in
      Printf.bprintf b "\
        the %s requires the type of every nonterminal symbol to be\n\
        known. Please specify the type of every symbol via %%type declarations, or\n\
        enable type inference (look up --infer in the manual).\n\
        Type inference is automatically enabled when Menhir is used via Dune,\n\
        provided the dune-project file says (using menhir 2.0) or later.\n"
        reason
      ;
      Printf.bprintf b "The types of the following nonterminal symbols are unknown:";
      List.iter (fun nt ->
        Printf.bprintf b "\n%s" (print false nt)
      ) nts;
      Report.Just.error [] "%s" (Buffer.contents b)

end (* Nonterminal *)

(* Sets and maps of nonterminal symbols. *)

module NonterminalMap =
  IntMap.Make(Nonterminal)()

module NonterminalSet =
  NonterminalMap.Domain

(* -------------------------------------------------------------------------- *)

(* Terminal symbols. *)

module Terminal = struct

  type t = int

  (* Determine how many terminals we have and build mappings
     both ways between names and indices. A new terminal "#"
     is created. A new terminal "error" is created. The fact
     that the integer code assigned to the terminal "#"
     is the last one is exploited in the table-based back-end.
     (The right-most row of the action table is not created.)

     Pseudo-tokens (used in %prec declarations, but never
     declared using %token) are filtered out. *)

  (* In principle, the number of the [error] token is irrelevant.
     It is currently 0, but we do not rely on that. *)

  let (n : int), (name : string array), (map : int StringMap.t) =
    index ("error" :: tokens grammar @ [ "#" ])

  let lookup name =
    StringMap.find name map

  include Fix.Numbering.OperationsForIntSegment(struct let n = n end)

  let print t =
    name.(t)

  let sharp =
    lookup "#"

  let error =
    lookup "error"

  let special t =
    t = sharp || t = error

  let real t =
    t <> sharp && t <> error

  let eof =
    try
      Some (lookup "EOF")
    with Not_found ->
      None

  let default = (* applicable to [error] and [#] *)
    {
      tk_precedence    = UndefinedPrecedence;
      tk_associativity = UndefinedAssoc;
      tk_ocamltype     = None;
      tk_is_declared   = true;
      tk_position      = Range.dummy;
      tk_attributes    = [];
      tk_alias         = None;
    }

  let properties =
    Array.init n @@ fun t ->
    try
      StringMap.find name.(t) grammar.tokens
    with Not_found ->
      assert (special t);
      default

  let ocamltype t =
    properties.(t).tk_ocamltype

  let associativity t =
    properties.(t).tk_associativity

  let attributes t =
    properties.(t).tk_attributes

  let alias t =
    properties.(t).tk_alias

  let unquoted_alias t =
    alias t
    |> Option.map MString.unquote

  let () =
    assert (sharp = n - 1)

  let[@inline] iterx f =
    MInt.iteri sharp f

  let[@inline] foldx f accu =
    MInt.foldi sharp f accu

  let[@inline] mapx f =
    MInt.mapi sharp f

  let[@inline] initx f =
    MInt.initi sharp f

  let () =
    assert (error = 0)

  let[@inline] iter_real f =
    MInt.iterij 1 sharp f

  let[@inline] fold_real f accu =
    MInt.foldij 1 sharp f accu

  let[@inline] map_real f =
    MInt.mapij 1 sharp f

  let tokens_without_an_alias =
    MRef.with_state 0 @@ fun accu ->
    iter_real @@ fun t ->
    if alias t = None then
      incr accu

  let every_token_has_an_alias =
    tokens_without_an_alias = 0

end (* Terminal *)

(* -------------------------------------------------------------------------- *)

(* Maps of terminal symbols. *)

module TerminalMap =
  IntMap.Make(Terminal)()

(* Sets of terminal symbols are used intensively in the LR(1) construction,
   so it is important that they be as efficient as possible. *)

module TerminalSet = struct

  include TerminalMap.Domain

  let universe =
    Terminal.fold_real add empty

  (* The signature [Bitsets.API.SET] does not offer [of_list],
     so we implement this function here in a naive way. *)

  let of_list ts =
    List.fold_right add ts empty

  let pick f ts =
    let exception Found of Terminal.t in
    match
      iter (fun x -> if f x then raise (Found x)) ts
    with
    | exception Found x -> Some x
    | ()                -> None

  let print ts =
    MString.separated_iter Terminal.print " " (fun f -> iter f ts)

  (* The following definitions aim to define [identify]. *)

  (* [big_max f ts] computes the maximum of the function [f] over the
     set of terminal symbols [ts]. *)

  let big_max (f : elt -> int) (ts : t) : int =
    fold (fun t accu -> max (f t) accu) ts 0

  (* [separator] is a run of underscore characters '_' that is long
     enough so as to guarantee that no terminal symbol contains such
     a run in its name. *)

  let separator : string Lazy.t =
    lazy begin
      let longest_run t = MString.longest_run '_' (Terminal.print t) in
      let k = big_max longest_run universe in
      String.make (k+1) '_'
    end

  (* [identify ts] converts the set of terminal symbols [ts] into
     a valid OCaml identifier. The cardinal of the set is mentioned
     (for informative purposes; it is not required for disambiguation)
     and the symbols are separated using [separator]. *)

  let identify ts =
    String.concat (Lazy.force separator) (
      (* The cardinal: *)
      Printf.sprintf "c%d" (cardinal ts) ::
      (* The symbols: *)
      List.map Terminal.print (elements ts)
    )

  (* The following definitions are used in the computation of FIRST sets
     below. They are not exported outside of this file. *)

  type property =
    t

  let bottom =
    empty

  let is_maximal _ =
    false

  let leq_join =
    union

end (* TerminalSet *)

(* -------------------------------------------------------------------------- *)

(* Symbols. *)

module Symbol = struct

  type t =
    | N of Nonterminal.t
    | T of Terminal.t

  let compare sym1 sym2 =
    match sym1, sym2 with
    | N nt1, N nt2 ->
        Nonterminal.compare nt1 nt2
    | T t1, T t2 ->
        Terminal.compare t1 t2
    | N _, T _ ->
        1
    | T _, N _ ->
        -1

  let[@inline] equal sym1 sym2 =
    compare sym1 sym2 = 0

  let[@inline] is_terminal sym =
    match sym with N _ -> false | T _ -> true

  let[@inline] is_nonterminal sym =
    match sym with T _ -> false | N _ -> true

  let lookup name =
    try
      T (Terminal.lookup name)
    with Not_found ->
      try
        N (Nonterminal.lookup name)
      with Not_found ->
        assert false

  let iter f =
    Terminal.iter (fun t -> f (T t));
    Nonterminal.iter (fun nt -> f (N nt))

  let print normalize sym =
    match sym with
    | N nt ->
        Nonterminal.print normalize nt
    | T t ->
        Terminal.print t

  let non_error sym =
    match sym with
    | T t ->
        t <> Terminal.error
    | N _ ->
        true

  let print_array' normalize symbols =
    MString.with_buffer 64 @@ fun b ->
    symbols |> Array.iter @@ fun symbol ->
    Printf.bprintf b " %s" (print normalize symbol)

  let print_array_bullet normalize dot symbols =
    MString.subarray_bullet (print normalize) " " 0 dot "." symbols

  let print_subarray normalize start symbols =
    MString.subarray_bullet (print normalize) " " start (-1) "." symbols

  let print_array normalize symbols =
    print_subarray normalize 0 symbols

  let print_list normalize symbols =
    print_array normalize (Array.of_list symbols)

end (* Symbol *)

(* -------------------------------------------------------------------------- *)

(* Sets of symbols. *)

module SymbolSet = struct

  include Set.Make(Symbol)

  let print normalize symbols =
    Symbol.print_list normalize (elements symbols)

  (* The following definitions are used in the computation of symbolic FOLLOW
     sets below. They are not exported outside of this file. *)

  type property =
    t

  let bottom =
    empty

  let leq =
    subset

  let join =
    union

end (* SymbolSet *)

(* -------------------------------------------------------------------------- *)

(* Maps of symbols. *)

module SymbolMap = struct

  include Map.Make(Symbol)

  let terminals m =
    fold (fun symbol _ ts ->
      match symbol with
      | Symbol.T t -> TerminalSet.add t ts
      | Symbol.N _ -> ts
    ) m TerminalSet.empty

end (* SymbolMap *)

(* -------------------------------------------------------------------------- *)

(* Productions. *)

module Production = struct

  type t =
    int

  (* For every start symbol [S], a new production S' -> S is created.
     It is known as a start production. *)

  (* Count how many productions we have, including the start productions.
     This is [n]. *)

  let n : int =
    let n = StringMap.fold (fun _ rule n ->
      n + List.length rule.branches
    ) grammar.rules 0 in
    n + StringSet.cardinal grammar.start_symbols

  include Fix.Numbering.OperationsForIntSegment(struct let n = n end)

  (* Create a number of uninitialized tables that map a production index to
     information about this production. *)

  (* [table] maps a production to the left-hand side and right-hand side of
     this production. [identifiers] maps a production to an array of the
     identifiers that are used to name the elements of the right-hand side.
     [actions] maps a production to an optional semantic action. (Only the
     start productions have none.) [positions] maps a production to an array
     of the positions (in the .mly file) of the elements of the right-hand
     side. [rhs_attributes] maps a production to an array of the attributes
     attached to the elements of the right-hand side. [prec_decl] maps a
     production to an optional [%prec] annotation. [production_level] maps a
     production to a production level. [attributes] maps a production to the
     list of its attributes. *)

  let table : (Nonterminal.t * Symbol.t array) array =
    Array.make n (-1, [||])

  let identifiers : identifier array array =
    Array.make n [||]

  let actions : action option array =
    Array.make n None

  let positions : Range.range list array =
    Array.make n []

  let rhs_attributes : attributes array array =
    Array.make n [||]

  let prec_decl : prec_annotation array =
    Array.make n None

  let production_level : production_level array =
    (* The start productions receive a level that pretends that they
       originate in a fictitious "builtin" file. So, a reduce/reduce
       conflict that involves a start production will not be solved. *)
    let dummy = ProductionLevel (InputFile.builtin, 0) in
    Array.make n dummy

  let attributes : attributes array =
    Array.make n []

  (* [ntprods] maps a nonterminal symbol to the interval of its productions. *)

  let ntprods : (int * int) array =
    Array.make Nonterminal.n (-1, -1)

  (* Create the start productions, populating the above arrays as appropriate.
     [start] is the number of start productions, therefore also the index of
     the first non-start production. [startprods] is a mapping of the start
     symbols to the corresponding start productions. *)

  let (start : int), (startprods : t NonterminalMap.t) =
    StringSet.fold (fun nonterminal (k, startprods) ->
      let nt = Nonterminal.lookup nonterminal
      and nt' = Nonterminal.lookup (nonterminal ^ "'") in
      table.(k) <- (nt', [| Symbol.N nt |]);
      identifiers.(k) <- [| "_1" |];
      ntprods.(nt') <- (k, k+1);
      positions.(k) <- Nonterminal.positions nt;
        (* A start production has no attributes. *)
      k+1,
      NonterminalMap.add nt k startprods
    ) grammar.start_symbols (0, NonterminalMap.empty)

  let[@inline] is_start prod =
    prod < start

  let start_production nt =
    try
      NonterminalMap.find nt startprods
    with Not_found ->
      assert false (* [nt] is not a start symbol *)

  (* We are now about to enumerate and number the non-start productions. There
     is a twist. If [X.topological_numbering] is [false] then we do so in an
     arbitrary order. If [X.topological_numbering] is [true] and if the
     grammar is acyclic then we number the productions in such a way that if
     [A → α] receives a smaller number than [B → β] then A => B is permitted
     but B => A is impossible. We exploit this property in the GLR parsing
     algorithm. *)

  let foreach_rule : (nonterminal -> rule -> 's -> 's) -> 's -> 's =
    (* An arbitrary ordering. *)
    fun yield s -> StringMap.fold yield grammar.rules s

  let foreach_rule : (nonterminal -> rule -> 's -> 's) -> 's -> 's =
    if X.topological_numbering then
      let module O = TopologicalOrdering.Make(G) in
      match O.nonterminals with
      | None ->
          (* The grammar is cyclic. Abandon silently. *)
          foreach_rule
      | Some nts ->
          (* A topological ordering. *)
          fun yield s ->
            List.fold_left (fun s nt ->
              let rule = StringMap.find nt grammar.rules in
              yield nt rule s
            ) s nts
    else
      foreach_rule

  (* Create the non-start productions, populating the above arrays. *)

  let prod_symbol producer =
    Symbol.lookup producer.prod_symbol

  let (_ : int) = foreach_rule (fun nonterminal rule k ->
    let nt = Nonterminal.lookup nonterminal in
    let k' = MList.accumulate k rule.branches @@ fun k branch ->
      let producers = Array.of_list branch.producers in
      let rhs = Array.map prod_symbol producers in
      table.(k) <- (nt, rhs);
      identifiers.(k) <- Array.map prod_id producers;
      actions.(k) <- Some branch.action;
      rhs_attributes.(k) <- Array.map prod_attributes producers;
      production_level.(k) <- branch.production_level;
      prec_decl.(k) <- branch.prec_annotation;
      positions.(k) <- [ branch.branch_position ];
      attributes.(k) <- branch.br_attributes;
      k+1
    in
    ntprods.(nt) <- (k, k');
    k'
  ) start

  (* Accessors. *)

  let[@inline] def prod =
    table.(prod)

  let[@inline] nt prod =
    let nt, _ = table.(prod) in
    nt

  let[@inline] rhs prod =
    let _, rhs = table.(prod) in
    rhs

  let[@inline] length prod =
    Array.length (rhs prod)

  let[@inline] identifiers prod =
    identifiers.(prod)

  let action prod =
    match actions.(prod) with
    | Some action ->
        action
    | None ->
        (* Start productions have no action. *)
        assert (is_start prod);
        assert false

  let[@inline] positions prod =
    positions.(prod)

  let[@inline] attributes prod =
    attributes.(prod)

  let[@inline] rhs_attributes prod =
    rhs_attributes.(prod)

  let[@inline] prec_decl k =
    prec_decl.(k)

  let[@inline] production_level k =
    production_level.(k)

  let get_start prod =
    assert (is_start prod);
    match (rhs prod).(0) with
    | Symbol.N nt ->
        nt
    | Symbol.T _ ->
        assert false

  let test_start prod =
    if is_start prod then Some (get_start prod) else None

  let error_free prod =
    MArray.for_all Symbol.non_error (rhs prod)

  (* Printing a production. *)

  let print prod =
    assert (not (is_start prod));
    let nt, rhs = table.(prod) in
    let nt = Nonterminal.print false nt in
    if Array.length rhs = 0 then
      (* Avoid producing a trailing space. *)
      Printf.sprintf "%s ->" nt
    else
      Printf.sprintf "%s -> %s" nt
        (Symbol.print_subarray false 0 rhs)

  let describe gerund prod =
    match test_start prod with
    | Some nt ->
        let ending = if gerund then "ing" else "" in
        Printf.sprintf "accept%s %s" ending (Nonterminal.print false nt)
    | None ->
        let ending = if gerund then "ing" else "e" in
        Printf.sprintf "reduc%s production %s" ending (print prod)

  (* Iteration. *)

  let[@inline] iterx f =
    MInt.iterij start n f

  let[@inline] foldx f accu =
    MInt.foldij start n f accu

  let[@inline] mapx f =
    MInt.mapij start n f

  let[@inline] productions nt =
    let k, k' = ntprods.(nt) in
    MInt.interval k k'

  let[@inline] iternt nt f =
    let k, k' = ntprods.(nt) in
    MInt.iterij k k' f

  let[@inline] foldnt nt f accu =
    let k, k' = ntprods.(nt) in
    MInt.foldij k k' f accu

  let[@inline] mapnt nt f =
    let k, k' = ntprods.(nt) in
    MInt.mapij k k' f

end (* Production *)

(* -------------------------------------------------------------------------- *)

(* Maps of productions. *)

module ProductionMap = struct

  include Patricia

  (* Iteration over the start productions only. *)

  let[@inline] start f =
    MInt.foldi Production.start (fun prod m ->
      add prod (f prod) m
    ) empty

end (* ProductionMap *)

(* -------------------------------------------------------------------------- *)

(* Reduction tables. *)

module Reductions = struct

  type t =
    Production.t list TerminalMap.t

  type reverse =
    TerminalSet.t ProductionMap.t

  let reverse (reductions : t) : reverse =
    TerminalMap.fold (fun t prods accu ->
      List.fold_left (fun accu prod ->
        let ts =
          try
            ProductionMap.find prod accu
          with Not_found ->
            TerminalSet.empty
        in
        ProductionMap.add prod (TerminalSet.add t ts) accu
      ) accu prods
    ) reductions ProductionMap.empty

  let add_reduction prod t reductions =
    let prods =
      try
        TerminalMap.find t reductions
      with Not_found ->
        []
    in
    TerminalMap.add t (prod :: prods) reductions

  let add_reductions prod ts reductions =
    TerminalSet.fold (add_reduction prod) ts reductions

  let flip (reductions : reverse) : t =
    ProductionMap.fold add_reductions reductions TerminalMap.empty

  let nonempty ts =
    not (TerminalSet.is_empty ts)
      (* It is a bit expensive to build a set [ts] just to find out
         whether this set is empty, but this will do. *)

  let shift_reduce_conflict_symbols transitions reductions =
    TerminalSet.inter
      (SymbolMap.terminals transitions)
      (TerminalMap.domain reductions)

  let more_than_one _t prods =
    match prods with
    | [] | [_] -> false | _ :: _ :: _ -> true

  let reduce_reduce_conflict_symbols reductions =
    TerminalMap.filter more_than_one reductions
    |> TerminalMap.domain

  let conflict_symbols transitions reductions =
    TerminalSet.union
      (shift_reduce_conflict_symbols transitions reductions)
      (reduce_reduce_conflict_symbols reductions)

  let has_shift_reduce_conflict transitions reductions =
    nonempty (shift_reduce_conflict_symbols transitions reductions)

  let has_reduce_reduce_conflict reductions =
    nonempty (reduce_reduce_conflict_symbols reductions)

  let has_conflict transitions reductions =
    nonempty (conflict_symbols transitions reductions)

  (* If there are two reduction actions on [#], then I believe that this
     should be considered a reduce/reduce conflict, not an end-of-stream
     conflict. Thus, in [has_eos_conflict], I do not check whether there
     exist multiple reduction actions on [#]. *)

  let has_eos_conflict transitions reductions =
    match TerminalMap.find_and_remove Terminal.sharp reductions with
    | exception Not_found ->
        (* There is no reduction action on [#], thus no conflict. *)
        false
    | prods, reductions ->
        assert (prods <> []);
        (* There is at least one reduction action on [#]. There is an
           end-of-stream conflict if and only if either there exists
           another shift or reduce action. *)
        not (TerminalMap.is_empty reductions) ||
        SymbolMap.exists (fun symbol _ -> Symbol.is_terminal symbol)
          transitions

end (* Reductions *)

(* -------------------------------------------------------------------------- *)

(* Items. *)

module Item = struct

  (* An LR(0) item encodes a pair of integers, namely the index of the
     production and the index of the bullet in the production's right-hand
     side. *)

  (* Both integers are packed into a single integer, using 10 bits for
     the bullet position and the rest (21 bits on a 32-bit architecture,
     53 bits on a 64-bit architecture) for the production index. This
     means that the length of a production must be at most 1023. This
     means that the number of productions must be at most:
     - 2^21, that is about 2 million, on a 32-bit architecture;
     - 2^53, that is practically unlimited, on a 64-bit architecture. *)

  (* These static limits could be adjusted if necessary. It would also be
     possible to dynamically adjust the limits depending on the grammar
     at hand. In practice, the need for this has not been felt. *)

  (* WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING *)

  (* The constants [low_bits], [low_limit] and the function [export] are
     duplicated in [lib/InspectionTableInterpreter.ml] and in
     [front/CheckGrammar.ml]. Do not modify them at all, or modify them
     everywhere in a consistent manner. *)

  type t = int

  let low_bits =
    10 (* have you read the above warning? *)

  let low_limit =
    1 lsl low_bits

  let equal =
    Int.equal

  let[@inline] import (prod, pos) =
    assert (0 <= pos && pos <= Production.length prod);
    assert (pos < low_limit);
    (Production.encode prod) lsl low_bits + pos

  let[@inline] export item =
    Production.decode (item lsr low_bits),
    item mod low_limit

  let[@inline] marshal item =
    item

  let is_start item =
    let prod, pos = export item in
    Production.is_start prod && pos = 0

  let test_start item =
    let prod, _pos = export item in
    Production.test_start prod

  let get_start item =
    let prod, _pos = export item in
    Production.get_start prod

  let print item =
    let prod, pos = export item in
    let nt = Production.nt prod in
    let rhs = Production.rhs prod in
    Printf.sprintf "%s -> %s"
      (Nonterminal.print false nt)
      (Symbol.print_array_bullet false pos rhs)

  type kind =
    | Shift of Symbol.t * t
    | Reduce of Production.t

  let classify item =
    let prod, pos = export item in
    let rhs = Production.rhs prod in
    let n = Array.length rhs in
    if pos = n then
      Reduce prod
    else
      Shift (rhs.(pos), import (prod, pos + 1))

  (* Sets of items and maps over items. Hashing these data structures is
     specifically allowed, so balanced trees (for instance) would not be
     applicable here. *)

  (* The main reason why [Set] is implemented using Patricia trees, as
     opposed to bit sets, is that, given our current encoding of items,
     items are not small consecutive integers. *)

  module Map =
    IntMap.Unbounded

  module Set =
    Map.Domain

end (* Item *)

(* -------------------------------------------------------------------------- *)

(* Operations on states. *)

(* These functions are parametric in the type of the lookahead sets,
   so they can be applied to a symbolic state or to a concrete state,
   for example *)

module AbstractState = struct

  type 'lookahead t =
    'lookahead Item.Map.t

  (* [transitions state] computes the non-epsilon transitions that leave the
     state [state]. Here, a state is a set of items. *)

  let transitions (state : 'a Item.Map.t) : 'a Item.Map.t SymbolMap.t =
    Item.Map.fold (fun item ts transitions ->
      match Item.classify item with
      | Item.Shift (symbol, item') ->
          let items : 'a Item.Map.t =
            try
              SymbolMap.find symbol transitions
            with Not_found ->
              Item.Map.empty
          in
          SymbolMap.add symbol (Item.Map.add item' ts items) transitions
      | Item.Reduce _ ->
          transitions
    ) state SymbolMap.empty

  (* -------------------------------------------------------------------------- *)

  (* [reverse_reductions state] computes the reduction opportunities at the
     (closed) state [state]. The result is a reverse reduction table, that is,
     a map of productions to lookahead sets. *)

  let reverse_reductions (state : 'a Item.Map.t) : 'a ProductionMap.t =
    Item.Map.fold (fun item ts accu ->
      match Item.classify item with
      | Item.Reduce prod ->
          assert (not (ProductionMap.mem prod accu));
          ProductionMap.add prod ts accu
      | Item.Shift _ ->
          accu
    ) state ProductionMap.empty

end (* AbstractState *)

(* -------------------------------------------------------------------------- *)

(* Concrete syntax trees. *)

module CST = struct

  type cst =
    | CstTerminal of Terminal.t
    | CstNonTerminal of Production.t * cst array
    | CstError
    | CstDisj of Nonterminal.t * cst list

  let unNT (cst : cst) : Nonterminal.t =
    match cst with
    | CstNonTerminal (prod, _) ->
        Production.nt prod
    | CstDisj (nt, _) ->
        nt
    | CstTerminal _
    | CstError ->
        assert false

  let asDisj (cst : cst) : cst list =
    match cst with
    | CstNonTerminal _ ->
        [cst]
    | CstDisj (_, disjuncts) ->
        disjuncts
    | CstTerminal _
    | CstError ->
        assert false

  let notDisj (cst : cst) : bool =
    match cst with CstDisj _ -> false | _ -> true

  let disj nt cst1 cst2 =
    assert (unNT cst1 = nt);
    assert (unNT cst2 = nt);
    let disjuncts1 = asDisj cst1 in
    assert (notDisj cst2);
    CstDisj (nt, cst2 :: disjuncts1)

  (* This is a pretty-printer for concrete syntax trees. *)

  (* A leaf is denoted by a terminal symbol. *)

  (* A node is denoted by a bracketed, whitespace-separated list,
     whose head is a non-terminal symbol (followed with a colon)
     and whose tail consists of the node's descendants. *)

  (* There is in fact some ambiguity in this notation, since we
     only print the non-terminal symbol that forms the left-hand
     side of production [prod], instead of the production itself.

     This abuse makes things much more readable, and should be
     acceptable for the moment. The cases where ambiguity actually
     arises should be rare. *)

  (* An error leaf is denoted by [error]. *)

  open PPrint

  let rec build : cst -> document = function
    | CstTerminal tok ->
        string (Terminal.print tok)
    | CstNonTerminal (prod, csts) ->
        brackets (
          group (
            string (Nonterminal.print false (Production.nt prod)) ^^
            colon ^^
            group (
              nest 2 (
                Array.fold_left (fun doc cst ->
                  doc ^/^ build cst
                ) empty csts
              )
            ) ^^
            break 0
          )
        )
    | CstError ->
        string "error"
    | CstDisj (_nt, csts) ->
        braces (group (nest 2 (
          concat_map (fun cst -> build cst ^^ break 0) csts
        )))

  let show f cst =
    ToChannel.pretty 0.8 80 f (build cst)

end (* CST *)

(* -------------------------------------------------------------------------- *)

(* Properties of the grammar. *)

let attributes =
  grammar.gr_attributes

let grammar_uses_error_token () =
  MRef.with_state false @@ fun used ->
  Production.iterx @@ fun prod ->
  if not (Production.error_free prod) then
    used := true

(* -------------------------------------------------------------------------- *)

(* Many analyses of the grammar are expressed as fixed point computations.
   We exploit the generic analysis in [GenericAnalysis], which itself relies
   on the fixed point algorithm in [Fix]. *)

(* -------------------------------------------------------------------------- *)

(* Compute which nonterminal symbols are nonempty, that is, which symbols
   generate a nonempty language. Also, compute which nonterminal symbols are
   nullable. The two computations are almost identical. The only difference is
   in the base case: a single terminal symbol is not nullable, but is
   nonempty. *)

module G = struct
  type nonterminal = Nonterminal.t
  type terminal = Terminal.t
  type symbol = Symbol.t
  type production = Production.t
  let classify symbol =
    match symbol with Symbol.N nt -> `N nt | Symbol.T t -> `T t
  let rhs = Production.rhs
  let productions = Production.productions
  let ignore _prod = false
end

module G_WithoutErrorProductions = struct
  include G
  (* The productions that contain [error] are ignored. *)
  let ignore prod = not (Production.error_free prod)
end

module NONEMPTY =
  GenericAnalysis.Make
    (Fix.Glue.ArraysAsImperativeMaps(Nonterminal))
    (Fix.Prop.Boolean)
    (G)
    (Nonempty.A)

module NULLABLE =
  GenericAnalysis.Make
    (Fix.Glue.ArraysAsImperativeMaps(Nonterminal))
    (Fix.Prop.Boolean)
    (G)
    (Nullable.A)

(* -------------------------------------------------------------------------- *)

(* Compute FIRST sets. *)

module FIRST =
  GenericAnalysis.Make
    (Fix.Glue.ArraysAsImperativeMaps(Nonterminal))
    (TerminalSet)
    (G)
    (struct
      let shortcut _nt = None
      (* A terminal symbol has a singleton FIRST set. *)
      let terminal = TerminalSet.singleton
      (* The FIRST set of an alternative is the union of the FIRST sets. *)
      let disjunction p q = TerminalSet.union p (q())
      (* The FIRST set of a sequence is the union of:
         the FIRST set of the first member, and
         the FIRST set of the second member, if the first member is nullable. *)
      let conjunction symbol p q =
        if NULLABLE.symbol symbol then
          TerminalSet.union p (q())
        else
          p
      (* The FIRST set of the empty sequence is empty. *)
      let epsilon = TerminalSet.empty
    end)

(* -------------------------------------------------------------------------- *)

(* For every nonterminal symbol [nt], compute a word of minimal length
   generated by [nt]. *)

(* In principle, this analysis subsumes [NONEMPTY] and [NULLABLE]. Indeed,
   [nt] produces a nonempty language if only if the minimal length is finite;
   [nt] is nullable if only if the minimal length is zero. Be careful, though:
   [MINIMAL] analyzes a grammar where error productions are ignored. *)

(* This analysis is in principle more costly than [NONEMPTY] and [NULLABLE].
   In practice, it seems to be very cheap: its cost is not measurable for any
   of the grammars in our benchmark suite. *)

(* This analysis could be made cheaper by computing just the length of a
   minimal sequence, as opposed to a minimal sequence. Currently the sequence
   itself is printed at verbosity level 2 but is otherwise unused. *)

module MINIMAL =
  GenericAnalysis.Make
    (Fix.Glue.ArraysAsImperativeMaps(Nonterminal))
    (struct
      include CompletedNatWitness
      type property = Terminal.t t
    end)
    (G_WithoutErrorProductions)
    (struct
      let shortcut _nt = None
      open CompletedNatWitness
      (* A terminal symbol has length 1. *)
      let terminal = singleton
      (* The length of an alternative is the minimum length of any branch. *)
      let disjunction = min_lazy
      (* The length of a sequence is the sum of the lengths of the members. *)
      let conjunction _ = add_lazy
      (* The epsilon sequence has length 0. *)
      let epsilon = epsilon
    end)

(* -------------------------------------------------------------------------- *)

(* Determine which symbols generate a nonempty word, that is, a word of length
   at least 1. We might tempted to deduce this information from the FIRST sets
   (indeed, a symbol generates a nonempty word if and only if its FIRST set is
   nonempty), but this does not work because (here) we want to ignore error
   productions. *)

(* This information is needed later on by the analysis MAXIMAL. *)

module GENERATES_NONEMPTY_WORD =
  GenericAnalysis.Make
    (Fix.Glue.ArraysAsImperativeMaps(Nonterminal))
    (Fix.Prop.Boolean)
    (G_WithoutErrorProductions)
    (struct
      let shortcut _nt = None
      (* A terminal symbol generates a nonempty word. *)
      let terminal _t = true
      (* An alternative generates a nonempty word if at least one branch
         generates a nonempty word. *)
      let disjunction p q = p || q()
      (* A sequence generates a nonempty word if at least one side generates
         a nonempty word. (We assume both languages are nonempty.) *)
      let conjunction _ p q = p || q()
      (* [epsilon] fails to generate a nonempty word. *)
      let epsilon = false
    end)

let generates_nonempty_word symbol : bool =
  GENERATES_NONEMPTY_WORD.symbol symbol

(* -------------------------------------------------------------------------- *)

(* Compute which symbols can produce words of unbounded length. *)

(* This information is needed later on by the analysis MAXIMAL. *)

(* To do so, we construct the reference graph of the grammar. The vertices
   of this graph are the nonterminal symbols, and, if there is a production
   [A -> alpha B gamma], then there is an edge from A to B. With each such
   edge, we associate a Boolean label, which is [true] if [alpha . gamma]
   generates a nonempty word. *)

module Graph = struct
  type node = Nonterminal.t
  let n = Nonterminal.n
  let index nt = nt
  let iter = Nonterminal.iter
  let labeled_successors yield nt =
    Production.iternt nt @@ fun prod ->
    let rhs = Production.rhs prod in
    rhs |> Array.iteri @@ fun i symbol ->
    match symbol with
    | Symbol.T _   -> ()
    | Symbol.N nt' ->
        (* There is an edge from [nt] to [nt'], whose Boolean label [gnw]
           is obtained by computing whether the right-hand side, deprived
           of [nt'], can generate a nonempty word. *)
        let gnw =
          rhs |> MArray.existsi @@ fun j symbol ->
          i <> j && generates_nonempty_word symbol
        in
        yield gnw nt'
  let successors yield nt =
    labeled_successors (fun _gnw nt' -> yield nt') nt
end

(* Then, we compute the strongly connected components of the reference
   graph. If a component contains an edge labeled [true], then every
   nonterminal symbol in this component can generate words of unbounded
   length. Mark these symbols in an array [unbounded]. (This computation is
   performed only when needed.) *)

let unbounded : bool array Lazy.t =
  lazy (
    let unbounded = Array.make Nonterminal.n false in
    let module T = Fix.SCC.Run(Graph) in
    let () =
      (* For each edge of [nt] to [nt'] labeled [gnw], *)
      Graph.iter @@ fun nt ->
      nt |> Graph.labeled_successors @@ fun gnw nt' ->
      (* If [gnw] is set and if [nt] and [nt'] lie in the same component, *)
      if gnw && T.representative nt = T.representative nt' then
        (* Then mark every symbol in this component as unbounded. *)
        T.scc (T.representative nt) |> List.iter @@ fun nt ->
        unbounded.(nt) <- true
    in
    unbounded
  )

let unbounded nt : bool =
  (Lazy.force unbounded).(nt)

(* -------------------------------------------------------------------------- *)

(* For every nonterminal symbol [nt], we wish to compute the maximum length of
   a word generated by [nt]. This length can be either finite or [infty]. *)

(* In this analysis, we assume that every symbol generates a nonempty
   language. This removes the need to watch out for productions whose right
   hand side is empty (which we would have to ignore). If this assumption is
   violated then MAXIMAL can produce an over-estimation.

   A similar assumption is made in the computation of FIRST sets above. *)

(* To find out the maximum length of a word generated by each nonterminal
   symbol [nt], we perform a least fixed point computation in the lattice
   [NatMaxInfinity]. Such a computation normally would not terminate, as the
   lattice has unbounded height: a cycle of strictly positive weight in the
   grammar will cause an endless climb. However, we have just identified the
   symbols that participate in these cycles: they are the symbols [nt] such
   that [unbounded nt] is [true]. We use the [shortcut] function to set
   these symbols directly to [infinity]. The cycles that may remain in the
   grammar must have zero weight and cannot cause divergence. The fixed
   point computation must therefore terminate and yield the desired
   information. *)

module MAXIMAL =
  GenericAnalysis.Make
    (Fix.Glue.ArraysAsImperativeMaps(Nonterminal))
    (NatInfinityMax)
    (G_WithoutErrorProductions)
    (struct
      open NatInfinityMax
      let shortcut nt = if unbounded nt then Some infinity else None
      (* A terminal symbol has length 1. *)
      let terminal _t = finite 1
      (* The length of an alternative is the maximum length of any branch. *)
      let disjunction = max_lazy
      (* The length of a sequence is the sum of the lengths of the members. *)
      let conjunction _ = add_lazy
      (* The epsilon sequence has length 0. *)
      let epsilon = bottom
    end)

(* -------------------------------------------------------------------------- *)

(* Computing FOLLOW or symbolic FOLLOW sets is mostly unnecessary for us,
   but a user has requested this analysis. Also, FOLLOW sets happen to be
   useful in the SLR(1) test. *)

(* FOLLOW sets and symbolic FOLLOW sets are computed exactly in the same
   way. To avoid code duplication, we write this computation just once,
   parameterized over a module [P]. The type [P.property] intuitively
   represents a set of symbols. *)

module FOLLOW (P : sig
  include Fix.MINIMAL_SEMI_LATTICE
  val bottom: property
  val terminal: Terminal.t -> property
  val first: Production.t -> int -> property
end) = struct

  module M =
    Fix.Glue.ArraysAsImperativeMaps(Nonterminal)

  module C =
    LatticeInequalities.Make(M)(P)()

  (* Build a system of constraints. *)

  (* Iterate over all start symbols. *)
  let () =
    let sharp = P.terminal Terminal.sharp in
    Nonterminal.iter_internal_start @@ fun nt ->
    (* Add # to FOLLOW(nt). *)
    C.source sharp nt
    (* We need to do this explicitly because our start productions are
       of the form S' -> S, not S' -> S #, so # will not automatically
       appear into FOLLOW(S) when the start productions are examined. *)

  (* Iterate over all productions. *)
  let () =
    Production.iter @@ fun prod ->
    let nt1, rhs = Production.def prod in
    (* Iterate over all nonterminal symbols [nt2] in the right-hand side. *)
    rhs |> Array.iteri @@ fun i symbol ->
    match symbol with
    | Symbol.T _ ->
        ()
    | Symbol.N nt2 ->
        let nullable = NULLABLE.production_suffix prod (i+1)
        and first = P.first prod (i+1) in
        (* The FIRST set of the remainder of the right-hand side
           contributes to the FOLLOW set of [nt2]. *)
        C.source first nt2;
        (* If the remainder of the right-hand side is nullable,
           FOLLOW(nt1) contributes to FOLLOW(nt2). *)
        if nullable then
          C.edge nt1 nt2

  (* Second pass. Solve the equations. *)

  module S =
    C.Solve()

  let follow (nt : Nonterminal.t) : P.property =
    Option.value (S.solution nt) ~default:P.bottom

end (* FOLLOW *)

(* -------------------------------------------------------------------------- *)

(* Compute standard (concrete) FOLLOW sets. *)

let follow : (Nonterminal.t -> TerminalSet.t) Lazy.t =
  lazy (
    let module F = FOLLOW(struct
      include TerminalSet
      let terminal = singleton
      let first = FIRST.production_suffix
    end) in
    F.follow
  )

let follow (nt : Nonterminal.t) : TerminalSet.t =
  (Lazy.force follow) nt

(* -------------------------------------------------------------------------- *)

(* Compute FOLLOW sets for the terminal symbols as well. Again, this is
   unnecessary for us, but has been requested by a user. This is done in
   a single pass over the grammar -- a new fixed point computation is not
   required. *)

let tfollow : TerminalSet.t array Lazy.t =
  lazy (

    let tfollow = Array.make Terminal.n TerminalSet.empty in

    (* Iterate over all productions. *)
    let () =
      Production.iter @@ fun prod ->
      let nt1, rhs = Production.def prod in
      (* Iterate over all terminal symbols [t2] in the right-hand side. *)
      rhs |> Array.iteri @@ fun i symbol ->
      match symbol with
      | Symbol.N _ ->
          ()
      | Symbol.T t2 ->
          let nullable = NULLABLE.production_suffix prod (i+1)
          and first = FIRST.production_suffix prod (i+1) in
          (* The FIRST set of the remainder of the right-hand side
             contributes to the FOLLOW set of [t2]. *)
          tfollow.(t2) <- TerminalSet.union first tfollow.(t2);
          (* If the remainder of the right-hand side is nullable,
             FOLLOW(nt1) contributes to FOLLOW(t2). *)
          if nullable then
            tfollow.(t2) <- TerminalSet.union (follow nt1) tfollow.(t2)
    in
    tfollow

  )

let tfollow t =
  (Lazy.force tfollow).(t)

(* -------------------------------------------------------------------------- *)

(* Compute symbolic FIRST and symbolic FOLLOW sets. *)

(* The symbolic FIRST set of the word determined by [prod/i] is defined
   (and computed) as follows. *)

let sfirst prod i =
  let rhs = Production.rhs prod in
  let n = Array.length rhs in
  let rec loop i =
    if i = n then
      (* If the word [prod/i] is empty, the set is empty. *)
      SymbolSet.empty
    else
      let sym = rhs.(i) in
      (* If the word [prod/i] begins with a symbol [sym], then [sym]
         itself is part of the symbolic FIRST set, unconditionally. *)
      SymbolSet.union
        (SymbolSet.singleton sym)
        (* Furthermore, if [sym] is nullable, then the symbolic
           FIRST set of the sub-word [prod/i+1] contributes, too. *)
        (if NULLABLE.symbol sym then loop (i + 1) else SymbolSet.empty)
  in
  loop i

(* The symbolic FOLLOW sets are computed just like the FOLLOW sets, except
   we use a symbolic FIRST set instead of a standard FIRST set. *)

let sfollow : (Nonterminal.t -> SymbolSet.t) Lazy.t =
  lazy (
    let module F = FOLLOW(struct
      let bottom = SymbolSet.bottom
      include Fix.Glue.MinimalSemiLattice(SymbolSet)
      let terminal t = SymbolSet.singleton (Symbol.T t)
      let first = sfirst
    end) in
    F.follow
  )

let sfollow (nt : Nonterminal.t) : SymbolSet.t =
  (Lazy.force sfollow) nt

(* -------------------------------------------------------------------------- *)

(* Provide explanations about FIRST sets. *)

(* We want to explain why a terminal symbol [t] appears in the FIRST set of a
   sequence of symbols. Such an explanation involves basic assertions of the
   form: (i) [nt] is nullable and (ii) [t] appears in the FIRST set of [nt].
   We choose to take these basic facts for granted, instead of recursively
   explaining them, so as to keep explanations short. *)

(* We first produce an explanation in abstract syntax, then convert it to a
   human-readable string. *)

type explanation =
  | EObvious
    (**The sequence begins with the desired terminal symbol. *)
  | EFirst of Terminal.t * Nonterminal.t
    (**The sequence begins with a nonterminal symbol
       that produces the desired terminal symbol. *)
  | ENullable of Symbol.t list * explanation
    (**The sequence begins with a list of nullable symbols and ... *)

let explain (t : Terminal.t) (rhs : Symbol.t array) (i : int) =
  let n = Array.length rhs in
  let rec loop i =
    assert (i < n);
    let symbol = rhs.(i) in
    match symbol with
    | Symbol.T t' ->
        assert (Terminal.equal t t');
        EObvious
    | Symbol.N nt ->
        if TerminalSet.mem t (FIRST.nonterminal nt) then
          EFirst (t, nt)
        else begin
          assert (NULLABLE.nonterminal nt);
          match loop (i + 1) with
          | ENullable (symbols, e) ->
              ENullable (symbol :: symbols, e)
          | e ->
              ENullable ([ symbol ], e)
        end
  in
  loop i

let rec convert (e : explanation) : string =
  match e with
  | EObvious ->
      ""
  | EFirst (t, nt) ->
      Printf.sprintf "%s can begin with %s"
        (Nonterminal.print false nt)
        (Terminal.print t)
  | ENullable (symbols, e) ->
      let e = convert e in
      Printf.sprintf "%s can vanish%s%s"
        (Symbol.print_list false symbols)
        (if e = "" then "" else " and ")
        e

(* -------------------------------------------------------------------------- *)
(* Package the analysis results. *)

module Analysis = struct

  let nonempty = NONEMPTY.nonterminal

  let nullable = NULLABLE.nonterminal

  let nullable_symbol = NULLABLE.symbol

  let first = FIRST.nonterminal

  let first_symbol = FIRST.symbol

  (* An initial definition of [nullable_first]. *)

  let nullable_first prod i =
    NULLABLE.production_suffix prod i,
    FIRST.production_suffix prod i

  (* A memoised version of [nullable_first]. We want to avoid recomputing
     along a production's right-hand side. *)

  let nullable_first =
    Production.tabulate @@ fun prod ->
    MArray.tabulate (Production.length prod + 1) @@ fun i ->
    nullable_first prod i

  let explain_first t prod i =
    convert (explain t (Production.rhs prod) i)

  let follow = follow
  let tfollow = tfollow
  let sfollow = sfollow

  let minimal' nt =
    MINIMAL.nonterminal nt

  let minimal nt =
    CompletedNatWitness.to_int (MINIMAL.nonterminal nt)

  let minimal_prod prod i =
    assert (0 <= i && i <= Production.length prod);
    CompletedNatWitness.to_int (MINIMAL.production_suffix prod i)

  let maximal nt =
    NatInfinityMax.to_int (MAXIMAL.nonterminal nt)

  let maximal_prod prod i =
    assert (0 <= i && i <= Production.length prod);
    NatInfinityMax.to_int (MAXIMAL.production_suffix prod i)

end (* Analysis *)

(* -------------------------------------------------------------------------- *)

(* %on_error_reduce declarations. *)

module OnErrorReduce = struct

  (* We keep a [StringMap] internally, and convert back and forth between
     the types [Nonterminal.t] and [string] when querying this map. This
     is not very elegant, and could be changed if desired. *)

  let declarations : on_error_reduce_level StringMap.t =
    grammar.on_error_reduce

  let print (nt : Nonterminal.t) : string =
    Nonterminal.print false nt

  let lookup (nt : string) : Nonterminal.t =
    try
      Nonterminal.lookup nt
    with Not_found ->
      (* If this fails, then we have an [%on_error_reduce] declaration
         for an invalid symbol. *)
      assert false

  let reduce prod =
    let nt = Production.nt prod in
    StringMap.mem (print nt) declarations

  let iter f =
    declarations |> StringMap.iter @@ fun nt _prec ->
    f (lookup nt)

  let preferable prod1 prod2 =
    (* The two productions that we are comparing must be distinct. *)
    assert (prod1 <> prod2);
    let nt1 = Production.nt prod1
    and nt2 = Production.nt prod2 in
    (* If they have the same left-hand side (which seems rather unlikely?),
       declare them incomparable. *)
    nt1 <> nt2 &&
    (* Otherwise, look up the priority levels associated with their left-hand
       symbols. *)
    let prec1, prec2 =
      try
        StringMap.find (print nt1) declarations,
        StringMap.find (print nt2) declarations
      with Not_found ->
        (* [preferable] should be used to compare two symbols for which
           there exist [%on_error_reduce] declarations. *)
        assert false
    in
    match production_order prec1 prec2 with
    | Gt ->
        (* [prec1] is a higher integer than [prec2], therefore comes later
           in the file. By analogy with [%left] and friends, we give higher
           priority to later declarations. *)
        true
    | Lt ->
        false
    | Eq
    | Incomparable ->
        (* We could issue a warning or an information message in these cases. *)
        false

end (* OnErrorReduce *)

(* -------------------------------------------------------------------------- *)

(* Facilities for reading and printing sentences. *)

module Sentence = struct

  type sentence =
    Nonterminal.t option * Terminal.t list

  open Validate

  let validate_nonterminal_symbol c raw_symbol =
    let (symbol, startpos, endpos) = raw_symbol in
    match Nonterminal.lookup symbol with
    | exception Not_found ->
        Report.signal c [Range.make (startpos, endpos)]
          "\"%s\" is not a known non-terminal symbol." symbol;
        raise Invalid
    | nt ->
        nt

  let validate_start_symbol c raw_symbol =
    let nt = validate_nonterminal_symbol c raw_symbol in
    if Nonterminal.is_user_start nt then
      nt
    else
      let (symbol, startpos, endpos) = raw_symbol in
      Report.signal c [Range.make (startpos, endpos)]
        "\"%s\" is not a start symbol." symbol;
      raise Invalid

  let validate_terminal_symbol c raw_symbol =
    let (symbol, startpos, endpos) = raw_symbol in
    try
      Terminal.lookup symbol
    with Not_found ->
      Report.signal c [Range.make (startpos, endpos)]
        "\"%s\" is not a known terminal symbol." symbol;
      raise Invalid

  (* This first version of [validate_sentence] checks that the start symbol,
     if present, is indeed a valid start symbol. It does not require a start
     symbol to be present. *)

  let validate_sentence c raw_sentence : sentence =
    let (nto, terminals) = raw_sentence in
    Option.map (validate_start_symbol c) nto,
    List.map (validate_terminal_symbol c) terminals

  (* This second version of [validate_sentence] requires the start symbol to
     be present unless the grammar has only one start symbol. *)

  let validate_nto c raw_sentence nto =
    match nto with
    | Some nt ->
        nt
    | None ->
        if Nonterminal.start > 1 then
          Report.error c [RawSentence.range raw_sentence]
            "because the grammar has multiple start symbols, each of the\n\
             sentences provided on the standard input channel must be of the\n\
             form: <start symbol>: <token>*"
        else
          List.hd Nonterminal.user_start_symbols

  let validate_sentence c raw_sentence : sentence =
    let sentence = validate_sentence c raw_sentence in
    let nto, terminals = sentence in
    let nt = validate_nto c raw_sentence nto in
    Some nt, terminals

  let start (nto, _terminals) =
    Option.get nto

  open Printf

  let print_abstract (nto, terminals) : string =
    MString.with_buffer 128 @@ fun b ->
    Option.iter (fun nt ->
      bprintf b "%s: " (Nonterminal.print false nt)
    ) nto;
    let separator = MString.this_then_that "" " " in
    List.iter (fun t ->
      bprintf b "%s%s" (separator()) (Terminal.print t)
    ) terminals;
    bprintf b "\n"

  let print_concrete (_nto, terminals) : string =
    MString.with_buffer 128 @@ fun b ->
    let separator = MString.this_then_that "" " " in
    terminals |> List.iter @@ fun t ->
    bprintf b "%s%s" (separator()) (Option.get (Terminal.unquoted_alias t))

  let print style sentence =
    match style with
    | `Abstract ->
        print_abstract sentence
    | `Concrete ->
        print_concrete sentence

end (* Sentence *)

(* -------------------------------------------------------------------------- *)

(* Information messages. *)

let info c =
  Report.log c
    "Grammar has %d nonterminal symbols, among which %d start symbols."
    (Nonterminal.n - Production.start) Production.start;
  Report.log c
    "Grammar has %d terminal symbols."
    (Terminal.n - 2);
  Report.log c
    "Grammar has %d productions."
    (Production.n - Production.start)

(* -------------------------------------------------------------------------- *)

(* Errors and warnings. *)

(* If a start symbol generates the empty language or generates the language
   {epsilon}, report an error. In principle, this could be just a warning.
   However, in [Engine], in the function [start], it is convenient to assume
   that neither of these situations can arise. This means that at least one
   token must be read. *)

let check_start_symbol c =
  Nonterminal.user_start_symbols |> List.iter @@ fun nt ->
  if not (Analysis.nonempty nt) then
    Report.signal c (Nonterminal.positions nt)
      "%s generates the empty language."
      (Nonterminal.print false nt);
  if TerminalSet.is_empty (Analysis.first nt) then
    Report.signal c (Nonterminal.positions nt)
      "%s generates the language {epsilon}."
      (Nonterminal.print false nt)

(* If a nonterminal symbol generates the empty language, issue a warning. *)

let warn_empty_symbol c =
  Nonterminal.iterx @@ fun nt ->
  if not (Analysis.nonempty nt) then
    Report.warning c (Nonterminal.positions nt)
      "%s generates the empty language."
      (Nonterminal.print false nt)

(* -------------------------------------------------------------------------- *)

(* Dumping the results of the analyses. *)

let dump_nullable c =
  if Report.live c then
    Nonterminal.iterx @@ fun nt ->
    Report.log c "nullable(%s) = %b"
      (Nonterminal.print false nt)
      (Analysis.nullable nt)

let dump_first c =
  if Report.live c then
    Nonterminal.iterx @@ fun nt ->
    Report.log c "first(%s) = %s"
      (Nonterminal.print false nt)
      (TerminalSet.print (Analysis.first nt))

let dump_minimal c =
  if Report.live c then
    Nonterminal.iterx @@ fun nt ->
    Report.log c "minimal(%s) = %s"
      (Nonterminal.print false nt)
      (CompletedNatWitness.print Terminal.print (Analysis.minimal' nt))

let dump_maximal c =
  if Report.live c then
    Nonterminal.iterx @@ fun nt ->
    let p = Analysis.maximal nt in
    Report.log c "maximal(%s) = %s"
      (Nonterminal.print false nt)
      (if p = max_int then "infinity" else string_of_int p)

let dump_follow c =
  if Report.live c then
    Nonterminal.iterx @@ fun nt ->
    Report.log c "follow(%s) = %s"
      (Nonterminal.print false nt)
      (TerminalSet.print (Analysis.follow nt))

let dump_tfollow c =
  if Report.live c then
    Terminal.iter @@ fun t ->
    Report.log c "follow(%s) = %s"
      (Terminal.print t)
      (TerminalSet.print (Analysis.tfollow t))

let dump_sfollow c =
  if Report.live c then
    Nonterminal.iterx @@ fun nt ->
    Report.log c "sfollow(%s) = %s"
      (Nonterminal.print false nt)
      (SymbolSet.print false (Analysis.sfollow nt))

(* -------------------------------------------------------------------------- *)

end (* Make *)
