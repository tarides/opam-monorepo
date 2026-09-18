(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

let error, warning = Report.(error, warning)
open Located
open Keyword
type sw = subject * where
open Attribute
open PlainSyntax
open PlainSyntaxAccessors

module Run (E : sig
  open Report

  (**A channel for reporting errors and emitting warnings. *)
  val main: channel

  (**The grammar. *)
  val grammar: grammar

end) = struct
open E

(* -------------------------------------------------------------------------- *)

(* Throughout this file, branches (productions) are represented as lists of
   producers. We consider it acceptable to perform operations whose cost is
   linear in the length of a production, even when (with more complicated
   code) it would be possible to eliminate this cost. *)

(* -------------------------------------------------------------------------- *)

(* [names producers] is the set of names of the producers [producers].
   The name of a producer is the OCaml variable that is used to name
   its semantic value. *)

let names producers =
  producers |> List.map prod_id |> StringSet.of_list

(* -------------------------------------------------------------------------- *)

(* [is_inline_symbol nt] determines whether the nonterminal symbol [nt] is
   marked %inline. *)

let is_inline_symbol nt : bool =
  assert (is_nonterminal grammar nt);
  let rule = StringMap.find nt grammar.rules in
  rule.inline_flag

(* [is_inline_producer producer] tests whether [producer] is a nonterminal
   symbol (as opposed to a terminal symbol) and:
   - either the definition of this symbol is marked %inline
   - or this producer is marked %inlined. *)

let is_inline_producer producer =
  let symbol = producer.prod_symbol in
  is_nonterminal grammar symbol &&
  (producer.prod_inlined || is_inline_symbol symbol)

(* -------------------------------------------------------------------------- *)

(* [search p i xs] searches the list [xs] for an element [x] that satisfies
   the predicate [p]. The search begins at index [i] in the list. If it
   succeeds then it returns a pair of the index of [x] in the list [xs] and
   the element [x] itself. *)

let search p i xs : (int * 'a) option =
  xs
  |> MList.index
  |> MList.drop i
  |> List.find_opt (fun (_, x) -> p x)

(* -------------------------------------------------------------------------- *)

(* [find symbol] looks up the definition of [symbol], which must be a valid
   nonterminal symbol, in the grammar [grammar]. *)

let find symbol : rule =
  assert (is_nonterminal grammar symbol);
  StringMap.find symbol grammar.rules

(* -------------------------------------------------------------------------- *)

(* [check_no_prod_attributes] checks that a producer, which is about to be
   inlined away, does not carry any attributes. This ensures that we need not
   worry about propagating producer attributes through inlining. *)

let check_no_prod_attributes producer =
  match producer.prod_attributes with
  | [] ->
      ()
  | attr :: _ ->
      warning main
        [attr.origin]
        "this use of the nonterminal symbol %s is inlined away.\n\
         It should not carry an attribute, as this attribute will be lost."
        producer.prod_symbol

(* [combine_name_attributes] combines the [@name] attributes of the caller and
   callee, as described below. It produces a list of zero or one attribute. *)

type oattr =
  attribute option

let combine_name_attributes (caller : oattr) (callee : oattr) : attributes =
  match caller, callee with
  | Some caller, Some callee ->
      let key = caller.key in
      assert (key = "name");
      let payload = Printf.sprintf "%s_%s" caller.payload callee.payload in
      let origin = caller.origin in (* not great! information is lost *)
      let attr = { key; payload; origin } in
      [attr]
  | Some attr, None ->
      [attr]
  | None, Some attr ->
      [attr]
  | None, None ->
      []

(* [inline_attributes caller callee] computes the attributes of the new branch
   that is obtained by inlining the branch [callee] into the branch [caller]. *)

(* Our general policy is as follows:
   the attributes of the new branch are those of the caller.
   The attributes of the callee (if there are any) are lost.
   This policy could change in the future. *)

(* As an exception to this general policy, [@name] attributes are treated
   in a special way. Our motivation is to preserve the property that no
   two productions carry the same name. To this end, the [@name] attributes
   of the caller and callee are combined, as follows:
   - if both names are present, then they are concatenated,
     with a '_' character as a separator;
   - if only one name is present, then this name is retained. *)

let inline_attributes (caller : branch) (callee : branch) : attributes =
  let caller, callee = caller.br_attributes, callee.br_attributes in
  (* Isolate the [@name] attributes. *)
  let ocaller, caller = extract_attribute "name" caller
  and ocallee, callee = extract_attribute "name" callee in
  (* Combine the [@name] attributes of the caller and callee. *)
  let oname = combine_name_attributes ocaller ocallee in
  (* Apply the default policy to the remaining attributes. *)
  let attrs = (ignore callee; caller) in
  (* Put everything together. *)
  oname @ attrs

(* -------------------------------------------------------------------------- *)

(* 2015/11/18. The interaction of %prec and %inline is not documented.
   It used to be the case that we would disallow marking a production
   both %inline and %prec. Now, we allow it, but we check that (1) it
   is inlined at the last position of the host production and (2) the
   host production does not already have a %prec annotation. *)

(* 2025/11/19. We currently do not perform any checks related to the
   interaction of %prec and %inlined. The syntax %inlined is not
   currently accessible to the end user. %inlined annotations can be
   produced by other transformations inside Menhir. One should check
   that if the definition of N contains a %prec annotation then a use
   of N can be marked %inlined only if it is at the last position of
   a production and this production does not have a %prec annotation. *)

let check_prec_inline caller producer nsuffix callee =
  callee.prec_annotation |> Option.iter @@ fun callee_prec ->
  (* The callee has a %prec annotation. *)
  (* Check condition 1. *)
  if nsuffix > 0 then begin
    let symbol = producer.prod_symbol in
    error main [ position callee_prec; caller.branch_position ]
      "this production carries a %%prec annotation,\n\
       and the nonterminal symbol %s is marked %%inline.\n\
       For this reason, %s can be used only in tail position."
      symbol symbol
  end;
  (* Check condition 2. *)
  caller.prec_annotation |> Option.iter @@ fun caller_prec ->
  let symbol = producer.prod_symbol in
  error main [ position callee_prec; position caller_prec ]
    "this production carries a %%prec annotation,\n\
     and the nonterminal symbol %s is marked %%inline.\n\
     For this reason, %s cannot be used in a production\n\
     which itself carries a %%prec annotation."
    symbol symbol

(* -------------------------------------------------------------------------- *)

(* 2015/11/18. If the callee has a %prec annotation (which implies that the
   caller does not have one, and that the callee appears in tail position in
   the caller) then the annotation is inherited. This seems reasonable, but
   remains undocumented. *)

let propagate_prec_annotation caller callee =
  match callee.prec_annotation with
  | (Some _) as annotation ->
      assert (caller.prec_annotation = None);
      annotation
  | None ->
      caller.prec_annotation

(* -------------------------------------------------------------------------- *)

(* [new_candidate x] is a candidate fresh name, which is based on [x] in an
   unspecified way. A fairly arbitrary construction can be used here; we just
   need it to produce an infinite sequence of names, so that eventually we are
   certain to be able to escape any finite set of unavailable names. We also
   need this construction to produce reasonably concise names, as it can be
   iterated several times in practice; I have observed up to 9 iterations in
   real-world grammars. *)

(* Here, the idea is to add a suffix of the form _inlined['0'-'9']+ to the
   name [x], if it does not already include such a suffix. If [x] already
   carries such a suffix, then we increment the integer number. *)

let new_candidate x =
  let x, n = Lex.chop (Lexing.from_string x) in
  Printf.sprintf "%s_inlined%d" x (n + 1)

(* [fresh names x] returns a fresh name that is not in the set [names].
   The new name is obtained by iterating [new_candidate] until we fall
   outside the set [names]. *)

let rec fresh names x =
  if StringSet.mem x names then fresh names (new_candidate x) else x

(* -------------------------------------------------------------------------- *)

(* [rename used producers] renames the producers [producers] of the inlined
   branch (the callee) if necessary to avoid a clash with the set [used] of
   the names used by the producers of the host branch (the caller). This set
   need not contain the name of the producer that is inlined away. *)

(* This function produces a pair of: 1. a substitution [phi], which represents
   the renaming that we have performed, and which must be applied to the
   semantic action of the callee; 2. the renamed [producers]. *)

let rename (used : StringSet.t) producers: Action.subst * producers =
  let phi, _used, producers =
    List.fold_left (fun (phi, used, producers) producer ->
      let id = producer.prod_id  in
      let x = value id in
      if StringSet.mem x used then
        let x' = fresh used x in
        let id' = Located.map (fun _x -> x') id in
        Action.extend x x' phi,
        StringSet.add x' used,
        { producer with prod_id = id' } :: producers
      else
        (phi, StringSet.add x used, producer :: producers)
    ) (Action.empty, used, []) producers
  in
  phi, List.rev producers

(* -------------------------------------------------------------------------- *)

(* [define_positions] defines how the start and end positions of the callee
   should be computed once it is inlined into the caller. This information is
   used to transform [$startpos] and [$endpos] in the callee and to transform
   [$startpos(x)] and [$endpos(x)] in the caller. *)

(* 2015/11/04. We ensure that positions are computed in the same manner,
   regardless of whether inlining is performed. *)

(* The arguments of this function are as follows:

   [name]       an array of the names of the producers of the new branch
   [nprefix]    the length of the prefix of the caller, up to the inlining site
   [ncallee]    the length of the callee

   The results are as follows:

   [startp]     how to transform $startpos in the callee
   [endp]       how to transform $endpos in the callee
   [beforeendp] how to transform $endpos($0) in the callee

 *)

let define_positions (name : string array) nprefix ncallee : sw * sw * sw =

  let startp =
    if ncallee > 0 then
      (* If the inner production is non-epsilon, things are easy. The start
         position of the inner production is the start position of its first
         element. *)
      RightNamed name.(nprefix), WhereStart
    else if nprefix > 0 then
      (* If the inner production is epsilon, we are supposed to compute the
         end position of whatever comes in front of it. If the prefix is
         nonempty, then this is the end position of the last symbol in the
         prefix. *)
      RightNamed (name.(nprefix - 1)), WhereEnd
    else
      (* If the inner production is epsilon and the prefix is empty, then
         we need to look up the end position stored in the top stack cell.
         This is the reason why we need the keyword [$endpos($0)]. It is
         required in this case to preserve the semantics of $startpos and
         $endpos. *)
      Before, WhereEnd

    (* Note that, contrary to intuition perhaps, we do NOT have that
       if the prefix is empty, then the start position of the inner
       production is the start production of the outer production.
       This is true only if the inner production is non-epsilon. *)

  in

  let endp =
    if ncallee > 0 then
      (* If the inner production is non-epsilon, things are easy: its end
         position is the end position of its last element. *)
      RightNamed (name.(nprefix + ncallee - 1)), WhereEnd
    else
      (* If the inner production is epsilon, then its end position is equal
         to its start position. *)
      startp

  (* We must also transform [$endpos($0)] if it is used by the inner
     production. It refers to the end position of the stack cell
     that comes before the inner production. So, if the prefix is
     non-empty, then it translates to the end position of the last
     element of the prefix. Otherwise, it translates to [$endpos($0)]. *)

  and beforeendp =
    if nprefix > 0 then
      RightNamed (name.(nprefix - 1)), WhereEnd
    else
      Before, WhereEnd

  in
  startp, endp, beforeendp

(* -------------------------------------------------------------------------- *)

(* [rename_sw_outer] transforms the keywords in the outer production (the
   caller) during inlining. It replaces [$startpos(x)] and [$endpos(x)], where
   [x] is the name of the callee, with [startpx] and [endpx], respectively. *)

let rename_sw_outer (x, startpx, endpx) (sw : sw) : sw option =
  match sw with
  | Before, _ ->
      None
  | RightNamed x', where ->
      if x' = x then
        match where with
        | WhereStart -> Some startpx
        | WhereEnd   -> Some endpx
        | WhereSymbolStart -> assert false (* has been expanded away *)
      else
        None
  | Left, _ ->
      (* [$startpos], [$endpos], and [$symbolstartpos] have been expanded away
         earlier; see [KeywordExpansion]. *)
      assert false

(* -------------------------------------------------------------------------- *)

(* [rename_sw_inner] transforms the keywords in the inner production (the callee)
   during inlining. It replaces [$endpos($0)] with [beforeendp]. *)

let rename_sw_inner beforeendp (sw : sw) : sw option =
  match sw with
  | Before, where ->
      assert (where = WhereEnd);
      Some beforeendp
  | RightNamed _, _ ->
      None
  | Left, _ ->
      (* [$startpos] and [$endpos] have been expanded away earlier; see
         [KeywordExpansion]. *)
      assert false

(* -------------------------------------------------------------------------- *)

(* [inline_branch caller site callee] inlines the branch [callee] into the
   branch [caller] at the site [site]. By convention, a site is a pair of an
   integer index -- the index [i] of the producer that must be inlined away --
   and a producer [producer] -- the producer itself. This is redundant, as
   [producer] can be recovered based on [caller] and [i], but convenient. *)

type site =
  int * producer

let inline_branch caller (i, producer : site) (callee : branch) : branch =

  (* The host branch (the caller) is divided into three sections: a prefix
     of length [nprefix], the producer that we wish to inline away, and a
     suffix of length [nsuffix]. *)

  (* Compute the length of the prefix and suffix. *)

  let nprefix = i in
  let nsuffix = List.length caller.producers - (i + 1) in

  (* Construct the prefix and suffix. *)

  let prefix = MList.take nprefix caller.producers
  and suffix = MList.drop (nprefix + 1) caller.producers in

  (* Apply the (undocumented) restrictions that concern the interaction
     between %prec and %inline. Then, (possibly) propagate a %prec
     annotation. *)
  check_prec_inline caller producer nsuffix callee;
  let prec_annotation = propagate_prec_annotation caller callee in

  (* Compute the names of the producers in the host branch (the caller), minus
     the one that is being inlined away. Rename the producers of the inlined
     branch (the callee), if necessary, so as to avoid a clash with this set.
     The goal is to guarantee that, after inlining, all producers in the newly
     constructed branch have unique names. *)
  let used = StringSet.union (names prefix) (names suffix) in
  let phi, inlined_producers = rename used callee.producers in

  (* Construct (the producers of) the new branch. The prefix and suffix of the
     caller are preserved. In the middle, [producer] disappears and is replaced
     with [inlined_producers]. For debugging purposes, check that each producer
     in the new branch carries a unique name. *)
  let producers = prefix @ inlined_producers @ suffix in
  let (_ : StringSet.t) = names producers in

  (* Find out how the start and end positions of the callee should be computed
     once it is inlined into the caller. *)

  let startp, endp, beforeendp =
    let name = producers |> Array.of_list |> Array.map prod_id in
    let ncallee = List.length callee.producers in
    define_positions name nprefix ncallee
  in

  (* Apply appropriate renamings to the semantic actions of the caller and
     callee, then compose them using a [let] binding. If [x] is the name of
     the producer that we wish to inline away, then the variable [x] in the
     caller's semantic action should refer to the semantic value produced by
     the callee's semantic action. *)

  let x = prod_id producer in
  let caller_action, callee_action =
    Action.rename (rename_sw_outer (x, startp, endp)) Action.empty caller.action,
    Action.rename (rename_sw_inner beforeendp) phi callee.action
  in
  let action = Action.compose x callee_action caller_action in

  (* The position and production level of the new branch are those
     of the caller. *)

  let { branch_position; production_level; _ } = caller in
  let br_attributes = inline_attributes caller callee in

  (* We are done! Build a new branch. *)

  {
    branch_position;
    producers;
    action;
    prec_annotation;
    production_level;
    br_attributes;
  }

(* -------------------------------------------------------------------------- *)

(* Inlining a list of branches [callees] into the branch [caller] at [site]. *)

let inline_branches caller site (callees : branches) : branches =
  List.map (inline_branch caller site) callees

(* -------------------------------------------------------------------------- *)

(* The transformation of the grammar [grammar] begins here. *)

(* Roughly speaking, the transformation is implemented by two mutually
   recursive functions. [expand_branches] transforms a list of branches into a
   list of (expanded) branches; [expand_symbol] maps a nonterminal symbol
   (which may or may not be marked %inline) to its definition in the
   transformed grammar, an (expanded) rule. In order to avoid duplication of
   work, we memoize [expand_symbol]. Thus, the expansion of each symbol is
   computed at most once. (Expansions are demanded top-down, but are computed
   bottom-up.) Memoization is implemented without pain by using a ready-made
   fixed point combinator, [Memoize.defensive_fix]. Furthermore, this fixed
   point combinator dynamically detects cycles of %inline nonterminal symbols,
   allowing us to avoid divergence and display a nice error message. *)

(* This is [expand_branches], parameterized by its companion function,
   [expand_symbol]. The parameter [i], an integer, is used to perform
   a left-to-right sweep: the precondition of [expand_branches] is that
   there are no inlining sites at indices less than [i] in [branches].
   Thus, we can begin searching at index [i]. (Beginning to search at
   index 0 would work, too, but would cause redundant searches.) *)

let rec expand_branches expand_symbol i branches : branches =
  (* For each branch [caller] in the list [branches], *)
  MList.bind branches @@ fun (caller : branch) ->
    (* Search for an inlining site in the branch [caller]. We begin the
       search at position [i], as we know that every inlining site left
       of this position has been dealt with already. *)
    match search is_inline_producer i caller.producers with
    | None ->
        (* There is none; we are done. *)
        MList.return caller
    | Some ((i, producer) as site) ->
        (* There is one. This is an occurrence of a nonterminal symbol
           [symbol] that is marked %inline. We look up its (expanded)
           definition (via a recursive call to [expand_symbol]), yielding
           a set of branches, which we inline into the branch [caller].
           Then, we continue looking for inlining sites. *)
        check_no_prod_attributes producer;
        let symbol = producer.prod_symbol in
        let rule = expand_symbol symbol in
        rule.branches
        |> inline_branches caller site
        |> expand_branches expand_symbol i

(* This is [expand_symbol], parameterized by itself. *)

let expand_symbol expand_symbol symbol : rule =
  (* Find the rule that defines this symbol. Then, transform this rule
     by applying [expand_branches] to its branches. The left-to-right
     sweep begins at index 0. *)
  let rule = find symbol in
  { rule with branches = expand_branches expand_symbol 0 rule.branches }

(* Apply [defensive_fix] to obtain a closed function [expand_symbol]. *)

let expand_symbol : symbol -> rule =
  Fix.Memoize.String.defensive_fix expand_symbol

(* Wrap [expand_symbol] in an exception handler, so that, when a cycle
   of %inline nonterminal symbols is detected, a good error message is
   displayed. *)

let expand_symbol symbol =
  try
    expand_symbol symbol
  with Fix.Memoize.String.Cycle (symbols, symbol) ->
    let rule = find symbol in
    let b = Buffer.create 128 in
    Printf.bprintf b "there is a cycle of %%inline nonterminal symbols:\n";
    begin match symbols with
    | [] ->
        assert false
    | head :: [] ->
        assert (head = symbol);
        Printf.bprintf b "  %s refers to itself." symbol
    | head :: next :: symbols ->
        assert (head = symbol);
        Printf.bprintf b "  %s refers to %s,\n" head next;
        List.iter (fun symbol ->
          Printf.bprintf b "  which refers to %s,\n" symbol
        ) symbols;
        Printf.bprintf b "  which refers back to %s." symbol
    end;
    error main rule.positions "%s" (Buffer.contents b)

(* Display a warning about %on_error_reduce declarations that concern %inline
   symbols. They are useless. *)

let () =
  grammar.on_error_reduce |> StringMap.iter @@ fun nt _level ->
  if is_inline_symbol nt then
    warning main []
      "the declaration %%on_error_reduce %s\n\
       has no effect: this symbol is marked %%inline and is expanded away." nt

(* The rules of the transformed grammar are obtained by expanding all rules. *)

(* To save some time, we filter out the rules that concern %inline symbols.
   This is not necessary, because they would be removed by [trim] anyway. *)

let rules =
  grammar.rules
  |> StringMap.filter (fun _ rule -> not rule.inline_flag)
  |> StringMap.mapi (fun symbol _rule -> expand_symbol symbol)

(* In this transformed grammar, remove any unreachable symbols, and remove the
   %type declarations and %on_error_reduce declarations that mention them. *)

(* An explicit removal of the unreachable symbols is necessary (or at least
   convenient) in order to support selective inlining, where some use sites of
   a symbol N are marked %inlined but the symbol N itself is not marked
   %inline. In the case where all use sites are marked %inlined, we want the
   symbol N to disappear; otherwise, we want to keep N. *)

let grammar =
  { grammar with rules }
  |> Trim.trim Report.null (* no warnings *)

end (* Run *)

(* -------------------------------------------------------------------------- *)

(* Re-package [Run] as a function. *)

let inline main grammar =
  let module I = Run(struct
    let main = main
    let grammar = grammar
  end) in
  I.grammar
