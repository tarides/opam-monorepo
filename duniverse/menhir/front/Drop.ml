(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

let accumulate = MList.accumulate
open Attribute
let value = Located.value
(* The source. *)
module S = Syntax
(* The target. *)
module T = PlainSyntax

(* -------------------------------------------------------------------------- *)

(* Most of the translation is straightforward. *)

let drop_parameter (param : S.parameter) : S.symbol =
  (* The grammar should not have any parameterized symbols. *)
  value (Parameter.head param)

let drop_producer ((id, param, attrs) : S.producer) : T.producer =
  { prod_id            = id;
    prod_symbol        = drop_parameter param;
    prod_attributes    = attrs;
    prod_inlined       = false }

let drop_branch (branch : S.parameterized_branch) : T.branch =
  { branch_position  = branch.pb_position;
    producers        = List.map drop_producer branch.pb_producers;
    action           = branch.pb_action;
    prec_annotation  = branch.pb_prec_annotation;
    production_level = branch.pb_production_level;
    br_attributes    = branch.pb_attributes }

let drop_rule (rule : S.parameterized_rule) : T.rule =
  (* The grammar should not have any parameterized symbols. *)
  assert (rule.pr_parameters = []);
  (* The [%public] flag is dropped. *)
  { branches = List.map drop_branch rule.pr_branches;
    positions = rule.pr_positions;
    inline_flag = rule.pr_inline;
    attributes = rule.pr_attributes;
    merge = Option.map value rule.pr_merge }

(* -------------------------------------------------------------------------- *)

(* We must store [%type] declarations and [%on_error_reduce] declarations
   in maps, whereas so far they were represented as lists. *)

let drop_declarations
  (f : 'info1 -> 'info2) (decls : (S.parameter * 'info1) list)
: 'info2 StringMap.t =
  accumulate StringMap.empty decls @@ fun accu (param, info) ->
  let symbol = value (Parameter.head param) in
  StringMap.add symbol (f info) accu

let drop_type_declarations decls =
  drop_declarations value decls

let drop_on_error_reduce_declarations decls =
  drop_declarations (fun x -> x) decls

(* -------------------------------------------------------------------------- *)

(* We must eliminate (that is, desugar) [%attribute] declarations. We examine
   them one by one and attach these attributes with terminal or nonterminal
   symbols, as appropriate. This is entirely straightforward. *)

let add_attribute (g : T.grammar) param attr : T.grammar =
  let symbol = drop_parameter param in
  match StringMap.find symbol g.tokens with
  | props ->
      (* This is a terminal symbol. *)
      let props = { props with tk_attributes = attr :: props.tk_attributes } in
      { g with tokens = StringMap.add symbol props g.tokens }
  | exception Not_found ->
      match StringMap.find symbol g.rules with
      | rule ->
          (* This is a nonterminal symbol. *)
          let rule = { rule with attributes = attr :: rule.attributes } in
          { g with rules = StringMap.add symbol rule g.rules }
      | exception Not_found ->
          (* This is an unknown symbol. This should not happen. *)
          assert false

let add_attributes g (params, attrs) =
  accumulate g params @@ fun g param ->
  accumulate g attrs @@ fun g attr ->
  add_attribute g param attr

let add_attributes (decls : (S.parameter list * attributes) list) g =
  accumulate g decls add_attributes

(* -------------------------------------------------------------------------- *)

(* Putting it all together. *)

(* The prefix [T.] is needed in OCaml 4.12. *)

let drop (g : S.grammar) : T.grammar =
  {
    T.preludes        = g.p_preludes;
    T.postludes       = g.p_postludes;
    T.parameters      = g.p_parameters;
    T.start_symbols   = StringMap.domain g.p_start_symbols;
    T.types           = drop_type_declarations g.p_types;
    T.tokens          = g.p_tokens;
    T.on_error_reduce = drop_on_error_reduce_declarations g.p_on_error_reduce;
    T.gr_attributes   = g.p_grammar_attributes;
    T.rules           = StringMap.map drop_rule g.p_rules;
    T.default_merge   = g.p_default_merge;
  } |> add_attributes g.p_symbol_attributes
