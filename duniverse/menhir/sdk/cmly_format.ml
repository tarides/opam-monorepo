(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(**This module defines the data that is stored in .cmly files. *)

(**In short, a .cmly file contains a value of type {!grammar}. *)

(**The type definitions in this module are used in [Cmly_write], which
   writes a .cmly file, and in {!Cmly_read}, which reads a .cmly file.
   They should not be used anywhere else. *)

(**All entities (terminal symbols, nonterminal symbols, and so on) are
   represented as integers. These integers serve as indices into arrays.
   This enables simple and efficient hashing, comparison, indexing, etc. *)

type terminal    = int
type nonterminal = int
type production  = int
type lr0         = int
type lr1         = int

type ocamltype   = string
type ocamlexpr   = string

type range = {
  r_start: Lexing.position;
  r_end: Lexing.position;
}

type 'a located = 'a * range

type attribute = {
  a_label: string;
  a_payload: string;
  a_position: range;
}

type attributes =
  attribute list

type terminal_def = {
  t_name: string;
  t_kind: [`REGULAR | `ERROR | `EOF | `PSEUDO];
  t_type: ocamltype option;
  t_attributes: attributes;
}

type nonterminal_def = {
  n_name: string;
  n_kind: [`REGULAR | `START];
  n_mangled_name: string;
  n_type: ocamltype option;
  n_positions: range list;
  n_nullable: bool;
  n_first: terminal list;
  n_attributes: attributes;
}

type symbol =
  | T of terminal
  | N of nonterminal

type identifier = string

type action = {
  a_expr: ocamlexpr;
  a_keywords: Keyword.keyword list;
}

type 'symbol producer_def =
  'symbol * identifier * attributes

type production_def = {
  p_kind: [`REGULAR | `START];
  p_lhs: nonterminal;
  p_rhs: symbol producer_def array;
  p_positions: range list;
  p_action: action option;
  p_attributes: attributes;
    (* Before 2023/08/02, these were the attributes of the left-hand
       side of the production. Now, these are the attributes of the
       production itself. *)
}

type lr0_state_def = {
  lr0_incoming: symbol option;
  lr0_items: (production * int) list;
}

(**Detailed information on a state of the LR(1) automaton. *)
type lr1_state_def = {

  lr1_lr0: lr0;
  (**The LR(0) state that underlies this state. *)

  lr1_transitions: (symbol * lr1) list;
  (**The outgoing transitions of this state. Each outgoing transition
     is represented as a pair of a label (a symbol) and a target state.

     For each terminal symbol [t], there is at most one transition.

     In deterministic mode (without --GLR), all shift/reduce conflicts are
     eliminated, so, for each terminal symbol [t], there cannot be both a
     transition and a reduction, and there is at most one reduction. *)

  lr1_reductions: (terminal * production) list;
  (**The reduction actions at this state. Each reduction action is
     represented as a pair of a terminal symbol and a production.

     In deterministic mode (without --GLR), all shift/reduce conflicts are
     eliminated, so, for each terminal symbol [t], there cannot be both a
     transition and a reduction, and there is at most one reduction. *)

  lr1_default_reduction: production option;
  (**This field is set to [Some prod] if this state reduces production [prod]
     as a default reduction. Regardless of the value of this field, the above
     fields are populated normally. *)

}

(* The surface syntax of a grammar, as found in the front-end.

   A single set of type definitions is used to represent several variants of
   the grammar, namely:

   - the "higher" grammar,
     before parameterized nonterminal symbols are expanded;

   - the "ground" grammar,
     after parameterized nonterminal symbols have been expanded,
     but before %inline nonterminal symbols are eliminated.

   The type parameter ['symbol] designates the symbols that appear in the
   right-hand side of a production. In the higher grammar, it is [parameter];
   in the ground grammar, it is [surface_symbol], that is, just a string.

   The type parameter ['param] designates the parameters of a production.
   In the higher grammar, it is [surface_symbol list], because a production
   can be parameterized; it the ground grammar, it is [unit]. *)

type surface_symbol = string

type token_associativity =
  | LeftAssoc
  | RightAssoc
  | NonAssoc
  | UndefinedAssoc

type priority_level = {
  pl_input_file: string;
  pl_level: int;
}

type 'symbol surface_branch = {
  br_position         : range;
  br_producers        : 'symbol producer_def list;
  br_action           : action;
  br_prec_annotation  : surface_symbol located option;
  br_production_level : priority_level;
  br_attributes       : attributes;
}

type ('param, 'symbol) surface_rule = {
  r_parameters : 'param;
  r_branches   : 'symbol surface_branch list;
  r_inline     : bool;
  r_positions  : range list;
  r_public     : bool;
  r_attributes : attributes;
}

type surface_token = {
  tk_ocamltype     : ocamltype option;
  tk_position      : range;
  tk_alias         : string option;
  tk_attributes    : attributes;
  tk_associativity : token_associativity;
  tk_precedence    : priority_level located option;
  tk_is_declared   : bool;
}

type ('param, 'symbol) surface_syntax = {
  s_types  : ('symbol * ocamltype) list;
  s_tokens : (string * surface_token) list;
  s_rules  : (string * ('param, 'symbol) surface_rule) list;
}

type parameter =
  | ParameterVar of surface_symbol located
  | ParameterApp of surface_symbol located * parameter list
  | ParameterAnonymous of (parameter surface_branch) list located

type higher_syntax = (surface_symbol list, parameter) surface_syntax

type ground_syntax = (unit, surface_symbol) surface_syntax

(* Record packing all information about a grammar *)
type grammar = {
  g_basename         : string;
  g_preludes         : string list;
  g_postludes        : string list;
  g_terminals        : terminal_def    array;
  g_nonterminals     : nonterminal_def array;
  g_productions      : production_def  array;
  g_lr0_states       : lr0_state_def   array;
  g_lr1_states       : lr1_state_def   array;
  g_start_symbols    : string list;
  g_entry_points     : (nonterminal * production * lr1) list;
  g_attributes       : attributes;
  g_parameters       : string list;
  g_on_error_reduce  : (string * priority_level) list;
  g_before_expansion : higher_syntax;
  g_before_inlining  : ground_syntax;
}
