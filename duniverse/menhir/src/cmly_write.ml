(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Attribute
open Syntax
open PlainSyntax
open Grammar
let value, position, map = Located.(value, position, map)
let n2i = Nonterminal.encode
let t2i = Terminal.encode
let p2i = Production.encode
module C = Cmly_format

let ocamltype (typ : ocamltype) : C.ocamltype =
  match typ with
  | FrontTypes.Declared fragment ->
      value fragment
  | FrontTypes.Inferred typ ->
      typ

let ocamltype_option (typo : ocamltype option) : C.ocamltype option =
  match typo with
  | None ->
      None
  | Some typ ->
      Some (ocamltype typ)

let range (pos : range) : C.range =
  {
    C.r_start = Range.startp pos;
    C.r_end   = Range.endp pos;
  }

let located l = (value l, range (position l))

let ranges =
  List.map range

let attribute (attr : attribute) : C.attribute =
  {
    C.a_label    = attr.key;
    C.a_payload  = attr.payload;
    C.a_position = range attr.origin;
  }

let attributes : Attribute.attributes -> C.attributes =
  List.map attribute

let terminal (t : Terminal.t) : C.terminal_def =
  {
    C.t_kind = (
      if Terminal.equal t Terminal.error then
        `ERROR
      else if
        (match Terminal.eof with
         | None -> false
         | Some eof -> Terminal.equal t eof) then
        `EOF
      else if Terminal.special t then
        `PSEUDO
      else
        `REGULAR
    );
    C.t_name = Terminal.print t;
    C.t_type = ocamltype_option (Terminal.ocamltype t);
    C.t_attributes = attributes (Terminal.attributes t);
  }

let nonterminal (nt : Nonterminal.t) : C.nonterminal_def =
  let is_start = Nonterminal.is_internal_start nt in
  {
    C.n_kind = if is_start then `START else `REGULAR;
    C.n_name = Nonterminal.print false nt;
    C.n_mangled_name = Nonterminal.print true nt;
    C.n_type = if is_start then None else ocamltype_option (Nonterminal.ocamltype nt);
    C.n_positions = if is_start then [] else ranges (Nonterminal.positions nt);
    C.n_nullable = Analysis.nullable nt;
    C.n_first = List.map t2i (TerminalSet.elements (Analysis.first nt));
    C.n_attributes = if is_start then [] else attributes (Nonterminal.attributes nt);
  }

let symbol (sym : Symbol.t) : C.symbol =
  match sym with
  | Symbol.N n -> C.N (n2i n)
  | Symbol.T t -> C.T (t2i t)

let action (a : Action.t) : C.action =
  {
    C.a_expr = ILPrinter.string_of_expr (Action.expr a);
    C.a_keywords = Keyword.KeywordSet.elements (Action.keywords a);
  }

let rhs (prod : Production.t) : C.symbol C.producer_def array =
  match Production.test_start prod with
  | Some n ->
      [| (C.N (n2i n), "", []) |]
  | None ->
      Array.mapi (fun i sym ->
        let id = (Production.identifiers prod).(i) in
        let attrs = attributes (Production.rhs_attributes prod).(i) in
        symbol sym, id, attrs
      ) (Production.rhs prod)

let production (prod : Production.t) : C.production_def =
  {
    C.p_kind = if Production.is_start prod then `START else `REGULAR;
    C.p_lhs = n2i (Production.nt prod);
    C.p_rhs = rhs prod;
    C.p_positions = ranges (Production.positions prod);
    C.p_action = if Production.is_start prod then None
               else Some (action (Production.action prod));
    C.p_attributes = attributes (Production.attributes prod);
  }

let item (i : Item.t) : C.production * int =
  let p, i = Item.export i in
  (p2i p, i)

let itemset (is : Item.Set.t) : (C.production * int) list =
  List.map item (Item.Set.elements is)

let lr0_state (node : Lr0.node) : C.lr0_state_def =
  {
    C.lr0_incoming = Option.map symbol (Lr0.incoming_symbol node);
    C.lr0_items = itemset (Lr0.items node)
  }

let transition (sym, node) : C.symbol * C.lr1 =
  (symbol sym, Lr1.encode node)

let lr1_state (node : Lr1.node) : C.lr1_state_def =
  {
    C.lr1_lr0 = Lr0.encode (Lr0.ALR1.core (Lr1.state node));
    C.lr1_transitions =
      List.map transition (SymbolMap.bindings (Lr1.transitions node));
    C.lr1_reductions = (
      let add t ps rs = (t2i t, p2i (List.hd ps)) :: rs in
      TerminalMap.fold_rev add (Lr1.reductions node) []
    );
    C.lr1_default_reduction =
      Option.map (fun (p, _ts) -> p2i p) (Lr1.test_default_reduction node);
  }

let entry_point prod node accu : (C.nonterminal * C.production * C.lr1) list =
  let nt = Production.get_start prod in
  (n2i nt, p2i prod, Lr1.encode node) :: accu

let priority_level input level = {
  C.pl_input_file = InputFile.name input;
  C.pl_level = level;
}

let precedence_level pl =
  match pl with
  | UndefinedPrecedence ->
      None
  | PrecedenceLevel il ->
      let input, level = value il in
      Some (priority_level input level, range (position il))

let production_level (ProductionLevel (input, level)) =
  priority_level input level

let associativity = function
  | LeftAssoc      -> C.LeftAssoc
  | RightAssoc     -> C.RightAssoc
  | NonAssoc       -> C.NonAssoc
  | UndefinedAssoc -> C.UndefinedAssoc

let import_bindings f map =
  List.map (fun (k, v) -> (k, f v)) (StringMap.bindings map)

let token (tk : properties) = {
  C.tk_ocamltype = ocamltype_option tk.tk_ocamltype;
  C.tk_position = range tk.tk_position;
  C.tk_alias = tk.tk_alias;
  C.tk_attributes = attributes tk.tk_attributes;
  C.tk_associativity = associativity tk.tk_associativity;
  C.tk_precedence = precedence_level tk.tk_precedence;
  C.tk_is_declared = tk.tk_is_declared;
}

let basic_producer (pr : producer) =
  (pr.prod_symbol,
   value pr.prod_id,
   attributes pr.prod_attributes)

let basic_branch (br : branch) = {
  C.br_position         = range br.branch_position;
  C.br_producers        = List.map basic_producer br.producers;
  C.br_action           = action br.action;
  C.br_prec_annotation  = Option.map located br.prec_annotation;
  C.br_production_level = production_level br.production_level;
  C.br_attributes       = attributes br.br_attributes;
}

let basic_rule (rule : rule) = {
  C.r_parameters = ();
  C.r_branches   = List.map basic_branch rule.branches;
  C.r_inline     = rule.inline_flag;
  C.r_positions  = List.map range rule.positions;
  C.r_public     = false;
  C.r_attributes = attributes rule.attributes;
}

let basic_syntax (g : grammar) : C.ground_syntax = {
  C.s_types  = import_bindings ocamltype g.types;
  C.s_tokens = import_bindings token g.tokens;
  C.s_rules  = import_bindings basic_rule g.rules;
}

let rec higher_branch (br : parameterized_branch) = {
  C.br_position         = range br.pb_position;
  C.br_producers        = List.map higher_producer br.pb_producers;
  C.br_action           = action br.pb_action;
  C.br_prec_annotation  = Option.map located br.pb_prec_annotation;
  C.br_production_level = production_level br.pb_production_level;
  C.br_attributes       = attributes br.pb_attributes;
}

and parameter = function
  | ParamVar sym ->
      C.ParameterVar (located sym)
  | ParamApp (sym, args) ->
      C.ParameterApp (located sym, List.map parameter args)
  | ParamAnonymous brs ->
      C.ParameterAnonymous (located (map (List.map higher_branch) brs))

and higher_producer (id, param, attr) =
  (parameter param, value id, attributes attr)

let higher_rule (r : parameterized_rule) = {
  C.r_parameters = r.pr_parameters;
  C.r_branches   = List.map higher_branch r.pr_branches;
  C.r_inline     = r.pr_inline;
  C.r_positions  = List.map range r.pr_positions;
  C.r_public     = r.pr_public;
  C.r_attributes = attributes r.pr_attributes;
}

let higher_syntax (g : Syntax.grammar) : C.higher_syntax = {
  C.s_types  = (
    let import_type (p,t) = (parameter p, ocamltype (value t)) in
    List.map import_type g.p_types;
  );
  C.s_tokens = import_bindings token g.p_tokens;
  C.s_rules  = import_bindings higher_rule g.p_rules;
}

let encode () : C.grammar =
  {
    C.g_basename     = Settings.base;
    C.g_preludes     = List.map value Front.grammar.preludes;
    C.g_postludes    = List.map value Front.grammar.postludes;
    C.g_terminals    = Terminal.init terminal;
    C.g_nonterminals = Nonterminal.init nonterminal;
    C.g_productions  = Production.init production;
    C.g_lr0_states   = Lr0.init lr0_state;
    C.g_lr1_states   = Lr1.init lr1_state;
    C.g_entry_points = ProductionMap.fold entry_point Lr1.entry [];
    C.g_attributes   = attributes Grammar.attributes;
    C.g_parameters   = List.map value Front.grammar.parameters;
    C.g_start_symbols = StringSet.elements Front.grammar.start_symbols;
    C.g_on_error_reduce =
      List.map (fun (sym, lvl) -> (sym, production_level lvl))
        (StringMap.bindings Front.grammar.on_error_reduce);
    C.g_before_expansion = higher_syntax Front.grammar_before_expansion;
    C.g_before_inlining = basic_syntax Front.grammar_before_inlining;
  }

let write oc (t : C.grammar) =
  (* .cmly file format: CMLY ++ version string ++ grammar *)
  let magic = "CMLY" ^ Version.version in
  output_string oc magic;
  output_value oc (t : C.grammar)

let write filename =
  (* Opening in binary mode is required. This is not a text file;
     we write to it using [output_value]. *)
  let oc = open_out_bin filename in
  write oc (encode());
  close_out oc
