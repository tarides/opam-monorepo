/******************************************************************************/
/*                                                                            */
/*                                    Menhir                                  */
/*                                                                            */
/*   Copyright Inria. All rights reserved. This file is distributed under     */
/*   the terms of the GNU General Public License version 2, as described in   */
/*   the file LICENSE.                                                        */
/*                                                                            */
/******************************************************************************/

/* This is the "crude" (stage 1) version of the parser. It is meant to be
   processed by ocamlyacc. Its existence is necessary for bootstrapping. It is
   kept in sync with the "fancy" (stage 2) parser, with a few differences:
   0. the crude parser produces dummy position information;
   1. the fancy parser exploits many features of Menhir;
   2. the fancy parser performs slightly more refined error handling;
   3. the fancy parser supports anonymous rules;
   4. the fancy parser supports the new rule syntax;
   5. the fancy parser supports %merge. */

%{

open Syntax
open Range
open Located

%}

%token TOKEN TYPE LEFT RIGHT NONASSOC START PREC PUBLIC COLON BAR EOF EQUAL
%token INLINE LPAREN RPAREN COMMA QUESTION STAR PLUS PARAMETER ON_ERROR_REDUCE
%token PERCENTATTRIBUTE SEMI MERGE
%token <string Located.located> LID UID QID
%token <string Located.located> HEADER
%token <string Located.located> ANGLED
%token <string Located.located Lazy.t> PERCENTPERCENT
%token <Syntax.raw_action> ACTION
%token <Attribute.attribute> ATTRIBUTE GRAMMARATTRIBUTE
/* For the new rule syntax: */
%token LET TILDE UNDERSCORE COLONEQUAL EQUALEQUAL
%start grammar
%type <ParserAux.early_producer> producer
%type <ParserAux.early_production> production
%type <Syntax.partial_grammar> grammar

/* These declarations solve a shift-reduce conflict in favor of
   shifting: when the declaration of a non-terminal symbol begins with
   a leading bar, it is understood as an (insignificant) leading
   optional bar, *not* as an empty right-hand side followed by a bar.
   This ambiguity arises due to the existence of a new notation for
   letting several productions share a single semantic action. */

%nonassoc no_optional_bar
%nonassoc BAR

%%

/* ------------------------------------------------------------------------- */
/* A grammar consists of declarations and rules, followed by an optional
   postlude, which we do not parse. */

grammar:
  declarations PERCENTPERCENT rules postlude
    {
      {
        pg_filename          = ""; (* filled in by the caller *)
        pg_declarations      = List.rev $1;
        pg_rules             = $3;
        pg_postlude          = $4
      }
    }

postlude:
  EOF
    { None }
| PERCENTPERCENT /* followed by actual postlude */
    { Some (Lazy.force $1) }

/* ------------------------------------------------------------------------- */
/* A declaration is an %{ OCaml header %}, or a %token, %start,
   %type, %left, %right, or %nonassoc declaration. */

declarations:
  /* epsilon */
    { [] }
| declarations declaration
    { $2 @ $1 }
| declarations SEMI
    { $1 }

declaration:
| HEADER /* lexically delimited by %{ ... %} */
    { [ locate dummy (DCode $1) ] }

| TOKEN optional_ocamltype terminals
    { let ty, ts = $2, $3 in
      List.map (Located.map (fun (terminal, alias, attrs) ->
        DToken (ty, terminal, alias, attrs)
      )) ts }

| START nonterminals
    { List.map (Located.map (fun nonterminal -> DStart nonterminal)) $2 }

| TYPE ocamltype actuals
    { List.map (Located.map (fun nt -> DType ($2, nt)))
        (List.map Parameter.locate $3) }

| START ocamltype nonterminals
    /* %start <ocamltype> foo is syntactic sugar for %start foo %type <ocamltype> foo */
    { let dstart nt = DStart nt
      and dtype ntloc = Located.map (fun _nt -> DType ($2, ParamVar ntloc)) ntloc in (* ugly/weird *)
      List.map (Located.map dstart) $3 @
      List.map dtype $3 }

| priority_keyword symbols
    { let prec = ParserAux.new_precedence_level (rhs_start_pos 1, rhs_end_pos 1) in
      List.map (Located.map (fun symbol -> DTokenProperties (symbol, $1, prec))) $2 }

| PARAMETER ANGLED
    { [ locate dummy (DParameter $2) ] }

| GRAMMARATTRIBUTE
    { [ locate dummy (DGrammarAttribute $1) ] }

| PERCENTATTRIBUTE actuals attributes
    { [ locate dummy (DSymbolAttributes ($2, $3)) ] }

| ON_ERROR_REDUCE actuals
    { let prec = ParserAux.new_on_error_reduce_level() in
      List.map (Located.map (fun nt -> DOnErrorReduce (nt, prec)))
        (List.map Parameter.locate $2) }

ocamltype:
  ANGLED
    { OCamlType.Declared $1 }

optional_ocamltype:
  /* epsilon */
    { None }
| ocamltype /* lexically delimited by angle brackets */
    { Some $1 }

priority_keyword:
  LEFT
    { LeftAssoc }
| RIGHT
    { RightAssoc }
| NONASSOC
    { NonAssoc }

/* ------------------------------------------------------------------------- */
/* A symbol is a terminal or nonterminal symbol. */

/* One would like to require nonterminal symbols to begin with a lowercase
   letter, so as to lexically distinguish them from terminal symbols, which
   must begin with an uppercase letter. However, for compatibility with
   ocamlyacc, this is impossible. It can be required only for nonterminal
   symbols that are also start symbols. */

/* We also accept token aliases in place of ordinary terminal symbols.
   Token aliases are quoted strings. */

symbols:
  /* epsilon */
    { [] }
| symbols optional_comma symbol
    { $3 :: $1 }

symbol:
  LID
    { $1 }
| UID
    { $1 }
| QID
    { $1 }

optional_comma:
  /* epsilon */
    { () }
| COMMA
    { () }

attributes:
  /* epsilon */
    { [] }
| ATTRIBUTE attributes { $1 :: $2 }

/* ------------------------------------------------------------------------- */
/* Terminals must begin with an uppercase letter. Nonterminals that are
   declared to be start symbols must begin with a lowercase letter. */

terminals:
  /* epsilon */
    { [] }
| terminals optional_comma UID optional_alias attributes
    { let ts, uid, alias, attrs = $1, $3, $4, $5 in
      let alias = Option.map Located.value alias in
      Located.map (fun uid -> uid, alias, attrs) uid :: ts }

nonterminals:
  /* epsilon */
    { [] }
| nonterminals LID
    { $2 :: $1 }

optional_alias:
  /* epsilon */
    { None }
| QID
    { Some $1 }

/* ------------------------------------------------------------------------- */
/* A rule defines a symbol. It is optionally declared %public, and optionally
   carries a number of formal parameters. The right-hand side of the definition
   consists of a list of production groups. */

rules:
  /* epsilon */
    { [] }
| rules rule
    { $2 :: $1 }
| rules SEMI
    { $1 }

rule:
  flags
  symbol
  attributes
  optional_formal_parameters
  COLON
  optional_bar
  production_group production_groups
    {
      let public, inline = $1 in
      { pr_public = public;
        pr_inline = inline;
        pr_nt          = Located.value $2;
        pr_positions   = [ Located.position $2 ];
        pr_attributes  = $3;
        pr_parameters  = $4;
        pr_branches    = List.flatten ($7 :: List.rev $8);
        pr_merge       = None; (* %merge not supported *)
      }
    }

flags:
  /* epsilon */
    { false, false }
| PUBLIC
    { true, false }
| INLINE
    { false, true }
| PUBLIC INLINE
    { true, true }
| INLINE PUBLIC
    { true, true }

/* ------------------------------------------------------------------------- */
/* Parameter are surroundered with parentheses and delimited by commas.
   The syntax of actual parameters allows applications, whereas the syntax
   of formal parameters does not. It also allows use of the "?", "+", and
   "*" shortcuts. */

optional_formal_parameters:
  /* epsilon */
    { [] }
| LPAREN formal_parameters RPAREN
    { $2 }

formal_parameters:
  symbol
    { [ Located.value $1 ] }
| symbol COMMA formal_parameters
    { Located.value $1 :: $3 }

optional_actuals:
  /* epsilon */
    { [] }
| LPAREN actuals_comma RPAREN
    { $2 }

actuals_comma:
  actual
    { [ $1 ] }
| actual COMMA actuals_comma
    { $1 :: $3 }

actual:
  symbol optional_actuals
    { Parameter.apply $1 $2 }
| actual modifier
    { Parameter.apply $2 [$1] }

actuals:
  /* epsilon */
    { [] }
| actuals optional_comma actual
    { $1 @ [$3] }

optional_bar:
  /* epsilon */ %prec no_optional_bar
    { () }
| BAR
    { () }

/* ------------------------------------------------------------------------- */
/* The "?", "+", and "*" modifiers are short-hands for applications of
   certain parameterized nonterminals, defined in the standard library. */

modifier:
  QUESTION
    { locate dummy "option" }
| PLUS
    { locate dummy "nonempty_list" }
| STAR
    { locate dummy "list" }

/* ------------------------------------------------------------------------- */
/* A production group is a set of productions that share a semantic action.

   Thus a production group is a list of productions,
   followed by a semantic action,
   followed by an optional precedence specification,
   followed by a possibly empty list of attributes. */

production_groups:
  /* epsilon */
    { [] }
| production_groups BAR production_group
    { $3 :: $1 }

production_group:
  productions
  ACTION /* action is lexically delimited by braces */
  optional_precedence
  attributes
    {
      let productions, action, oprec2, attrs = $1, $2, $3, $4 in
      (* If multiple productions share a single semantic action, check
         that all of them bind the same names. *)
      ParserAux.check_production_group productions;
      (* Then, *)
      List.map (fun (producers, oprec1, level, pos) ->
        (* Replace [$i] with [_i]. *)
        let pb_producers = ParserAux.normalize_producers producers in
        (* Distribute the semantic action and attributes onto every production.
           Also, check that every [$i] is within bounds. *)
        let names = ParserAux.producer_names producers in
        let pb_action = action !ParserAux.dollars names in
        {
          pb_producers;
          pb_action;
          pb_prec_annotation  = ParserAux.override pos oprec1 oprec2;
          pb_production_level = level;
          pb_position         = pos;
          pb_attributes       = attrs;
        })
      productions
    }

optional_precedence:
  /* epsilon */
    { None }
| PREC symbol
    { Some $2 }

/* ------------------------------------------------------------------------- */
/* A production is a list of producers, optionally followed by a
   precedence declaration. Lists of productions are nonempty and
   separated with bars. */

productions:
  production
    { [ $1 ] }
| production bar_productions
    { $1 :: $2 }

bar_productions:
  BAR production
    { [ $2 ] }
| BAR production bar_productions
    { $2 :: $3 }

production:
  producers optional_precedence
    { List.rev $1,
      $2,
      ParserAux.new_production_level(),
      Range.make (symbol_start_pos(), symbol_end_pos())
    }

producers:
  /* epsilon */
    { [] }
| producers producer
    { $2 :: $1 }

/* ------------------------------------------------------------------------- */
/* A producer is an actual parameter, possibly preceded by a
   binding, and possibly followed with attributes. */

producer:
|           actual attributes optional_semis
    { Range.make (symbol_start_pos(), symbol_end_pos()),    None, $1, $2 }
| LID EQUAL actual attributes optional_semis
    { Range.make (symbol_start_pos(), symbol_end_pos()), Some $1, $3, $4 }

/* ------------------------------------------------------------------------- */
/* Semicolons used to be considered whitespace by our lexer, but are no longer.
   We must allow optional semicolons in a few conventional places. */

optional_semis:
  /* empty */         { () }
| optional_semis SEMI { () }

%%
