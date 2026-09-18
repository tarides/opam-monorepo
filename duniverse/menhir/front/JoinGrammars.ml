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
let error, signal, note = Report.(error, signal, note)
open Located
open Syntax

module Make (E : sig
  open Report

  (**A channel for reporting errors. *)
  val main: channel

  (**A channel for information messages. *)
  val info: channel

  (**[can_complain_about symbol] determines whether it is permitted to report
     an error, using [signal], about the symbol [symbol]. This function
     typically returns [true] if queried about a certain symbol for this time,
     and returns [false] if queried again about the same symbol. This lets us
     avoid flooding the user with multiple reports about a single symbol. *)
  val can_complain_about : symbol -> bool

end) = struct
open E

(* -------------------------------------------------------------------------- *)

(* As of 2025/10/15, we use [signal] instead of [error] in many places.
   This lets us report more than one error. *)

(* -------------------------------------------------------------------------- *)

(* [update_properties] updates an existing token-property record with
   the information found in a %token declaration at position [pos].
   This information includes [ocamltype], [t], [alias], and [attrs]. *)

let update_properties properties pos ocamltype t alias attrs =
  (* If the previous definition was a %token declaration (as opposed to
     a %left, %right, or %nonassoc specification), signal an error. *)
  if properties.tk_is_declared && can_complain_about t then
    signal main [properties.tk_position; pos]
      "the token %s has multiple definitions." t;
  (* Update the previous definition. *)
  { properties with
    tk_is_declared = true;
    tk_position    = pos;
    tk_ocamltype   = ocamltype;
    tk_alias       = alias;
    tk_attributes  = attrs }

(* [create_properties] creates a new token-property record based on the
   information found in a %token declaration. *)

let create_properties pos ocamltype t alias attrs =
  ignore t;
  { tk_associativity = UndefinedAssoc;
    tk_precedence    = UndefinedPrecedence;
    tk_is_declared   = true;
    tk_position      = pos;
    tk_ocamltype     = ocamltype;
    tk_alias         = alias;
    tk_attributes    = attrs }

(* [implicit_properties] creates a new token-property record that contains
   no information except a position. The token is considered not declared.
   This is used when a %left, %right or %nonassoc declaration is encountered
   and the token that is named in it has never been encountered before. *)

let implicit_properties pos =
  { tk_associativity = UndefinedAssoc;
    tk_precedence    = UndefinedPrecedence;
    tk_is_declared   = false;
    tk_position      = pos;
    tk_ocamltype     = None;
    tk_alias         = None;
    tk_attributes    = [] }

(* -------------------------------------------------------------------------- *)

let join_default_merge_functions omfl1 mfl2 : merge_fun located =
  match omfl1 with
  | None ->
      mfl2
  | Some mfl1 ->
      error main [ position mfl1; position mfl2 ]
        "at most one default merge function can be provided."

(* -------------------------------------------------------------------------- *)

(* [join_declaration grammar decl] adds the declaration [decl] to the
   grammar [grammar]. *)

let join_declaration (grammar : grammar) decl : grammar =
  let pos = position decl in
  match value decl with

  (* We store preludes (code fragments) in the joined grammar in an
     arbitrary order, yet we preserve the order of two preludes that
     originate in the same source file is preserved. *)

  | DCode code ->
      { grammar with p_preludes = grammar.p_preludes @ [ code ] }

  (* Functor parameters are treated in the same way. *)

  | DParameter fragment ->
      { grammar with p_parameters = grammar.p_parameters @ [ fragment ] }

  (* Token declarations are recorded. Things are made somewhat difficult by
     the fact that %token and %left-%right-%nonassoc declarations are
     independent. *)

  (* If a token carries an alias, it is recorded in the field [tk_alias]. *)

  | DToken (ocamltype, t, alias, attrs) ->
      (* Create or update a token-property record for the token [t]. *)
      let properties =
        match StringMap.find t grammar.p_tokens with
        | exception Not_found ->
            create_properties pos ocamltype t alias attrs
        | properties ->
            update_properties properties pos ocamltype t alias attrs
      in
      (* Store this token-property record. *)
      { grammar with
        p_tokens = StringMap.add t properties grammar.p_tokens }

  (* Start symbols. *)

  | DStart nt ->
      { grammar with
        p_start_symbols = StringMap.add nt pos grammar.p_start_symbols }

  (* Type declarations for nonterminals. *)

  | DType (ocamltype, nt) ->
      { grammar with
        p_types = (nt, locate pos ocamltype) :: grammar.p_types }

  (* Reductions on error for nonterminals. *)

  | DOnErrorReduce (nt, prec) ->
      { grammar with
        p_on_error_reduce = (nt, prec) :: grammar.p_on_error_reduce }

  (* Token associativity and precedence. *)

  | DTokenProperties (t, assoc, prec) ->
      (* Retrieve the property record for this token, creating one if none
         existed (the token is then deemed not declared). *)
      let properties =
        try  StringMap.find t grammar.p_tokens
        with Not_found -> implicit_properties pos
      in
      (* Reject duplicate precedence declarations. *)
      if properties.tk_associativity <> UndefinedAssoc
      && can_complain_about t then
        signal main [ pos; properties.tk_position ]
          "there are multiple precedence declarations for token %s." t;
      (* Record the new declaration. *)
      let tk_associativity = assoc and tk_precedence = prec in
      let properties = { properties with tk_associativity; tk_precedence } in
      { grammar with p_tokens = StringMap.add t properties grammar.p_tokens }

  | DGrammarAttribute attr ->
      { grammar with
        p_grammar_attributes = attr :: grammar.p_grammar_attributes }

  | DSymbolAttributes (actuals, attrs) ->
      { grammar with
        p_symbol_attributes = (actuals, attrs) :: grammar.p_symbol_attributes }

  | DDefaultMergeFunction mfl ->
      let p_default_merge =
        Some (join_default_merge_functions grammar.p_default_merge mfl)
      in
      { grammar with p_default_merge }

(* [join_declarations decls grammar] adds the declarations [decls] to the
   grammar [grammar]. *)

let join_declarations decls grammar =
  List.fold_left join_declaration grammar decls

(* -------------------------------------------------------------------------- *)

(* [join_postlude] adds an optional postlude to a grammar.
   Postludes are stored in an arbitrary order. *)

let join_postlude postlude grammar =
  match postlude with
  | None ->
      grammar
  | Some postlude ->
      { grammar with p_postludes = postlude :: grammar.p_postludes }

(* [join grammar pg] adds the declarations and postludes of the partial
   grammar [pg] to the grammar [grammar]. *)

let join grammar pg =
  grammar
  |> join_declarations pg.pg_declarations
  |> join_postlude pg.pg_postlude

(* -------------------------------------------------------------------------- *)

(* Before allowing renamings of parameters in a parameterized rule, we check
   that every parameterized rule has distinct parameters. (This is new as of
   2025/10/12.) This error is intentionally fatal. This allows us to include
   a sanity check (an assertion) in the function [R.add] below. It also lets
   us produce a better error message when a rule has two parameters with the
   same name. *)

let check_rule (rule : parameterized_rule) =
  MList.foreach_duplicate String.compare rule.pr_parameters @@ fun x ->
    error main rule.pr_positions
      "several parameters of this rule are named \"%s\"." x

let check (grammar : partial_grammar) =
  List.iter check_rule grammar.pg_rules

(* -------------------------------------------------------------------------- *)

(* Joining two partial grammars can require renaming nonterminal symbols.
   Only symbols that are not marked %public can be renamed. We sometimes
   also need to rename the parameters of a parameterized rule. *)

(* The functions [use] and [used] keep track of the names that have been
   used already (and must not be used again). *)

let (use : nonterminal -> nonterminal), (used : nonterminal -> bool) =
  let used = ref StringSet.empty in
  let use nt = used := StringSet.add nt !used; nt
  and used nt = StringSet.mem nt !used in
  use, used

(* The function [fresh] generates a fresh name (which has not been used
   already) and records the fact that this name has been used. Its argument
   is used as a base for the new name. *)

let fresh : string -> nonterminal =
  let c = ref 0 in
  let rec fresh base =
    let nt = base ^ string_of_int (MInt.preincrement c) in
    if used nt then fresh base (* try again *)
    else use nt
  in fresh

(* A renaming [phi] is a finite map of nonterminals to nonterminals. *)

module R : sig

  type renaming
  val empty : renaming
  val is_empty : renaming -> bool
  val add : nonterminal -> nonterminal -> renaming -> renaming
  val apply : renaming -> nonterminal -> nonterminal

  (**[zip nts nts'] builds a renaming of the names [nts] to the names
     [nts']. The two lists must have the same length. *)
  val zip : nonterminal list -> nonterminal list -> renaming

  (**[fresh nts] builds a renaming of the names [nts] to fresh names. *)
  val fresh : nonterminal list -> renaming

  (**[support phi] is the union of the domain and codomain of [phi]. *)
  val support : renaming -> StringSet.t

end = struct
  include StringMap
  type renaming = nonterminal t
  let add nt nt' phi =
    assert (not (mem nt phi));
    add nt nt' phi
  let apply phi nt =
    try find nt phi
    with Not_found -> nt
  let fresh nts =
    accumulate empty nts @@ fun phi nt ->
    add nt (fresh nt) phi
  let zip nts nts' =
    assert (List.length nts = List.length nts');
    List.fold_right2 add nts nts' empty
end

(* -------------------------------------------------------------------------- *)

(* Applying a renaming to various syntactic categories. *)

let rewrite_nonterminal =
  R.apply

let rewrite_parameter phi param =
  Parameter.map (Located.map (rewrite_nonterminal phi)) param

let rewrite_producer phi ((ido, param, attrs) : producer) =
  ido, rewrite_parameter phi param, attrs

let rewrite_producers phi producers =
  List.map (rewrite_producer phi) producers

let rewrite_branch phi branch =
  { branch with pb_producers = rewrite_producers phi branch.pb_producers }

let rewrite_branches phi branches =
  List.map (rewrite_branch phi) branches

let rewrite_branches phi branches =
  if R.is_empty phi then branches else (* fast path *)
  rewrite_branches phi branches

(* [avoid bad rule] renames some of the parameters of the rule [rule] with
   fresh names so as to avoid the bad names in the set [bad]. *)

let avoid bad (rule : parameterized_rule) : parameterized_rule =
  let xs = rule.pr_parameters in
  let captured = List.filter (fun x -> StringSet.mem x bad) xs in
  if captured = [] then rule else (* fast path *)
  let phi = R.fresh captured in
  { rule with
    pr_parameters  = List.map (rewrite_nonterminal phi) xs;
    pr_branches    = rewrite_branches phi rule.pr_branches }

(* Applying a renaming to a rule can force us to rename some of the rule's
   parameters so as to avoid capture. *)

let rewrite_rule phi rule =
  let rule = avoid (R.support phi) rule in
  { rule with
    pr_nt = rewrite_nonterminal phi rule.pr_nt;
    pr_branches = rewrite_branches phi rule.pr_branches }

let rewrite_rules phi rules =
  List.map (rewrite_rule phi) rules

let rewrite_grammar phi grammar =
  (* We assume that [phi] affects only private symbols, so it does not affect
     the start symbols, which are automatically considered public. *)
  if R.is_empty phi then grammar else (* fast path *)
  { grammar with pg_rules = rewrite_rules phi grammar.pg_rules }

(* -------------------------------------------------------------------------- *)

(* [valid c] determines whether the character [c] can appear in the name
   of a nonterminal symbol. *)

let valid c =
  match c with
  | 'A' .. 'Z'
  | 'a' .. 'z'
  | '_'
  | '\192' .. '\214'
  | '\216' .. '\246'
  | '\248' .. '\255'
  | '0' .. '9' ->
      true
  | _ ->
      false

(* [sanitize filename] removes the extension of the file name [filename]
   and replaces invalid characters with an underscore character. *)

let sanitize filename =
  filename
  |> Filename.remove_extension
  |> String.map (fun c -> if valid c then c else '_')

(* To rename a nonterminal symbol, our first attempt is to prefix it with
   (a sanitized version of) its filename. If this does not work then we
   use [fresh]. *)

let rename nt filename =
  let nt' = sanitize filename ^ "_" ^ nt in
  if not (used nt') then use nt'
  else fresh nt'

(* -------------------------------------------------------------------------- *)

(* Before we can operate on a partial grammar, we must build a symbol table,
   which maps a symbol to a symbol kind. We build one symbol table for each
   partial grammar.  *)

type kind =

  | NTPublic of Range.range
      (**This nonterminal symbol is declared public at a certain position. *)

  | NTPrivate of Range.range
      (**This nonterminal symbol is declared (private) at a certain position. *)

  | Terminal of properties
      (**This is a terminal symbol. *)

  | Unknown of Range.range
      (**We do not know yet what this symbol means. Perhaps we have not
         encountered its definition yet; perhaps it is not defined at
         all in this partial grammar. *)

(* These functions access a (mutable) symbol table. *)

module T : sig
  type table (* mutable *)
  val create : unit -> table
  val find : table -> string -> kind (* can raise [Not_found] *)
  val add : table -> string -> kind -> unit
  val replace : table -> string -> kind -> unit
  val public_symbols : table -> StringSet.t
  val private_symbols : table -> StringSet.t
  val unknown_symbols : table -> StringSet.t
end = struct
  include Hashtbl
  type table = (symbol, kind) t
  let create () = create 13
  let add table symbol kind =
    let (_ : string) = use symbol in
    add table symbol kind
  let replace table symbol kind =
    assert (mem table symbol);
    assert (used symbol);
    replace table symbol kind
  let gather (p : kind -> bool) table =
    MRef.with_state StringSet.empty @@ fun accu ->
    table |> iter @@ fun symbol kind ->
    if p kind then accu := StringSet.add symbol !accu
  let public_symbols table =
    let is_public = function NTPublic _ -> true | _ -> false in
    StringSet.add "error" (gather is_public table)
  let private_symbols table =
    let is_private = function NTPrivate _ -> true | _ -> false in
    gather is_private table
  let unknown_symbols table =
    let is_unknown = function Unknown _ -> true | _ -> false in
    gather is_unknown table
end

(* [store_symbol table symbol kind] extends the table [table] with a
   mapping of the symbol [symbol] to the kind [kind]. Various errors
   are detected at this point. If a previous mapping of this symbol
   to [Unknown _] existed then it is replaced. *)

let store_symbol (table : T.table) symbol kind =
  match T.find table symbol, kind with

  (* The symbol is not known so far. Add it. *)
  | exception Not_found ->
      T.add table symbol kind

  (* There are two definitions of this symbol in one grammatical unit
     (that is, one .mly file), and at least one of them is private.
     This is forbidden. *)
  | NTPrivate pos, NTPrivate pos'
  | NTPublic pos, NTPrivate pos'
  | NTPrivate pos, NTPublic pos' ->
      if can_complain_about symbol then
        signal main [ pos; pos']
          "the nonterminal symbol %s is multiply defined.\n\
           Only %%public symbols can have split definitions."
          symbol

  (* The symbol is known to be a terminal symbol but is now declared
     as a nonterminal symbol. *)
  | Terminal properties, (NTPrivate pos | NTPublic pos)
  | (NTPrivate pos | NTPublic pos), Terminal properties ->
      if can_complain_about symbol then
        signal main [ pos; properties.tk_position ]
          "the identifier %s is a reference to a token."
          symbol

  (* In the following cases, we do not gain any piece of information.
     As of 2017/03/29, splitting the definition of a %public nonterminal
     symbol is permitted. (It used to be permitted over multiple units,
     but forbidden within a single unit.) *)
  | _, Unknown _
  | Terminal _, Terminal _
  | NTPublic _, NTPublic _ ->
      ()

  (* We learn that the symbol is a nonterminal or a token. *)
  | Unknown _, _ ->
      T.replace table symbol kind

(* [tokens] maps the terminal symbols of a grammar to property records. *)

type tokens =
  properties StringMap.t

(* [initial_kind tokens pos symbol] constructs a kind for the symbol
   [symbol], which has just been encountered at position [pos]. *)

let initial_kind (tokens : tokens) pos symbol =
  try
    Terminal (StringMap.find symbol tokens)
  with Not_found ->
    Unknown pos

(* [initial_encounter] records a first encounter of the symbol [symbol]. *)

let initial_encounter tokens table (symbol : symbol located) =
  let pos, symbol = position symbol, value symbol in
  store_symbol table symbol (initial_kind tokens pos symbol)

(* [non_terminal_is_not_reserved] checks that [symbol], which is used as a
   nonterminal symbol, is not a reserved word. This error is intentionally
   made fatal because otherwise we would continue and publish a strange
   message about renaming the symbol [error]. *)

let non_terminal_is_not_reserved symbol positions =
  if symbol = "error" then
    if can_complain_about symbol then
      error main positions
        "%s is reserved and thus cannot be used \
         as a non-terminal symbol." symbol

(* This checks that [symbol], which is used as a nonterminal symbol,
   is not already known to be a terminal symbol. *)

let non_terminal_is_not_a_token (tokens : tokens) symbol positions =
  match StringMap.find symbol tokens with
  | exception Not_found ->
      ()
  | properties ->
      if can_complain_about symbol then
        signal main (positions @ [ properties.tk_position ])
          "the identifier %s is a reference to a token."
          symbol

(* [store_nonterminal tokens table symbol positions public] records the fact
   that [symbol] is used as a nonterminal symbol. The parameter [public]
   determines whether this symbol should be considered public or private. *)

let store_nonterminal tokens table symbol positions public =
  non_terminal_is_not_reserved symbol positions;
  non_terminal_is_not_a_token tokens symbol positions;
  let pos = List.hd positions in
  let kind = if public then NTPublic pos else NTPrivate pos in
  store_symbol table symbol kind

(* [build tokens start_symbols g] builds a symbol table for the partial
   grammar [g]. [tokens] and [start_symbols] are the terminal symbols
   and the start symbols found in ALL partial grammars. *)

type start_symbols =
  range StringMap.t

let build (tokens : tokens) (start_symbols : start_symbols) (g : partial_grammar)
: T.table =
  let table = T.create() in
  let () =
    (* For each rule, *)
    g.pg_rules |> List.iter @@ fun prule ->
    (* Process the left-hand side of this rule. This nonterminal symbol is
       considered public if it is marked %public or if it is a start symbol. *)
    let nt = prule.pr_nt in
    let public = prule.pr_public || StringMap.mem nt start_symbols in
    store_nonterminal tokens table nt prule.pr_positions public;
    (* For each branch in this rule, *)
    prule.pr_branches |> List.iter @@ fun branch ->
    (* For each symbol in the right-hand side, *)
    branch.pb_producers |> List.iter @@ fun (_, param, _) ->
    param |> Parameter.iter @@ fun symbol ->
    (* If this symbol is a parameter of this rule, then it is a local name;
       it should not be added to the symbol table. Otherwise, remember that
       this symbol has been encountered. *)
    if not (List.mem (value symbol) prule.pr_parameters) then
      initial_encounter tokens table symbol
  in
  table

(* -------------------------------------------------------------------------- *)

let empty, union, inter, diff, big_union =
  StringSet.(empty, union, inter, diff, big_union)

(* [every_unknown_symbol_is_public public table] checks that every symbol
   that is mapped to [Unknown _] in the table [table] appears in the set
   [public]. This set contains all public symbols of all partial grammars. *)

let every_unknown_symbol_is_public public table =
  let undefined = diff (T.unknown_symbols table) public in
  undefined |> StringSet.iter @@ fun symbol ->
  match T.find table symbol with
  | Unknown pos ->
      if can_complain_about symbol then
        signal main [ pos ] "The symbol %s is undefined." symbol
  | _ ->
      assert false

(* [clashes public tables] computes the set of symbols that are private in
   one partial grammar yet are also declared public (in some other partial
   grammar). These symbols must be renamed (in every partial grammar where
   they are used as a private symbol). *)

let clashes public tables =
  let defined = ref public in
  MRef.with_state empty @@ fun clashes ->
  tables |> List.iter @@ fun table ->
    let local = T.private_symbols table in
    (* Any local symbol that is either public or previously defined
       in another partial grammar is a clash. *)
    clashes := union !clashes (inter local !defined);
    defined := union !defined local

(* [rename_private_symbols clashes grammars tables] renames the clashing
   private symbols in each partial grammar in the list [grammars], producing a
   new list of grammars. A private symbol is clashing if it appears in the set
   [clashes]. [tables] is a list of the symbol tables for the grammars in the
   list [grammars]. *)

let rename_private_symbols clashes grammars tables : partial_grammar list =
  (* For each partial grammar (and accompanying symbol table), *)
  List.map2 (fun grammar table ->
    (* Construct a renaming of the private symbols in this grammar, *)
    let phi =
      StringSet.fold (fun x phi ->
        match T.find table x with
        | NTPrivate pos ->
            let x' = rename x grammar.pg_filename in
            note info [pos] "the nonterminal symbol %s is renamed %s.\n" x x';
            R.add x x' phi
        | _ ->
            phi
        | exception Not_found ->
            phi
      ) clashes R.empty
    in
    (* and apply this renaming to this partial grammar. *)
    rewrite_grammar phi grammar
  ) grammars tables

(* [merge_rules nt rule rule'] merges two rules whose left-hand side is
   the nonterminal symbol [nt]. The result is a single rule. *)

let merge_rules nt rule rule' : parameterized_rule =
  assert (rule.pr_nt = nt && rule'.pr_nt = nt);
  let positions = rule.pr_positions @ rule'.pr_positions in
  (* Check that the two rules have the same number of parameters.
     This error is intentionally fatal. This allows us to use [R.zip]
     without worrying that the two lists might have different lengths. *)
  let n, n' = List.(length rule.pr_parameters, length rule'.pr_parameters) in
  if n <> n' then
    error main positions
      "the symbol %s is defined with arities %d and %d."
      nt n n';
  (* Check that the two rules have the same %inline status. *)
  if rule.pr_inline <> rule'.pr_inline && can_complain_about nt then
    signal main positions
      "not all definitions of %s are marked %%inline." nt;
  (* Combine the branches. The parameters could have different names in the
     two rules, so a renaming may be necessary. *)
  (* We want to rename the parameters of [rule] to those of [rule'], or
     vice-versa. However, one cannot always do so straight ahead: a name
     capture can take place if a name that serves as a parameter in one rule
     appears free in the other rule. So, we first rename the parameters of
     [rule'] so as to avoid the free names of [rule]. Then we can rename
     the parameters of [rule] to those of [rule'] without fear of capture. *)
  let rule' = avoid (FreeNames.rule rule) rule' in
  let phi = R.zip rule.pr_parameters rule'.pr_parameters in
  let branches = rewrite_branches phi rule.pr_branches in
  { rule' with
    pr_positions = positions;
    pr_branches  = branches @ rule'.pr_branches;
    pr_attributes = rule.pr_attributes @ rule'.pr_attributes }

type rules =
  parameterized_rule StringMap.t

let extend (rules : rules) nt rule =
  StringMap.add nt rule rules

(* [collect_all_rules] collects all rules in the partial grammars [grammars],
   whose symbol tables form the list [tables]. The result is a single set of
   rules. *)

let collect_all_rules grammars tables : rules =

  (* Retrieve all the public symbols of all partial grammars. *)
  let public = tables |> List.map T.public_symbols |> big_union in

  (* Every symbol that is unknown in a partial grammar should be declared
     public (in some other partial grammar). *)
  List.iter (every_unknown_symbol_is_public public) tables;

  (* Compute which private symbols must be renamed. *)
  let clashes = clashes public tables in

  (* Rename the private symbols that must be renamed. We do not update
     the symbol tables, so they must not be used any more. *)
  let grammars = rename_private_symbols clashes grammars tables in

  (* Merge the definitions of public nonterminal symbols and copy the
     definitions of private nonterminal symbols. Since all name clashes
     have been resolved already, these copies are safe. *)
  (* For each partial grammar, *)
  accumulate StringMap.empty grammars @@ fun rules grammar ->
  (* For each rule in this partial grammar, defining a symbol [nt], *)
  accumulate rules grammar.pg_rules @@ fun rules rule ->
  let nt = rule.pr_nt in
  (* Extend our current set of rules with a definition of [nt], *)
  extend rules nt @@
  (* whose right-hand side is the following. *)
  match StringMap.find nt rules with
  | exception Not_found ->
      (* There is no previous definition of [nt]. Just keep this rule. *)
      rule
  | rule' ->
      (* There is a previous definition of [nt]. Merge the previous rule
         and this new rule. *)
      merge_rules nt rule rule'

(* The empty grammar. *)

let empty : grammar =
  {
    p_preludes                = [];
    p_postludes               = [];
    p_parameters              = [];
    p_start_symbols           = StringMap.empty;
    p_types                   = [];
    p_tokens                  = StringMap.empty;
    p_rules                   = StringMap.empty;
    p_on_error_reduce         = [];
    p_grammar_attributes      = [];
    p_symbol_attributes       = [];
    p_default_merge           = None;
  }

(* We are at last ready to put everything together. *)

let join grammars =
  (* Check that every parameterized rule has distinct parameters. *)
  List.iter check grammars;
  (* Construct a unified grammar, which does not yet have any rules. *)
  let grammar = List.fold_left join empty grammars in
  (* Nevertheless, this step is useful, as it gives us the knowledge of all
     tokens and all start symbols. *)
  let tokens = grammar.p_tokens
  and start_symbols = grammar.p_start_symbols in
  (* Build a symbol table for each partial grammar. *)
  let tables = List.map (build tokens start_symbols) grammars in
  (* Now collect and merge all rules in all partial grammars. *)
  let rules = collect_all_rules grammars tables in
  (* Done! *)
  { grammar with p_rules = rules }

end (* Make *)
