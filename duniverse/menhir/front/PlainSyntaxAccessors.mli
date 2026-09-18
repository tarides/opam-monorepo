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

(**This module offers a number of accessors that help exploit a grammar. *)

(**[prod_id] returns the identifier of a producer, that is, the name of
   the variable that is bound to the semantic value of this producer. *)
val prod_id : producer -> identifier

(**[prod_attributes] returns the attributes of a producer. *)
val prod_attributes : producer -> attributes

(**[ocamltype_of_token grammar symbol] produces the OCaml type of the terminal
   symbol [symbol] in the grammar [grammar]. This terminal symbol must exist
   and must not be a pseudo-token. [None] is returned if and only if this
   token does not carry a semantic value. *)
val ocamltype_of_token : grammar -> symbol -> ocamltype option

(**[ocamltype_of_symbol grammar symbol] produces the OCaml type of the
   nonterminal symbol [symbol] in the grammar [grammar], if this type
   has been explicitly specified via a %type declaration. Otherwise,
   [None] is returned. *)
val ocamltype_of_symbol : grammar -> symbol -> ocamltype option

(**[ocamltype_of_start_symbol grammar symbol] produces the OCaml type of the
   start symbol [symbol] in the grammar [grammar]. The type of a start symbol
   is always known, as it must be declared. *)
val ocamltype_of_start_symbol : grammar -> symbol -> ocamltype

(**[tokens grammar] is a list of all (real) tokens in the grammar [grammar].
   The special tokens "#" and "error" are not included. Pseudo-tokens (used
   in %prec declarations, but never declared using %token) are filtered out.
   Therefore this is *not* synonymous with [grammar.tokens], which includes
   the pseudo-tokens.

   Beware: this list is not sorted in alphabetical order. *)
val tokens : grammar -> terminal list

(**[typed_tokens grammar] is analogous to [tokens grammar], but includes
   the OCaml type of each token, if it is known.

   Beware: this list is not sorted in alphabetical order. *)
val typed_tokens : grammar -> (terminal * ocamltype option) list

(**[nonterminals grammar] is a list of all nonterminal symbols in the grammar
   [grammar]. It does not include the artificial start symbols [S'] that
   Menhir creates internally.

   Beware: this list is not sorted in alphabetical order. *)
val nonterminals : grammar -> nonterminal list

(**[foreach_nonterminal grammar] enumerates the nonterminal symbols
   in the grammar [grammar], that is, the elements of the list
   [nonterminals grammar].  *)
val foreach_nonterminal : grammar -> (nonterminal -> unit) -> unit

(**[foreach_branch grammar] enumerates the branches of the grammar
   [grammar], that is, the productions. *)
val foreach_branch : grammar -> (branch -> unit) -> unit

(**[error_free_branch branch] determines whether the branch [branch]
   does not mention the special token [error]. *)
val error_free_branch : branch -> bool

(**[grammar_uses_error_token grammar] determines whether the grammar
   [grammar] mentions the special token [error]. *)
val grammar_uses_error_token : grammar -> bool

(**[is_terminal grammar symbol] tests whether the symbol [symbol] is a
   terminal symbol. This symbol must exist; that is, it must be a terminal
   symbol or a nonterminal symbol. *)
val is_terminal : grammar -> symbol -> bool

(**[is_nonterminal grammar symbol] tests whether the symbol [symbol] is a
   nonterminal symbol. This symbol must exist; that is, it must be a terminal
   symbol or a nonterminal symbol. *)
val is_nonterminal : grammar -> symbol -> bool

(**[unquoted_alias grammar symbol] returns the token alias of the terminal
   symbol [symbol], if there is one, deprived of its surrounding double quotes
   and unescaped; see [MString.unquote]. The terminal symbol [symbol] must
   exist and must not be a pseudo-token. *)
val unquoted_alias : grammar -> symbol -> alias

(**[print_production nt branch] prints the production whose left-hand side
   is the nonterminal symbol [nt] and whose right-hand side is [branch].
   An example of the result is [A -> a A B]. *)
val print_production : nonterminal -> branch -> string

(**This type reflects the outcome of a comparison
   in a possibly-non-total order. *)
type order =
  | Lt
  | Gt
  | Eq
  | Incomparable

(**[precedence_order] compares two precedence levels. *)
val precedence_order : precedence_level -> precedence_level -> order

(**[production_order] compares two production levels. *)
val production_order : production_level -> production_level -> order

(**[transform_branch_producers f] applies the transformation function [f]
   to the producers (the right-hand side) of the branch [branch]. *)
val transform_branch_producers : (producers -> producers) -> branch -> branch
