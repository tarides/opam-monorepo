(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module defines several signatures that play an important role
   in the middle-end and back-end. *)

open Report
include FrontTypes

(* -------------------------------------------------------------------------- *)

(** {2 Numbered Types} *)

(**The signature [NUMBERING] describes a type [t] whose elements are in a
   bijection with an integer segment [\[0, n)]. This signature provides
   the integer constant [n] as well as the operations [encode], [decode].
   Please note that [decode] is considered {b unsafe} and should be used
   as little as possible. *)
module type NUMBERING =
  Fix.NUMBERING

(**The signature [NUMBERED] extends [NUMBERING] with more operations:
   [equal], [compare], [init], [tabulate], [iter], [fold], [map]. *)
module type NUMBERED =
  Fix.NUMBERED

(* -------------------------------------------------------------------------- *)

(**The signature [GRAMMAR] offers a rich view of a grammar. This view is
   well-suited to the computations that the middle-end performs. It offers
   abstract types of terminal symbols, nonterminal symbols, symbols, and
   productions, as well as efficient sets and maps. Furthermore, it offers
   access to the results of several analyses of the grammar, including
   NONEMPTY, NULLABLE, FIRST, FOLLOW, MINIMAL, MAXIMAL. The functor
   [GrammarConstruction.Make] implements this signature. *)
module type GRAMMAR = sig

(* -------------------------------------------------------------------------- *)

(** {2 Nonterminal Symbols} *)

module Nonterminal : sig

  (**The type of nonterminal symbols includes the nonterminal symbols that
     the user is aware of, plus, for each user start symbol [S], an internal
     start symbol [S'].

     A nonterminal symbol is internally represented as a small nonnegative
     integer. Two conversion functions exist. They are exploited in the
     table-based back-end. They are considered unsafe and should be used as
     little as possible. *)
  type t

  (**This type is numbered. *)
  include NUMBERED with type t := t

  (**[is_internal_start nt] tells whether the nonterminal symbol [nt] is one
     of the internal start symbols, that is, one of the new symbols [S']
     that are created for each user start symbol [S]. *)
  val is_internal_start: t -> bool

  (**[is_user_start nt] tells whether the nonterminal symbol [nt] is a
     user start symbol, that is, one of the start symbols that the user
     has defined. *)
  val is_user_start: t -> bool

  (**[lookup] maps an identifier [s] to a nonterminal symbol. If there is
     no nonterminal symbol named [s] then [Not_found] is raised. *)
  val lookup: string -> t

  (**[print normalize nt] produces a string representation of the
     nonterminal symbol [nt].

     Ideally, [nt] should not be an internal start symbol, as we do not wish
     users to be aware of the existence of these extra nonterminal symbols.
     However, we do sometimes violate this rule when it is difficult to do
     otherwise.

     The Boolean parameter [normalize] indicates whether the string
     representation should be normalized, that is, whether parentheses and
     commas should be replaced with underscores. This is necessary if the
     string is intended for use as a valid nonterminal name or as a valid
     OCaml identifier. *)
  val print: bool -> t -> string

  (**[ocamltype nt] is the OCaml type associated with the nonterminal symbol
     [nt]. [nt] must not be an internal start symbol.

     The value [None] means that the type of this nonterminal symbol is
     unknown. The type of a symbol is known only if the grammar contains a
     %type declaration for it. Type inference augments a grammar with %type
     declarations; see [Infer]. *)
  val ocamltype: t -> ocamltype option

  (**[ocamltype_of_start_symbol] is a simplified variant of [ocamltype],
     which can be applied only to (user) start symbols. Because every start
     has a known type, this function has result type [ocamltype]. *)
  val ocamltype_of_start_symbol: t -> ocamltype

  (**[check_every_symbol_has_ocaml_type reason] checks that every nonterminal
     symbol has a known OCaml type. If this is not the case then the process
     is aborted with a fatal error. *)
  val check_every_symbol_has_ocaml_type: string -> unit

  (**[positions nt] is a list of the positions associated with the definition
     of the nonterminal symbol [nt]. [nt] should not be an internal start
     symbol. There can be more than one position because definitions can be
     split over multiple files. *)
  val positions: t -> ranges

  (**[attributes nt] is the list of attributes attached with the nonterminal
     symbol [nt]. *)
  val attributes: t -> attributes

  (**[merge_function nt] is the (optional) %merge function associated with the
     nonterminal symbol [nt]. This %merge function is exploited only when the
     GLR back-end is selected. *)
  val merge_function: t -> merge_fun option

  (**[iterx] enumerates the nonterminal symbols
     except the internal start symbols. *)
  val iterx: (t -> unit) -> unit

  (**[foldx] enumerates the nonterminal symbols
     except the internal start symbols. *)
  val foldx: (t -> 'a -> 'a) -> 'a -> 'a

  (**[mapx] enumerates the nonterminal symbols
     except the internal start symbols. *)
  val mapx: (t -> 'a) -> 'a list

  (**[user_start_symbols] is a list of the user start symbols. *)
  val user_start_symbols: t list

end

(* -------------------------------------------------------------------------- *)

(**Sets whose elements are nonterminal symbols. *)
module NonterminalSet : Vendored_bitsets.API.SET with type elt = Nonterminal.t

(**Maps whose keys are nonterminal symbols. *)
module NonterminalMap : BaseAPI.MAP with type key = Nonterminal.t

(* -------------------------------------------------------------------------- *)

(** {2 Terminal Symbols} *)

module Terminal : sig

  (**The type of terminal symbols includes the terminal symbols that the
     user is aware of, plus the two special symbols [#] and [error].

     A terminal symbol is internally represented as a small nonnegative
     integer. Two conversion functions exist. They are exploited in the
     table-based back-end and in other places. They are considered unsafe
     and should be used as little as possible. *)
  type t

  (**This type is numbered. *)
  include NUMBERED with type t := t

  (**[lookup] maps an identifier [s] to a terminal symbol. If there is no
     terminal symbol named [s] then [Not_found] is raised. *)
  val lookup: string -> t

  (**[print t] produces a string representation of the terminal symbol [t]. *)
  val print: t -> string

  (**The special terminal symbol [#] denotes the end of the input stream. *)
  val sharp: t

  (**The special terminal symbol [error] is accessible to the user and is
     used for handling errors. (We discourage its use, but it is currently
     still supported.) *)
  val error: t

  (**[special t] determines whether the terminal symbol [t] is special,
     that is, whether it is one of the special symbols [#] and [error]. *)
  val special: t -> bool

  (**[real t] is equivalent to [not (special t)]. It determines whether
     the terminal symbol is real, that is, whether it is a symbol in the
     input alphabet. *)
  val real: t -> bool

  (**[eof] is the programmer-defined terminal symbol [EOF], if there is one.
     It is recognized based solely on its name, which is fragile, but this
     behavior is documented. This token is assumed to represent [ocamllex]'s
     [eof] pattern. It is used only by the reference interpreter, and in a
     rather non-essential way. *)
  val eof: t option

  (**[ocamltype t] returns the OCaml type of the semantic value of the
     terminal symbol [t]. If this terminal symbol carries no semantic value,
     then it returns [None]. *)
  val ocamltype: t -> ocamltype option

  (**[associativity t] is the associativity of the terminal symbol [t]. It
     plays a role in the resolution of shift/reduce conflicts. *)
  val associativity: t -> associativity

  (**[attributes t] is the list of attributes attached with the terminal
     symbol [t]. *)
  val attributes: t -> attributes

  (**[alias t] is the token alias that has been defined by the user
     for the terminal symbol [t], if there is one. It is a quoted
     escaped string literal.  *)
  val alias: t -> string option

  (**[unquoted_alias t] is the string [alias t],
     deprived of its opening and closing quotes, and unescaped.*)
  val unquoted_alias: t -> string option

  (**[every_token_has_an_alias] is [true] if a token alias has been defined
     by the user for every (real) token. *)
  val every_token_has_an_alias: bool

  (**[iterx] enumerates the terminal symbols except [#]. *)
  val iterx: (t -> unit) -> unit

  (**[foldx] enumerates the terminal symbols except [#]. *)
  val foldx: (t -> 'a -> 'a) -> 'a -> 'a

  (**[mapx] enumerates the terminal symbols except [#]. *)
  val mapx: (t -> 'a) -> 'a list

  (**[initx] enumerates the terminal symbols except [#]. *)
  val initx: (t -> 'a) -> 'a array

  (**[iter_real] enumerates the real terminal symbols, that is,
     the terminal symbols except [#] and [error]. *)
  val iter_real: (t -> unit) -> unit

  (**[fold_real] enumerates the real terminal symbols, that is,
     the terminal symbols except [#] and [error]. *)
  val fold_real: (t -> 'a -> 'a) -> 'a -> 'a

  (**[map_real] enumerates the real terminal symbols, that is,
     the terminal symbols except [#] and [error]. *)
  val map_real: (t -> 'a) -> 'a list

end

(* -------------------------------------------------------------------------- *)

(** {2 Sets and Maps of Terminal Symbols} *)

(**Sets whose elements are terminal symbols. *)
module TerminalSet : sig

  include Bitsets.API.SET with type elt = Terminal.t

  (**[universe] is the set of all real terminal symbols, that is, all terminal
     symbols except [#] and [error]. *)
  val universe: t

  (**[of_list] converts a list of terminal symbols
     into a set of terminal symbols. *)
  val of_list: elt list -> t

  (**[pick f ts] returns an element of the set [ts] which satisfies [f],
     provided such an element exists. *)
  val pick: (elt -> bool) -> t -> elt option

  (**[print] converts a set of terminal symbols into a string representation.
     The symbols are listed one after the other and separated with spaces. *)
  val print: t -> string

  (**[identify] offers a string representation of a set of terminals. This
     string is a valid OCaml identifier and begins with the character 'c'.
     Furthermore, this representation is unambiguous: two distinct sets are
     mapped to two distinct identifiers. *)
  val identify: t -> string

end

(**Maps whose keys are terminal symbols. *)
module TerminalMap : sig
  include BaseAPI.MAP with type key = Terminal.t
  include BaseAPI.CONVERT
    with type key := key
     and type 'a map := 'a t
     and type set := TerminalSet.t
end

(* -------------------------------------------------------------------------- *)

(** {2 Symbols} *)

module Symbol : sig

  (**A symbol is either a nonterminal symbol or a terminal symbol. *)
  type t =
    | N of Nonterminal.t
    | T of Terminal.t

  (**[equal] determines whether two symbols are equal. *)
  val equal: t -> t -> bool

  (**[compare] compares two symbols according to a fixed but unspecified
     total order. *)
  val compare: t -> t -> int

  (**[is_terminal sym] determines whether the symbol [sym] is a terminal
      symbol. *)
  val is_terminal: t -> bool

  (**[is_nonterminal sym] determines whether the symbol [sym] is a
     nonterminal symbol. *)
  val is_nonterminal: t -> bool

  (**[lookup] maps an identifier [s] to a symbol. There must be exist a
     nonterminal or terminal symbol named [s]. *)
  val lookup: string -> t

  (**[print normalize sym] prints (produces a string representation of)
     the symbol [sym], using either [Nonterminal.print normalize] or
     [Terminal.print]. *)
  val print: bool -> t -> string

  (**[print_array' normalize syms] prints the symbols in the array [syms].
     Every symbol is preceded with a space. *)
  val print_array': bool -> t array -> string

  (**[print_array normalize syms] prints the symbols in the array [syms].
     The symbols are separated with spaces. *)
  val print_array: bool -> t array -> string

  (**[print_subarray normalize start syms] prints the symbols in the array
     [syms], starting at offset [start]. The symbols are separated with
     spaces. *)
  val print_subarray: bool -> int -> t array -> string

  (**[print_array_bullet normalize dot] prints the symbols in the array
     [syms]. The symbols are separated with spaces. Furthermore, if the
     offset [dot] is part of the closed interval [\[start, n\]], where [n]
     is the length of the array [symbols], then the character '.' is
     printed at offset [dot]. Therefore, this character can appear first,
     appear between two symbols, or appear last. *)
  val print_array_bullet: bool -> int -> t array -> string

  (**[iter] enumerates the symbols. *)
  val iter: (t -> unit) -> unit

end

(* -------------------------------------------------------------------------- *)

(** {2 Sets and Maps of Symbols} *)

(**Sets whose elements are symbols. *)
module SymbolSet : sig

  include Set.S with type elt = Symbol.t

  (**[print normalize syms] prints the symbols in the set [syms].
     The symbols are separated with spaces. *)
  val print: bool -> t -> string

end

(**Maps whose keys are symbols. *)
module SymbolMap : sig

  include Map.S with type key = Symbol.t

  (**[terminals m] returns the terminal symbols in the domain of the map [m]. *)
  val terminals: 'a t -> TerminalSet.t

end

(* -------------------------------------------------------------------------- *)

(** {2 Productions} *)

module Production : sig

  (**The type of productions includes the user-defined productions as
     well as the internally generated productions associated with the
     start symbols.

     A production is internally represented as a small nonnegative
     integer. Two conversion functions exist. They are exploited in
     the encoding of items. They are considered unsafe and should
     be used as little as possible. *)
  type t

  (**This type is numbered. *)
  include NUMBERED with type t := t

  (**The productions whose index is less than [start] are the start
     productions. The integer index [start] is published for use by
     the table back-end and the GLR back-end. It should not be used
     otherwise. *)
  val start: int

  (**[start_production nt] is the start production associated with the (user)
     nonterminal symbol [nt]. *)
  val start_production: Nonterminal.t -> t

  (**The definition of a production consists of a nonterminal symbol (the
     left-hand side) and an array of symbols (the right-hand side). *)

  (**[def prod] returns the definition of the production [prod]. *)
  val def: t -> Nonterminal.t * Symbol.t array

  (**[nt prod] returns the left-hand side of the production [prod]. *)
  val nt: t -> Nonterminal.t

  (**[rhs prod] returns the right-hand side of the production [prod]. *)
  val rhs: t -> Symbol.t array

  (**[length prod] returns the length of the production [prod].
     It is equivalent to [Array.length (rhs prod)]. *)
  val length: t -> int

  (**[identifiers prod] returns an array of the identifiers that should be used
     to name the semantic values of the symbols in the right-hand side of the
     production [prod]. The length of this array is [length prod]. *)
  val identifiers: t -> identifier array

  (**[action prod] returns the semantic action associated with the production
     [prod]. [prod] must not be a start production. *)
  val action: t -> action

  (**[positions prod] is a list of the positions associated with the
     production [prod]. This is usually a singleton list. A start production
     can have more than one position if the definition of the corresponding
     start symbol is split over multiple files. *)
  val positions: t -> ranges

  (**[attributes prod] are the attributes attached with the production
     [prod]. *)
  val attributes: t -> attributes

  (**[rhs_attributes prod] is an array of the attributes attached with each
     element in the right-hand side of the production [prod]. *)
  val rhs_attributes: t -> attributes array

  (**[prec_decl prod] is the optional %prec annotation carried by the
     production [prod]. This information plays a role in the resolution of
     shift/reduce conflicts. *)
  val prec_decl: t -> prec_annotation

  (**[production_level prod] is the production level of the production [prod].
     This information plays a role in the resolution of reduce/reduce
     conflicts. *)
  val production_level: t -> production_level

  (**[is_start prod] determines whether the production [prod] is a start
     production. *)
  val is_start: t -> bool

  (**[test_start prod] determines whether the production [prod] is a start
     production. If it is a start production, then the user start symbol that
     this production is associated with is returned. If it is a regular
     production, nothing is returned. *)
  val test_start: t -> Nonterminal.t option

  (**[get_start] maps a start production to a user start symbol.
     [get_start prod] is equivalent to [Option.get (test_start prod)]. *)
  val get_start: t -> Nonterminal.t

  (**[error_free prod] returns [true] if the right-hand side of the
     production [prod] does {i not} contain the [error] token. *)
  val error_free: t -> bool

  (**[print prod] is a string representation of the production [prod]. It
     must not be applied to a start production, because we do not wish
     users to become aware of the existence of these extra productions. *)
  val print: t -> string

  (**[describe prod] is a representation of the production [prod] as a string
     of the form "accepting <nt>" or "reducing <nt> -> <rhs>". The Boolean
     flag [gerund] allows choosing between gerund and infinitive forms. *)
  val describe: bool -> t -> string

  (**[iterx] enumerates the productions except the start productions. *)
  val iterx: (t -> unit) -> unit

  (**[foldx] enumerates the productions except the start productions. *)
  val foldx: (t -> 'a -> 'a) -> 'a -> 'a

  (**[mapx] enumerates the productions except the start productions. *)
  val mapx: (t -> 'a) -> 'a list

  (**[iternt] enumerates the productions of the nonterminal symbol [nt]. *)
  val iternt: Nonterminal.t -> (t -> unit) -> unit

  (**[foldnt] enumerates the productions of the nonterminal symbol [nt]. *)
  val foldnt: Nonterminal.t -> (t -> 'a -> 'a) -> 'a -> 'a

  (**[mapnt] enumerates the productions of the nonterminal symbol [nt]. *)
  val mapnt:  Nonterminal.t -> (t -> 'a) -> 'a list

end

(* -------------------------------------------------------------------------- *)

(** {2 Maps of Productions} *)

module ProductionMap : sig

  include BaseAPI.MAP with type key = Production.t

  (**[start] constructs a map whose domain consists of just the start
     productions. *)
  val start: (Production.t -> 'a) -> 'a t

end

(* -------------------------------------------------------------------------- *)

(** {2 Reduction Tables} *)

module Reductions : sig

  (**A reduction table maps terminal symbols to nonempty lists of productions.
     A reduction table concerns just one state. It can be nondeterministic:
     that is, a terminal symbol can be mapped to a list whose length is
     greater than 1. *)
  type t =
    Production.t list TerminalMap.t

  (**A reverse reduction table maps productions to sets of terminal symbols. *)
  type reverse =
    TerminalSet.t ProductionMap.t

  (**[reverse] transforms a reduction table into a reverse reduction table. *)
  val reverse: t -> reverse

  (**[flip] transforms a reverse reduction table into a reduction table. *)
  val flip: reverse -> t

  (**[has_shift_reduce_conflict transitions reductions] determines whether the
     tables [transitions] and [reductions] have a shift/reduce conflict, that
     is, whether there exists a terminal symbol [t] for which a transition
     exists and a reduction is permitted. *)
  val has_shift_reduce_conflict: _ SymbolMap.t -> t -> bool

  (**[has_reduce_reduce_conflict reductions] determines whether the table
     [reductions] has a reduce/reduce conflict, that is, whether there exists
     a terminal symbol [t] for which several reductions are permitted. *)
  val has_reduce_reduce_conflict: t -> bool

  (**[has_conflict transitions reductions] determines whether the tables
     [transitions] and [reductions] have a shift/reduce or reduce/reduce
     conflict. End-of-stream conflicts are not taken into account. *)
  val has_conflict: _ SymbolMap.t -> t -> bool

  (**[conflict_symbols transitions reductions] is the set of all terminal
     symbols [t] such that the tables [transitions] and [reductions] have a
     shift/reduce or reduce/reduce conflict on the symbol [t]. End-of-stream
     conflicts are not taken into account. *)
  val conflict_symbols: _ SymbolMap.t -> t -> TerminalSet.t

  (**[has_eos_conflict transitions reductions] tells whether the tables
     [transitions] and [reductions] have an end-of-stream conflict, that is, a
     reduction action on [#] and at least one other (shift or reduce)
     action. *)
  val has_eos_conflict: _ SymbolMap.t -> t -> bool

end

(* -------------------------------------------------------------------------- *)

(** {2 Items} *)

module Item : sig

  (**An LR(0) item encodes a pair of integers, namely the index of the
     production and the index of the bullet in the production's right-hand
     side. *)
  type t

  (**[equal] determines whether two items are equal. *)
  val equal: t -> t -> bool

  (**[import (prod, i)] converts a pair of a production [prod] and an index [i]
     to an item. The index [i] must lie in the interval [\[0, n\]], where [n]
     is the length of production [prod]. *)
  val import: Production.t * int -> t

  (**[export item] converts the item [item] back to a pair of a production
     [prod] and an index [i]. The index [i] lies in the interval [\[0, n\]],
     where [n] is the length of production [prod]. *)
  val export: t -> Production.t * int

  (**An item is internally encoded as a single integer value. This feature
     should be used as little as possible. It is currently exploited in the
     table back-end only. The decoding function (which is really a copy of
     [export]) is in [InspectionTableInterpreter]. *)
  val marshal: t -> int

  (**[is_start item] determines whether the item [item] is a start item.
     This is the case if the production [prod] is a start production and
     the index [i] is zero. *)
  val is_start: t -> bool

  (**[test_start item] determines whether the item [item] is a start item.
     If so, the user start symbol that corresponds to this start production
     is returned. *)
  val test_start: t -> Nonterminal.t option

  (**[get_start] maps a start item to a user start symbol.
     [test_start item] is equivalent to [Option.get (test_start item)]. *)
  val get_start: t -> Nonterminal.t

  (**[print item] produces a string representation of the item [item]. *)
  val print: t -> string

  (**A shift item is an item where the bullet can still advance. A reduce item
     is an item where the bullet has reached the end of the right-hand side of
     the production. *)
  type kind =
    | Shift of Symbol.t * t
        (**[Shift] carries the (terminal or nonterminal) symbol that is expected
           next and the item that is obtained by advancing the bullet. *)
    | Reduce of Production.t
        (**[Reduce] carries the production that this item is associated with. *)

  (**[classify item] classifies the item [item] as either a shift item or a
     reduce item. *)
  val classify: t -> kind

  (**Sets whose elements are items. These sets are canonical: that is, they
     support the use of OCaml's generic equality and hash functions, so they
     can be used as keys in a hash table. *)
  module Set : BaseAPI.SET with type elt = t

  (**Maps whose keys are items. These maps are canonical: that is, they
     support the use of OCaml's generic equality and hash functions, so
     they can be used as keys in a hash table. *)
  module Map : sig
    include BaseAPI.MAP with type key = t
    include BaseAPI.CONVERT
        with type key := key
        and type 'a map := 'a t
        and type set := Set.t
  end

end (* Item *)

(* -------------------------------------------------------------------------- *)

(* Abstract LR states. *)

module AbstractState : sig

  (**An abstract LR state is a map of items to lookahead sets.
     Here, the type of the lookahead sets is undetermined. *)
  type 'lookahead t =
    'lookahead Item.Map.t

  (**[transitions s] computes the outgoing (shift and goto) transitions of the
     state [s], in the form a map of symbols to successor states. The state
     [s] must be closed. The successor states are not necessarily closed. *)
  val transitions: 'lookahead t -> 'lookahead t SymbolMap.t

  (**[reverse_reductions s] computes a reverse reduction table for the state
     [s]. The state [s] must be closed. *)
  val reverse_reductions:  'lookahead t -> 'lookahead ProductionMap.t

end

(* -------------------------------------------------------------------------- *)

(** {2 Properties of the Grammar} *)

(* [attributes] are the attributes attached with the grammar. *)
val attributes: attributes

(**[grammar_uses_error_token()] determines whether the [error] token appears
   in at least one production. *)
val grammar_uses_error_token: unit -> bool

(* -------------------------------------------------------------------------- *)

(** {2 Concrete Syntax Trees} *)

module CST : sig

  (**A concrete syntax tree is a terminal node (which carries a terminal
     symbol and has no children) or a non-terminal node -- which carries
     a production, and whose children correspond to the right-hand side
     of this production.

     If the grammar involves the token [error] then a concrete syntax tree
     can also be an error node (which corresponds to a point where the
     [error] token was shifted).

     When a GLR parser is used, a concrete syntax tree can also be a
     disjunction node, which proposes several ways of understanding an input
     segment. All children of a disjunction node [CstDisj (nt, _)] must be
     non-terminal nodes for the non-terminal symbol [nt]. They must not be
     disjunction nodes.

     When a GLR parser is used, a concrete syntax tree can form a DAG in
     memory; that is, some subtrees can be shared. *)
  type cst =
    | CstTerminal of Terminal.t
    | CstNonTerminal of Production.t * cst array
    | CstError
    | CstDisj of Nonterminal.t * cst list

  (**[disj] is a smart constructor for disjunctions. In [disj nt cst1 cst2],
     the tree [cst1] can be a disjunction; [cst2] must not be a disjunction. *)
  val disj: Nonterminal.t -> cst -> cst -> cst

  (**[show c cst] pretty-prints the concrete syntax tree [cst] on the
     output channel [c]. *)
  val show: out_channel -> cst -> unit

end

(* -------------------------------------------------------------------------- *)

(** {2 Analyses of the Grammar} *)

module Analysis : sig

  (**[nonempty nt] is true if and only if the nonterminal symbol [nt]
     generates a nonempty language. That is, it is true if and only if
     this symbol generates at least one word. *)
  val nonempty: Nonterminal.t -> bool

  (**[nullable nt] is the NULLABLE flag of the nonterminal symbol [nt].
     That is, it is true if and only if this symbol generates the empty
     word [epsilon]. *)
  val nullable: Nonterminal.t -> bool

  (**[nullable sym] is the NULLABLE flag of the symbol [sym].
     A terminal symbol is not nullable. *)
  val nullable_symbol: Symbol.t -> bool

  (**[first nt] is the FIRST set of the nonterminal symbol [nt]. That is, it
     is the set of terminal symbols with which a word generated by [nt] can
     begin. This analysis is carried out under the assumption that every
     symbol generates a nonempty language. *)
  val first: Nonterminal.t -> TerminalSet.t

  (**[first sym] is the FIRST set of the symbol [sym].
     The FIRST set of a terminal symbol is just itself. *)
  val first_symbol: Symbol.t -> TerminalSet.t

  (**[nullable_first prod i] returns the NULLABLE flag and the FIRST set of
     the production suffix determined by [prod] and [i]. The offset [i] must
     lie in the interval [\[0, n\]], where [n] is the length of production
     [prod]. *)
  val nullable_first: Production.t -> int -> bool * TerminalSet.t

  (**[explain_first t rhs i] explains why the terminal symbol [t] appears in
     the FIRST set of the production suffix determined by [prod] and [i].
     The offset [i] must lie in the interval [\[0, n\]], where [n] is the
     length of production [prod]. *)
  val explain_first: Terminal.t -> Production.t -> int -> string

  (**[follow nt] is the FOLLOW set of the nonterminal symbol [nt]. It is the
     set of terminal symbols that could follow [nt] in a sentential form
     that is derived from a start symbol. *)
  val follow: Nonterminal.t -> TerminalSet.t

  (**[tfollow t] is the FOLLOW set of the terminal symbol [t]. *)
  val tfollow: Terminal.t -> TerminalSet.t

  (**[sfollow nt] is the symbolic FOLLOW set of the nonterminal symbol [nt].
     It is the set of symbols that could follow [nt] in a sentential form
     that is derived from a start symbol. *)
  val sfollow: Nonterminal.t -> SymbolSet.t

  (**[minimal nt] is the minimal size of a sentence generated by the
     nonterminal symbol [nt]. If this symbol generates an empty language,
     then [minimal nt] is [max_int]. Any productions whose right-hand side
     mentions the [error] token are ignored by this analysis. *)
  val minimal: Nonterminal.t -> int

  (**[minimal'] is analogous to [minimal]. Instead of returning just the
     length of a minimal sentence generated by [nt], it returns a minimal
     sentence. *)
  val minimal': Nonterminal.t -> Terminal.t CompletedNatWitness.t

  (**[minimal_prod prod i] is the minimal size of a sentence generated by
     the production suffix determined by [prod] and [i]. The offset [i] must
     lie in the interval [\[0, n\]], where [n] is the length of production
     [prod]. Any productions whose right-hand side mentions the [error]
     token are ignored by this computation. *)
  val minimal_prod: Production.t -> int -> int

  (**[maximal nt] is the maximal size of a sentence generated by the
     nonterminal symbol [nt]. An unbounded maximal size is represented by
     the special value [max_int]. This analysis is carried out under the
     assumption that every symbol generates a nonempty language. Any
     productions whose right-hand side mentions the [error] token are
     ignored by this analysis. *)
  val maximal: Nonterminal.t -> int

  (**[maximal_prod prod i] is the maximal size of a sentence generated by
     the production suffix determined by [prod] and [i]. The offset [i] must
     lie in the interval [\[0, n\]], where [n] is the length of production
     [prod]. Any productions whose right-hand side mentions the [error]
     token are ignored by this analysis. *)
  val maximal_prod: Production.t -> int -> int

end

(* -------------------------------------------------------------------------- *)

(** {2 [%on_error_reduce] declarations} *)

module OnErrorReduce : sig

  (**[reduce prod] determines whether the left-hand side of the production
     [prod] (a nonterminal symbol) appears in an [%on_error_reduce]
     declaration. *)
  val reduce: Production.t -> bool

  (**[iter] enumerates the nonterminal symbols that appear in an
     [%on_error_reduce] declaration. *)
  val iter: (Nonterminal.t -> unit) -> unit

  (**When two productions can be reduced, in a single state, due to
     [%on_error_reduce] declarations, these productions can be compared,
     using [preferable], to test if one of them takes precedence over the
     other. This is a partial order; two productions may be incomparable. *)
  val preferable: Production.t -> Production.t -> bool

end

(* -------------------------------------------------------------------------- *)

(** {2 Sentences} *)

module Sentence : sig

  (**A sentence is a pair of an optional start symbol and a sequence of
     terminal symbols. The start symbol can be omitted if this omission is
     unambiguous, that is, if the grammar has exactly one start symbol. *)
  type sentence =
    Nonterminal.t option * Terminal.t list

  open RawSentence
  open Validate

  (**[validate_sentence] validates a raw sentence: it checks that every symbol
     in it is defined and that a start symbol is provided, unless the grammar
     has only one start symbol, in which case it fills in the unique start
     symbol. Therefore, the sentence that is returned is always of the form
     [(Some _, _)]. *)
  val validate_sentence: (raw_sentence, sentence) validator

  (**[start sentence] returns the start symbol of the sentence [sentence].
     This sentence must have the form [(Some _, _)]. *)
  val start: sentence -> Nonterminal.t

  (**[print style sentence] prints a sentence as a space-separated list of
     terminal symbols. If [style] is [`Abstract] then each terminal symbol is
     printed as a symbolic name. If [style] is [`Concrete] then each terminal
     symbol is printed as its unquoted alias; see [Terminal.unquoted_alias].
     In the latter case, every token must have a token alias: that is,
     [Terminal.every_token_has_an_alias] must be [true]. *)
  val print: [`Abstract | `Concrete] -> sentence -> string

end

(* -------------------------------------------------------------------------- *)

(* Information, errors and warnings. *)

(**[info c] writes a few lines of information about the size of the
   grammar to the channel [c]. *)
val info: channel -> unit

(**[check_start_symbol c] checks that the language generated by a
   start symbol is not empty and is not the singleton {epsilon}.
   If this check fails, an error is signaled on the channel [c]. *)
val check_start_symbol: channel -> unit

(**[warn_empty_symbol c] emits a warning on the channel [c] if
   a nonterminal symbol generates the empty language. *)
val warn_empty_symbol: channel -> unit

(**The following functions write the results of the analyses [nullable],
   [first], [minimal], [maximal], [follow], [tfollow], [sfollow] on the
   channel that they receive as an argument. *)

val dump_nullable: channel -> unit
val dump_first: channel -> unit
val dump_minimal: channel -> unit
val dump_maximal: channel -> unit
val dump_follow: channel -> unit
val dump_tfollow: channel -> unit
val dump_sfollow: channel -> unit

(* -------------------------------------------------------------------------- *)

end (* GRAMMAR *)

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(**[LR0] is the result signature of the functor [LR0Construction.Make].
   It offers access to the LR(0) automaton. Furthermore, it provides
   facilities for the efficient construction of LR(1) automata. *)
module type LR0 = sig

(* Because this signature depends on many of the types defined in
   the signature [GRAMMAR], we include a component [G : GRAMMAR].
   It can be understood as a parameter, which can be instantiated
   by writing [LR0 with module G := ...]. *)
module G : GRAMMAR
  open G

(**A state of the LR(0) automaton is known as a node. *)
type node
type t = node

(**This type is numbered. *)
include NUMBERED with type t := t

(**[entry] is a map of each start production to the corresponding start node
   in the LR(0) automaton. *)
val entry: t ProductionMap.t

(**[start_node nt] is the start node associated with the (user)
   nonterminal symbol [nt]. *)
val start_node: Nonterminal.t -> t

(**[get_start] maps a start node to a user start symbol.
   [get_start node] is equivalent to [Option.get (test_start node)]. *)
val get_start: t -> Nonterminal.t

(**Each node in the LR(0) represents a set of LR(0) items.
   [items node] returns this set. This set is not closed. *)
val items: t -> Item.Set.t

(**All of the edges that enter a node carry the same symbol. If a node has at
   least one incoming edge, then this common symbol is known as the incoming
   symbol of this node. A node that has no incoming edges has no incoming
   symbol. A node has no incoming symbol if and only if it is a start node.
   [incoming_symbol node] is the incoming symbol of the node [node]. *)
val incoming_symbol: t -> Symbol.t option

(**[outgoing_edges node] returns the outgoing (shift and goto) edges of the
   node [node], in the form of a map of symbols (which serve as edge labels)
   to successor nodes. *)
val outgoing_edges: t -> t SymbolMap.t

(**[foreach_outgoing_edge node] enumerates the outgoing edges of the node
   [node]. *)
val foreach_outgoing_edge: t -> (Symbol.t -> t -> unit) -> unit

(**[outgoing_symbols node] constructs a map whose domain is the outgoing
   symbols of the node [node]. *)
val outgoing_symbols: t -> (Symbol.t -> 'a) -> 'a SymbolMap.t

(* -------------------------------------------------------------------------- *)

(**This module offers a concrete representation of LR(1) states. *)
module CLR1 : sig

  (**An LR(1) state is a mapping of LR(0) items to lookahead sets, that is,
     sets of terminal symbols. This is a concrete representation: it is
     simple but heavyweight. *)
  type t =
    TerminalSet.t Item.Map.t

  (**[closure s] is the closure of the LR(1) state [s]. *)
  val closure: t -> t

  (**[transitions s] computes the outgoing (shift and goto) transitions of the
     state [s], in the form a map of symbols to successor states. The state
     [s] must be closed. The successor states are not necessarily closed. *)
  val transitions: t -> t SymbolMap.t

  (**[reverse_reductions s] computes a reverse reduction table for the state
     [s]. The state [s] must be closed. *)
  val reverse_reductions: t -> Reductions.reverse

  (**[reductions s] computes a reverse reduction table for the state [s].
     The state [s] must be closed. *)
  val reductions: t -> Reductions.t

  (**[has_conflict s] determines whether the state [s] has a shift/reduce or
     reduce/reduce conflict. The state [s] must be closed. *)
  val has_conflict: t -> bool

  (**[print leading s] is a string representation of the LR(1) state [s].
     The string [leading] is inserted at the beginning of every line. *)
  val print: string -> t -> string

end

(* -------------------------------------------------------------------------- *)

(**This module offers an abstract representation of LR(1) states, which is
   particularly useful to construct of various kinds of LR(1) automata. *)
module ALR1 : sig

  (**An LR(1) state is internally represented as a pair of an LR(0) state
     number and an array of concrete lookahead sets, whose length depends on
     the LR(0) state. This representation is made available as an abstract
     type. It is equipped with a series of efficient operations. *)
  type t

  (**[compare] is a total order on states. *)
  val compare: t -> t -> int

  (**[export s] is the concrete LR(1) state that corresponds to the state
     [s]. *)
  val export: t -> CLR1.t

  (**[core s] is the core of the state [s], that is, the underlying
     LR(0) state that is obtained by erasing the lookahead sets. *)
  val core: t -> node

  (**If [node] is a start node of the LR(0) automaton then [start node]
     is the corresponding LR(1) start state. *)
  val start: node -> t

  (**[transitions s] returns the outgoing (shift and goto) transitions of the
     state [s], in the form of a map of symbols (which serve as edge labels)
     to successor states. *)
  val transitions: t -> t SymbolMap.t

  (**[transition symbol s] return the successor of the state [s] along the
     transition labeled [symbol]. This transition must exist. *)
  val transition: Symbol.t -> t -> t

  (**[reductions s] is a reduction table for the state [s]. *)
  val reductions: t -> Reductions.t

  (**[reverse_reductions s] is a reverse reduction table for the state [s]. *)
  val reverse_reductions: t -> Reductions.reverse

  (**[equal s1 s2] determines whether the states [s1] and [s2] are equal, that
     is, whether their lookahead sets are pointwise equal. This test is
     permitted only if [s1] and [s2] have the same core. *)
  val equal: t -> t -> bool

  (**[subset s1 s2] determines whether the states [s1] and [s2] are in the
     subset relation, that is, whether their lookahead sets are pointwise in the
     subset relation. This test is permitted only if [s1] and [s2] have the same
     core. *)
  val subset: t -> t -> bool

  (**[compatible s1 s2] determines whether the states [s1] and [s2] are
     compatible according to a slightly modified version of Pager's weak
     compatibility criterion. This test is permitted only if [s1] and [s2] have
     the same core. *)
  val compatible: t -> t -> bool

  (**[eos_compatible s1 s2] determines whether the states [s1] and [s2] can be
     merged without creating an end-of-stream conflict. This test is permitted
     only if [s1] and [s2] have the same core. *)
  val eos_compatible: t -> t -> bool

  (**[error_compatible s1 s2] determines whether the states [s1] and [s2] can be
     merged without creating spurious reductions on [error]. This test is
     permitted only if [s1] and [s2] have the same core. *)
  val error_compatible: t -> t -> bool

  (**[union s1 s2] computes the union of the states [s1] and [s2]. This is
     permitted only if [s1] and [s2] have the same core.The new state is
     obtained by pointwise union of the lookahead sets. If [s1] is a subset of
     [s2], then [union s1 s2] is physically equal to [s2]. *)
  val union: t -> t -> t

  (**[restrict ts s] restricts the state [s] to the set of terminal symbols
     [ts]. Every lookahead set in [s] is intersected with the set [ts]. *)
  val restrict: TerminalSet.t -> t -> t

  (**[print leading s] is a string representation of the state [s].
     The string [leading] is inserted at the beginning of every line. *)
  val print: string -> t -> string

end (* ALR1 *)

end (* LR0 *)

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(**[MINIMAL_LR1_AUTOMATON] is the result signature of several LR(1) automaton
   construction algorithms. It is a minimal description of an LR(1) automaton.
   In this format, there is no description of the reductions: the reduction
   table at each node is implicit. It can be obtained by applying the function
   [Lr0.ALR1.reductions] to the state [state node]. *)
module type MINIMAL_LR1_AUTOMATON = sig

  module Lr0 : LR0
    open Lr0.G

  (**A state of the LR(1) automaton is known as a node. *)
  type node
  type t = node

  (**This type is numbered. *)
  include NUMBERING with type t := t

  (**[entry] is a map of each start production to the corresponding start node
     in the LR(1) automaton. *)
  val entry: t ProductionMap.t

  (**[transitions node] returns the outgoing (shift and goto) transitions of
     the node [node], in the form a map of symbols to successor nodes. *)
  val transitions: t -> t SymbolMap.t

  (**[state node] is the LR(1) state that corresponds to the node [node].
     It is not necessarily closed. *)
  val state: t -> Lr0.ALR1.t

end (* MINIMAL_LR1_AUTOMATON *)

(**[LR1_AUTOMATON] is a richer description of an LR(1) automaton. In this
   format, there is an explicit description of the reductions. This format is
   sufficiently general to serve as the input and output format of several
   transformations, including silent conflict resolution, default conflict
   resolution, insertion of extra reductions on error, and introduction of
   default reductions. *)
module type LR1_AUTOMATON = sig

  include MINIMAL_LR1_AUTOMATON
  open Lr0.G

  (**[reductions node] is the reduction table at the node [node]. In an ideal
     automaton (that is, in the absence of conflict resolution), this table is
     obtained by applying [Lr0.ALR1.reductions] to the state [state node]. If
     some reductions have been removed by a conflict resolution process, or if
     some reductions have added to obey [%on_error_reduce] declarations, then
     this reduction table can differ from the ideal table. *)
  val reductions: t -> Reductions.t

  (**When set, the flag [forbid_default_reduction node] forbids adding a
     default reduction at the node [node]. In an ideal automaton, this flag
     is [false]. It can be set to [true] during silent conflict resolution:
     when a shift/reduce conflict is solved in favor of neither action, we
     set it to [true]. *)
  val forbid_default_reduction: t -> bool

  (**[test_default_reduction node] returns [Some (prod, ts)] if the node
     [node] has a default reduction of the production [prod] on the set set of
     terminal symbols [ts]. It returns [None] if this node has no default
     reduction.

     When the result is [Some (prod, ts)], this implies that the node [node]
     has a reduction of [prod] on every symbol in the set [ts], and does not
     have any other shift or reduce actions. The reduction of [prod] is then
     considered a default reduction: this reduction is performed without
     inspecting the next input symbol and sometimes even without requesting
     the next input symbol.

     The set [ts] should be either a singleton set containing just the
     terminal symbol [#] or a set of non-[#] terminal symbols. In the former
     case, reduction is performed without even requesting the next input
     token. In the latter case, first the next input token is requested, then
     reduction is performed, without inspecting this input token.

     If [node] has a default reduction then [forbid_default_reduction node]
     must be false. It is up to the automaton construction procedure to
     respect this rule. *)
  val test_default_reduction : t -> (Production.t * TerminalSet.t) option

end (* LR1_AUTOMATON *)

(**Compared with [LR1_AUTOMATON], [LR1] is a more convenient description of
   an LR(1) automaton. It does not offer more information, but offers more
   facilities for accessing this information. *)
module type LR1 = sig

  include LR1_AUTOMATON
  open Lr0.G

  (**[equal] tests whether two nodes are equal. *)
  val equal: t -> t -> bool

  (**[iter] enumerates the nodes. *)
  val iter: (t -> unit) -> unit

  (**[fold] enumerates the nodes. *)
  val fold: (t -> 'a -> 'a) -> 'a -> 'a

  (**[map] enumerates the nodes. *)
  val map: (t -> 'a) -> 'a list

  (**[init f] initializes a fresh array, indexed by the nodes. *)
  val init: (t -> 'a) -> 'a array

  (**[tabulate] tabulates a function [f] whose domain is the nodes,
     producing a new function whose time complexity is O(1). *)
  val tabulate: (t -> 'a) -> (t -> 'a)

  (**Sets whose elements are nodes. *)
  module NodeSet : sig

    include Set.S with type elt = t

    (**[leq_join p q] computes the union of the sets [p] and [q]. If the
       result is logically equal to [q], then [q] itself is returned. Thus,
       we have [leq_join p q == q] if and only if [subset p q] holds. *)
    val leq_join: t -> t -> t

    (**[print s] produces a string representation of the set [s]. The set
       is printed as a comma-separated list of elements and is surrounded
       with curly braces. This function should be used for debugging only. *)
    val print: t -> string

  end

  (**Maps whose keys are nodes. *)
  module NodeMap : Map.S with type key = t

  (**[print] produces a string representation of a node.
     The node is displayed as a number. *)
  val print: t -> string

  (**[start_node nt] is the start node associated with the user nonterminal
     symbol [nt]. *)
  val start_node: Nonterminal.t -> t

  (**[is_start node] determines whether the node [node] is a start node. *)
  val is_start: t -> bool

  (**[get_start] maps a start node to a user start symbol. *)
  val get_start: t -> Nonterminal.t

  (**All of the edges that enter a node carry the same symbol. If a node has at
     least one incoming edge, then this common symbol is known as the incoming
     symbol of this node. A node that has no incoming edges has no incoming
     symbol. A node has no incoming symbol if and only if it is a start node.
     [incoming_symbol node] is the incoming symbol of the node [node]. *)
  val incoming_symbol: t -> Symbol.t option

  (**[targets symbol] is a list of the nodes whose incoming symbol is
     [symbol]. In other words, it is a list of all targets of edges
     labeled with [symbol]. *)
  val targets: Symbol.t -> NodeSet.t

  (**[predecessors node] returns the set of the predecessors
     of the node [node]. *)
  val predecessors: t -> NodeSet.t

  (**[reduction_sites prod] is the set of all nodes where production [prod]
     might be reduced. *)
  val reduction_sites: Production.t -> NodeSet.t

  (**[has_default_reduction node] determines whether the node [node]
     has a default reduction. This function is a simplified variant of
     {!test_default_reduction}. *)
  val has_default_reduction: t -> bool

  (**[has_default_reduction_on_sharp node] determines whether the node
     [node] has a default reduction on the terminal symbol [#]. This
     function is a simplified variant of {!test_default_reduction}. *)
  val has_default_reduction_on_sharp: t -> bool

  (**[deterministic()] determines whether the LR(1) automaton is
     deterministic. This is the case if no node has a shift/reduce,
     reduce/reduce, or end-of-stream conflict. *)
  val deterministic: unit -> bool

end (* LR1 *)

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(**[SHAPE] is the result of an analysis of the shape of the stack in an LR(1)
   automaton.

   A stack shape describes just a suffix of the stack, not the whole stack.

   This shape information must be sound in the following sense: whenever there
   is an edge from [node1] to [node2] in the automaton, the shape at [node1],
   onto which a new cell is pushed, must flow into the shape at [node2]; where
   the relation "[shape1] flows into [shape2]" allows forgetting information
   by truncating a shape at its left end. For example, in the case of an
   analysis of the height of the stack, this soundness condition becomes: the
   height at [node1], plus 1, must be greater than or equal to the height at
   [node2]. In other words, the height at a node [node] must be at most the
   minimum of the heights at the predecessors of [node], plus one. *)
module type SHAPE = sig

  module G : GRAMMAR
    open G

  type node

  (**A shape is a description of the known suffix of the stack at a node. It
     is typically (but not necessarily) represented as an array of cells. By
     convention, the top of the stack is the right end of the array. *)
  type shape

  (**[node_shape node] is the shape of the stack at the node [node]. *)
  val node_shape: node -> shape

  (**[production_shape prod] is the shape of the stack at a node where the
     production [prod] can be reduced.

     In the short invariant, the length of this shape is [Production.length
     prod]. In the long invariant, its length can be greater.

     If there are no nodes where [prod] can be reduced, then this shape may be
     a bottom shape. By this I mean, in the case of the analysis [StackStates],
     that each stack cell may contain an empty set of nodes. *)
  val production_shape: Production.t -> shape

  (**[goto_shape nt] is the shape of the stack at a node where an edge
     labeled with the nonterminal symbol [nt] has just been followed.

     In the short invariant, the length of this shape is [1]: indeed, this
     shape consists of just one cell, associated with the symbol [nt].
     In the long invariant, its length can be greater. *)
  val goto_shape: Nonterminal.t -> shape

  (**[variant] distinguishes the short invariant and the long invariant. *)
  val variant: [`Short | `Long]

  (**[show_shape] produces a string representation of a shape. *)
  val show_shape: shape -> string

end

(**A kindergarten offers a set of operations on shapes. *)
module type KINDERGARTEN = sig

  (**A shape is a description of a suffix of the stack. It can be thought of
     as an array of cells. By convention, the top of the stack is the right
     end of the array. Thus, the index 0 in this array corresponds to the cell
     that lies deepest in the stack. *)
  type shape

  (**A cell is a description of a stack cell. *)
  type cell

  (**A symbol. *)
  type symbol

  (**A node. *)
  type node

  (**A set of nodes. *)
  type nodes

  (* Operations on shapes. *)

  (**[empty] is the empty shape. *)
  val empty: shape

  (**[get sh i] is the [i]-th cell in the shape [sh]. *)
  val get: shape -> int -> cell

  (**[length sh] is the length of the shape [sh]. *)
  val length: shape -> int

  (**[fold_left] enumerates the cells in a shape, from left to right. *)
  val fold_left: ('a -> cell -> 'a) -> 'a -> shape -> 'a

  (**[append sh1 sh2] is the concatenation of the shapes [sh1] and [sh2]. *)
  val append: shape -> shape -> shape

  (**[filter p sh] is the subsequence of the cells in the shape [sh]
     that satisfy the predicate [p]. *)
  val filter: (cell -> bool) -> shape -> shape

  (**[push sh cell] is the shape [sh], onto which the cell [cell]
     has been pushed. *)
  val push: shape -> cell -> shape

  (**[pop sh] is the shape [sh], deprived of its top element.
     The shape [sh] must be nonempty. *)
  val pop: shape -> shape

  (**[top sh] is the top element of the shape [sh].
     The shape [sh] must be nonempty. *)
  val top: shape -> cell

  (**[split sh k] splits the shape [sh] into two parts: a lower part of length
     [length sh - k] and an upper part of length [k]. The length of [sh] must
     be at least [k]. *)
  val split: shape -> int -> shape * shape

  (**[meet sh1 sh2] is the meet of the two shapes [sh1] and [sh2], that is, the
     logical conjunction of the information contained in [sh1] and [sh2]. It is
     computed roughly as follows. If the shapes [sh1] and [sh2] agree on a
     suffix of length [min (length sh1) (length s2)], then they are compatible.
     Then, their meet has length [max (length sh1) (length sh2)]. Otherwise,
     [sh1] and [sh2] are incompatible: they contradict each other. Then, their
     meet is bottom: [meet sh1 sh2] is [None]. *)
  val meet: shape -> shape -> shape option

  (**[show_shape sh] prints the sequence of symbols associated with the shape
     [sh]. *)
  val show_shape: shape -> string

  (**[to_list sh] converts the shape [sh] to a list of cells. *)
  val to_list: shape -> cell list

  (* Operations on cells. *)

  (**[symbol cell] is the symbol associated with the cell [cell]. This symbol
     determines the presence and the type of the semantic value stored in this
     cell. It also determines whether a start position and an end position are
     stored in this cell: see {!track_startp} and {!track_endp}. *)
  val symbol: cell -> symbol

  (**[states cell] is the set of the states that might be stored at runtime
     in this cell. The states in this set have the property that either all
     of them are represented, in which case [holds_state] is [true], or none
     of them is represented, in which case [holds_state] is [false]. *)
  val states: cell -> nodes

  (**[holds_semv cell] tells whether a semantic value is stored in the cell
     [cell]. By convention, if [symbol cell] is a nonterminal symbol, then a
     semantic value is stored. (We do not attempt to detect the situation where
     the semantic value could be omitted because it has type [unit], or the
     situation where it could be omitted because it is never used.) If [symbol
     cell] is a terminal symbol, then a semantic value is stored if and only if
     the [%token] declaration was annotated with a type. *)
  val holds_semv: cell -> bool

  (**[holds_state cell] tells whether a state is stored in the cell [cell]. *)
  val holds_state: cell -> bool

  (**[holds_startp cell] tells whether a start position is stored in the cell
     [cell]. It is equivalent to [track_startp (symbol cell)]. *)
  val holds_startp: cell -> bool

  (**[holds_endp cell] tells whether a start position is stored in the cell
     [cell]. It is equivalent to [track_endp (symbol cell)]. *)
  val holds_endp: cell -> bool

  (**[present cell] determines whether at least one of the four fields
     [state], [semv], [startp] or [endp] is present in the cell [cell]. *)
  val present: cell -> bool

  (**[similar] determines whether two stack cells have the same layout in
     memory, that is, the same OCaml type. This is equivalent to comparing
     all fields except [states]. *)
  val similar: cell -> cell -> bool

  (* Miscellaneous operations. *)

  (**[represented node] tells whether the node [node] has an explicit
     representation, that is, whether it is pushed onto the stack. *)
  val represented: node -> bool

  (**[track_startp symbol] tells whether a start position must be recorded
     for the symbol [symbol]. *)
  val track_startp: symbol -> bool

  (**[track_endp symbol] tells whether an end position must be recorded
     for the symbol [symbol]. *)
  val track_endp: symbol -> bool

end (* KINDERGARTEN *)

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Fragments of settings. *)

module type CONSTRUCTION_MODE_SETTINGS = sig

  (**[construction_mode] determines which construction method is used.
     In short,
     - Knuth's canonical LR(1) construction performs no fusion.
       This is the minimal amount of fusion.
     - In "inclusion-only" mode, two states are fused only
       if one is a subset of the other.
     - In "Pager" mode, two states are fused if they compatible
       according to a variant of Pager's weak compatibility criterion.
     - In LALR mode, all states with the same LR(0) core must be fused.
       This is the maximal amount of fusion. *)
  val construction_mode: [`Canonical | `InclusionOnly | `Pager | `LALR]

end

module type NOPREFIX_SETTINGS = sig

  (**If [noprefix] is [false] then most identifiers begin with an unlikely
    prefix, in order to avoid name collisions. If [noprefix] is [true] then
    this prefix is removed. This makes the code more readable but can cause
    name collisions. This flag is not documented. *)
  val noprefix: bool

end

module type REPRESENT_SETTINGS = sig

  (**If [represent_states] is [true] then every stack cell is forced to
     contain a state. If it is [false] then an analysis is performed so as
     determine which stack cells do and do not need to hold a state. *)
  val represent_states: bool

  (**If [represent_positions] is [true] then every stack cell is forced to
     contain a start position and an end position. If it is [false] then an
     analysis is performed so as determine which stack cells do and do not
     need to hold a start position and an end position. *)
  val represent_positions: bool

  (**If [represent_values] is [true] then every stack cell is forced to
     contain a semantic value. If it is [false] then a stack cell that
     corresponds to a terminal symbol whose OCaml type is [unit] does not
     contain a semantic value. *)
  val represent_values: bool

end

module type STRATEGY_SETTINGS = sig

  (**[stragegy] determines which error-handling strategy the interpreter
     should use. For an explanation of the available strategies, see
     [IncrementalEngine]. *)
  val strategy: [`Legacy | `Simplified]

end

module type TRACE_SETTINGS = sig

  (**[trace] determines whether the interpreter should log its actions
     to the standard error channel. *)
  val trace: bool

end
