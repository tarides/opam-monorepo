(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(**This signature defines the format of the tables that are produced (in
   addition to the tables described in [TableFormat]) when the command line
   switch [--inspection] is enabled. It is used as an argument to
   {!InspectionTableInterpreter.Make}. *)
module type TABLES = sig

  (* The types of symbols. *)
  include IncrementalEngine.SYMBOLS

  (**The type ['a lr1state] describes an LR(1) state. The generated parser
     defines it internally as [int]. *)
  type 'a lr1state

  (**Some of the tables that follow use encodings of (terminal and
     nonterminal) symbols as integers. So, we need functions that
     map the integer encoding of a symbol to its algebraic encoding. *)

  (**[terminal] maps an integer code for a terminal symbol to a (terminal)
     symbol. *)
  val    terminal: int -> xsymbol

  (**[nonterminal] maps an integer code for a nonterminal symbol to a
     (nonterminal) symbol. *)
  val nonterminal: int -> xsymbol

  (**The left-hand side of every production already appears in the
     signature [TableFormat.TABLES], so we need not repeat it here. *)

  (**The table [rhs] provides access to the right-hand side of every
     production. The encoding of symbols as integers in described in
     [TableBackend]. *)
  val rhs: int -> int list

  (**A mapping of every (non-initial) state to its LR(0) core. *)
  val lr0_core: int -> int

  (**A mapping of every LR(0) state to its set of LR(0) items. Each item is
     represented in its packed form (see [Item]) as an integer. *)
  val lr0_items: int -> int list

  (**A mapping of every LR(0) state to its incoming symbol, if it has one. *)
  val lr0_incoming: int -> int

  (**A table that tells which non-terminal symbols are nullable. *)
  val nullable: int -> int (* 0 or 1 *)

  (**A two-table dimensional table, indexed by a nonterminal symbol and
     by a terminal symbol (other than [#]), encodes the FIRST sets. *)
  val first: int -> int -> int (* 0 or 1 *)

end
