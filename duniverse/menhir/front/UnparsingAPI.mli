(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module generates the code and the signature of the unparsing API. *)

open IL
open PlainSyntax
open FrontAPI

module Code (G : sig
  module Terminal : sig
    type t
    val encode: t -> int
    val print: t -> string
    val sharp: t
    val real: t -> bool
    val unquoted_alias: t -> string option
    val ocamltype: t -> ocamltype option
    val map_real: (t -> 'a) -> 'a list
  end
  module TerminalSet : sig
    type elt = Terminal.t
    type t
    val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
  end
  module Nonterminal : sig
    type t
    val print: bool -> t -> string
    val mapx: (t -> 'a) -> 'a list
  end
  module Symbol : sig
    type t =
      | N of Nonterminal.t
      | T of Terminal.t
  end
  module Production : sig
    type t
    val encode: t -> int
    val print: t -> string
    val rhs: t -> Symbol.t array
    val positions: t -> ranges
    val attributes: t -> attributes
    val error_free: t -> bool
    val mapnt:  Nonterminal.t -> (t -> 'a) -> 'a list
    val mapx: (t -> 'a) -> 'a list
  end
end)
(A : sig

  (**[entry] is a list of the start symbols and corresponding start states. *)
  val entry: (G.Nonterminal.t * int) list

end)
(N : sig

  (**[tables] is the name of the module that contains the parse tables. *)
  val tables: string

end)
(X : TOKEN_TYPE_SETTINGS)
: sig

  (**[unparsing_API] is the code of the unparsing API. *)
  val unparsing_API : unit -> structure

end

(**[unparsing_API] is the interface of the parsing API. *)
val unparsing_API : grammar -> interface
