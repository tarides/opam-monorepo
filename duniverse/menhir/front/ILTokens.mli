(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module helps generate IL code that deals with tokens. *)

open IL
open FrontAPI

module Make
(G : sig
  module Terminal : sig
    type t
    val print: t -> string
    val ocamltype: t -> ocamltype option
  end
  module TerminalSet : sig
    type elt = Terminal.t
    type t
    val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
  end
end)
(X : TOKEN_TYPE_SETTINGS)
: sig
  open G

  (**[tokpat t pat] is a pattern that matches a token for the terminal symbol
     [t] and binds its semantic value (if it has one) to the pattern [pat]. *)
  val tokpat: Terminal.t -> pattern -> pattern

  (**[tokexpr t e] is an expression that constructs a token for the terminal
     symbol [t], taking its semantic value (if it needs one) from the
     expression [e]. *)
  val tokexpr: Terminal.t -> expr -> expr

  (**[tokspat ts] is a pattern that matches a token for any terminal symbol in
     the set [ts]. The semantic value is not bound. *)
  val tokspat: TerminalSet.t -> pattern

  (**If the terminal symbol [t] has no semantic value then [tok_bind_unit t
     pat e] binds the pattern [pat] to the unit value in the expression [e] .
     If [t] has a semantic value then it expands to just [e]. *)
  val tok_bind_unit: Terminal.t -> pattern -> expr -> expr

end
