(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open IL
open ILConstruction
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
= struct
  open G

  module TokenType =
    TokenType.Make(X)

  let tokpat tok pat =
    let data = TokenType.tokendata (Terminal.print tok) in
    PData (
      data,
      if Terminal.ocamltype tok = None then [] else [ pat ]
    )

  let tok_bind_unit tok pat e =
    if Terminal.ocamltype tok = None then
      blet [ (pat, EUnit) ] e
    else
      e

  let tokspat toks =
    POr (
      TerminalSet.fold (fun tok pats ->
        tokpat tok PWildcard :: pats
      ) toks []
    )

  let tokexpr tok e =
    let data = TokenType.tokendata (Terminal.print tok) in
    EData (
      data,
      if Terminal.ocamltype tok = None then [] else [ e ]
    )

end
