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

module A = struct
  type property = bool
  let shortcut _nt = None
  (* A terminal symbol generates a non-epsilon word. *)
  let terminal _ = true
  (* An alternative generates a non-epsilon word if one of the
     branches generates a non-epsilon word. *)
  let disjunction p q = p || q()
  (* A sequence generates a non-epsilon word if one the components
     generates a non-epsilon word. (We assume that both components
     are non-void. Otherwise we obtain an over-approximation.) *)
  let conjunction _ p q = p || q()
  (* The empty sequence does not generate a non-epsilon word. *)
  let epsilon = false
end

module Make (G : sig val grammar: grammar end) =
  PlainSyntaxAnalysis.Make(Fix.Prop.Boolean)(G)(A)
