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
  (* A terminal symbol is not nullable. *)
  let terminal _ = false
  (* An alternative is nullable if at least one branch is nullable. *)
  let disjunction p q = p || q()
  (* A sequence is nullable if both members are nullable. *)
  let conjunction _ p q = p && q()
  (* The sequence epsilon is nullable. *)
  let epsilon = true
end

module Make (G : sig val grammar: grammar end) =
  PlainSyntaxAnalysis.Make(Fix.Prop.Boolean)(G)(A)
