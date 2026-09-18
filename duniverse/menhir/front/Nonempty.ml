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
  (* A terminal symbol is nonempty. *)
  let terminal _ = true
  (* An alternative is nonempty if at least one branch is nonempty. *)
  let disjunction p q = p || q()
  (* A sequence is nonempty if both members are nonempty. *)
  let conjunction _ p q = p && q()
  (* The sequence epsilon is nonempty. It generates the singleton
     language {epsilon}. *)
  let epsilon = true
end

module Make (G : sig val grammar: grammar end) =
  PlainSyntaxAnalysis.Make(Fix.Prop.Boolean)(G)(A)
