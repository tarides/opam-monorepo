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

module Make
(X : sig
  val grammar: grammar
end) ()
= struct
open X

(* -------------------------------------------------------------------------- *)

(* We need to know which symbols generate the empty language. *)

module Nonempty =
  Nonempty.Make(X)

(* -------------------------------------------------------------------------- *)

(* Transform the grammar. All empty symbols are removed.
   In the remaining definitions, all empty branches are removed. *)

let transform_branches (branches : branches) : branches =
  List.filter Nonempty.production branches

let transform_rule (rule : rule) : rule =
  { rule with branches = transform_branches rule.branches }

module M =
  StringMap

let rules =
  M.fold (fun nt rule rules ->
    if Nonempty.symbol nt then
      (* If N is nonempty, transform its definition. *)
      rules
      |> M.add nt (transform_rule rule)
    else
      (* If N is empty, throw away its definition. *)
      rules
  ) grammar.rules M.empty

let keep nt _ =
  Nonempty.symbol nt

let types =
  M.filter keep grammar.types

let on_error_reduce =
  M.filter keep grammar.on_error_reduce

let grammar =
  { grammar with rules; types; on_error_reduce }

(* If a start symbol generates the empty language, fail. *)

let () =
  grammar.start_symbols |> StringSet.iter @@ fun nt ->
  if not (Nonempty.symbol nt) then
    Report.Just.error []
      "The start symbol %s generates the empty language."
      nt

end (* Make *)

(* -------------------------------------------------------------------------- *)

(* Re-package [Make] as a function. *)

let transform grammar =
  let module M = Make(struct
    let grammar = grammar
  end)() in
  M.grammar
