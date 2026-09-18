(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

let error, warning = Report.(error, warning)
open PlainSyntax

module Run (G : sig
  open Report
  val main: channel
  val grammar : grammar
end) = struct
open G

(* Check that at least one start symbol has been declared: otherwise, the
   resulting grammar would be empty. This check cannot be performed earlier
   (e.g., as part of CheckGrammar) because, when [--only-tokens] is used,
   we must tolerate the absence of a start symbol. *)

let () =
  if StringSet.is_empty grammar.start_symbols then
    error main [] "no start symbol has been declared."

let rec visit_symbol visited symbol =
  match StringMap.find symbol grammar.rules with
  | exception Not_found ->
      (* This must be a terminal symbol. *)
      visited
  | rule ->
      if StringSet.mem symbol visited then
        visited
      else
        let visited = StringSet.add symbol visited in
        List.fold_left visit_branch visited rule.branches

and visit_branch visited branch =
  List.fold_left visit_producer visited branch.producers

and visit_producer visited producer =
  visit_symbol visited producer.prod_symbol

(* Compute the reachable symbols. *)

let reachable =
  StringSet.fold (fun symbol visited ->
    visit_symbol visited symbol
  ) grammar.start_symbols StringSet.empty

let roots =
  if StringSet.cardinal grammar.start_symbols = 1 then
    "the start symbol"
  else
    "any of the start symbols"

(* Warn about each unreachable symbol. *)

let () =
  grammar.rules |> StringMap.iter @@ fun symbol rule ->
  if not (StringSet.mem symbol reachable) then
    warning main rule.positions
      "symbol %s is unreachable from %s."
      symbol roots

(* Construct a trimmed grammar. *)

let grammar =
  { grammar with
    rules = StringMap.restrict reachable grammar.rules;
    types = StringMap.restrict reachable grammar.types;
    on_error_reduce = StringMap.restrict reachable grammar.on_error_reduce }

end (* Run *)

let trim main grammar =
  let module R = Run(struct
    let main = main
    let grammar = grammar
  end) in
  R.grammar
