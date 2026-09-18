(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* This check must be performed only after %inline symbols and nullable
   symbols have been expanded away. *)

let warning = Report.warning
let position = Located.position
open PlainSyntax

(* -------------------------------------------------------------------------- *)

let every_rule_has_merge_fun grammar =
  StringMap.fold (fun _nt rule accu ->
    accu && rule.merge <> None
  ) grammar.rules true

(* -------------------------------------------------------------------------- *)

module Run (X : sig
  open Report
  val main: channel
  val active: bool
  val grammar : grammar
end) = struct
open X

(* -------------------------------------------------------------------------- *)

(* When not in GLR mode, warn about %merge functions. *)

let () =
  if not active then
    grammar.rules |> StringMap.iter @@ fun _nt rule ->
    if rule.merge <> None then
      warning main rule.positions
        "merge functions are useless unless the GLR back-end is selected."

(* -------------------------------------------------------------------------- *)

(* When not in GLR mode, warn about default merge functions. *)

let () =
  if not active then
    if grammar.default_merge <> None then
      let pos = grammar.default_merge |> Option.get |> position in
      warning main [pos]
        "merge functions are useless unless the GLR back-end is selected."

(* -------------------------------------------------------------------------- *)

(* When in GLR mode, a default %merge function should be provided if and only
   if it is not true that every nonterminal symbol has a %merge function. *)

let () =
  if active then
    match every_rule_has_merge_fun grammar, grammar.default_merge with
    | true, None
    | false, Some _ ->
        ()
    | true, Some mfl ->
        warning main [position mfl]
          "because every rule has a merge function,\n\
           this default merge function is unnecessary."
    | false, None ->
        warning main []
          "because not every rule has a merge function,\n\
           a default merge function should be provided."

(* -------------------------------------------------------------------------- *)

let finished =
  ()

end (* Run *)

let check active main grammar =
  let module R = Run(struct
    let active = active
    let main = main
    let grammar = grammar
  end) in
  R.finished
