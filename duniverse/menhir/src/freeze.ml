(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

let () =
  if Settings.automaton_graph then
    let module A = AutomatonGraph.Make(Lr1) in
    A.print (Settings.base ^ ".dot")

(* Handle the command line options [--interpret], [--interpret-error],
   [--compile-errors], [--compare-errors], etc. *)

let () =
  match Settings.backend with
  | `Interpret show ->
      let module I = Interpret.Make(Lr1)(Settings) in
      I.interpret show
  | `InterpretError ->
      let module I = Interpret.Make(Lr1)(Settings) in
      I.interpret_error()
  | `InterpretGLR show ->
      let module I = InterpretGLR.Make(Lr1) in
      I.interpret show
  | _ ->
      ()

module Messages =
  Messages.Make(Lr1)(Settings)

let () =
  Settings.compile_errors |> Option.iter @@ fun filename ->
  Messages.compile_errors filename;
  exit 0

let () =
  Settings.compare_errors |> Option.iter @@ fun (filename1, filename2) ->
  Messages.compare_errors filename1 filename2;
  exit 0

let () =
  Settings.merge_errors |> Option.iter @@ fun (filename1, filename2) ->
  Messages.merge_errors filename1 filename2;
  exit 0

let () =
  Settings.update_errors |> Option.iter @@ fun filename ->
  Messages.update_errors filename;
  exit 0

let () =
  Settings.echo_errors |> Option.iter @@ fun filename ->
  Messages.echo_errors false filename;
  exit 0

let () =
  Settings.echo_errors_concrete |> Option.iter @@ fun filename ->
  Messages.echo_errors true filename;
  exit 0

(* If [--list-errors] is set, produce a list of erroneous input sentences,
   then stop. *)

let () =
  if Settings.list_errors then
    let module X = struct
      (* Undocumented: if [--log-automaton 2] is set, be verbose. *)
      let verbose = !Channels.logA >= 2
      (* For my own purposes, LRijkstra can print one line of statistics to a .csv file. *)
      let statistics = if false then Some "lr.csv" else None
    end in
    let module L = struct
      include LRijkstra.Run(X)()
    end in
    exit 0

(* If requested, generate a .cmly file. *)

let () =
  if Settings.cmly then
    Cmly_write.write (Settings.base ^ ".cmly")

(* If no code generation was requested (i.e., no back-end was selected)
   then exit at this point. *)

let () =
  if Settings.backend = `NoBackend then
    exit 0

(* If a test of the GLR interpreter has been requested, run it now (and stop). *)

(* Note: [TestGLR.benchmark] is currently unused. *)

let () =
  if Settings.test_GLR then (TestGLR.test(); exit 0)

let force () =
  ()
