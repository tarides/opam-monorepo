(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* This module lets us test the GLR interpreter. The GLR "reference
   interpreter" is the candidate. If the automaton is deterministic,
   then we can use the LR reference interpreter as the reference.
   Otherwise, we do not have a reference. *)

open Printf
let fail format =
  kfprintf (fun _ -> exit 1) stderr format

open Grammar

let deterministic =
  Lr1.deterministic()

module LR =
  ReferenceInterpreter.Make(Lr1)(struct
    let strategy = `Simplified
    let trace = false
  end)

module GLR =
  ReferenceInterpreterGLR.Make(Lr1)

let verbose =
  false

let vprintf format =
  if verbose then
    eprintf format
  else
    ikfprintf (fun _ -> ()) stderr format

(* -------------------------------------------------------------------------- *)

(* The possible outcomes of an execution. *)

type outcome =
  | Accepted of CST.cst
  | Rejected
  | Overshoot
  | Crash of exn * string (* backtrace *)
  | ShouldBeAccepted
      (* Used when there is no reference implementation,
         so we know that the input should be accepted,
         but we do not have a CST. *)

let print_outcome candidate = function
  | Accepted _ ->
      sprintf "%s accepts this sentence" candidate
  | Rejected ->
      sprintf "%s rejects this sentence" candidate
  | Overshoot ->
      sprintf "%s reads past the end of the input stream" candidate
  | Crash (e, _backtrace) ->
      sprintf "%s fails (%s)" candidate (Printexc.to_string e)
  | ShouldBeAccepted ->
      "this sentence should be accepted"

let extract_backtrace o =
  match o with
  | Crash (_e, backtrace) ->
      backtrace
  | _ ->
      ""

(* -------------------------------------------------------------------------- *)

(* [run parser nt sentence] runs the parser [parser]
   with the start symbol [nt] and the sentence [sentence]. *)

let run parser nt sentence : outcome =
  let module T = TerminalStream.Make(Grammar) in
  let lexer, lexbuf = T.stream sentence in
  match parser nt lexer lexbuf with
  | Some cst ->
      Accepted cst
  | None ->
      Rejected
  | exception T.EndOfStream ->
      Overshoot
  | exception e ->
      Crash (e, Printexc.get_backtrace())

(* [run_candidate nt sentence] runs the GLR parser
   with the start symbol [nt] and the sentence [sentence]. *)

let run_candidate nt sentence : outcome =
  run GLR.interpret nt sentence

(* [run_reference nt sentence] runs the LR parser
   with the start symbol [nt] and the sentence [sentence]. *)

let run_reference nt sentence : outcome =
  let count = ref 0 in
  run (LR.interpret count) nt sentence

(* [run nt sentence expected] runs the GLR parser
   with the start symbol [nt] and the sentence [sentence].
   and compares its outcome with the outcome [expected]. *)

let run nt sentence (expected : outcome) : unit =
  (* Show the sentence that we are about to test. *)
  vprintf "TestGLR: About to test this sentence:\n  %s%!"
    (Sentence.print `Abstract (Some nt, sentence));
  (* Run the candidate. *)
  let actual : outcome = run_candidate nt sentence in
  (* Check that the outcomes match. *)
  match actual, expected with
  | Accepted _, ShouldBeAccepted ->
      ()
  | Accepted cst, Accepted cst' ->
      if cst <> cst' then
        fail "TestGLR: got an unexpected CST.\nExpected:\n%a\nGot:\n%a\n"
          CST.show cst' CST.show cst
  | Rejected, Rejected
  | Overshoot, Overshoot ->
      ()
  | _, Crash _ ->
      (* This should not happen. *)
      assert false
  | ShouldBeAccepted, _ ->
      (* This cannot happen. *)
      assert false
  | _, _ ->
      fail "TestGLR: unexpected outcome:\n\
            %s,\n\
            but %s.\n%s%!"
        (print_outcome "the GLR parser" actual)
        (print_outcome "the LR parser" expected)
        (extract_backtrace actual)

(* [run nt sentence] runs the GLR parser with the start symbol [nt] and the
   sentence [sentence]. If [deterministic] is true then the LR parser is used
   to predict an expected outcome; otherwise the expected outcome is that the
   sentence should be accepted. *)

let run nt sentence : unit =
  let expected : outcome =
    if deterministic then
      run_reference nt sentence
    else
      ShouldBeAccepted
      (* In this case, we might want to first run the LR parser to obtain one
         CST, and check that this CST appears as one alternative among the set
         of CSTs produced by the GLR parser. *)
  in
  run nt sentence expected

(* -------------------------------------------------------------------------- *)

(* [test nt] tests the GLR parser with the start symbol [nt] and with a number
   of sentences. *)

(* We sample sentences of increasing sizes, picking at most [k] sentences of
   each size, until a certain size threshold is reached. *)

let k         = 2         (* number of repetitions at each size *)
let size      = 10                              (* initial size *)
let threshold = if deterministic then 500 else 50 (* final size *)

let test nt =
  vprintf
    "TestGLR: about to test start symbol %s...\n%!"
    (Nonterminal.print false nt);
  (* Sample sentences of increasing sizes, picking at most [k] sentences
     of each size, until a total of [n] sentences is reached or the size
     threshold is reached. *)
  let count, size = ref 0, ref size in
  let prev = ref !size in
  let total = ref 0 in
  while !size < threshold do
    for _ = 1 to k do
      let module R = RandomSentenceGenerator.Make(Grammar) in
      let sentence = R.nonterminal nt !size in
      run nt sentence;
      total := !total + List.length sentence;
    done;
    count := !count + k;
    prev := !size;
    size := !size + !size / 10
  done;
  (* Log a success message. *)
  if verbose then
    eprintf
      "TestGLR: Tested %d sentences of length up to %d.\n"
      !count !prev

(* -------------------------------------------------------------------------- *)

(* [test()] tests the GLR parser. *)

let test () =
  Time.time "Testing the GLR parser" @@ fun () ->
  (* For each start symbol [nt], test this entry point. *)
  Lr1.entry |> ProductionMap.iter @@ fun _prod s ->
  let nt = Lr1.get_start s in
  test nt

let test () =
  (* Record a backtrace even if OCAMLRUNPARAM does not say so. *)
  let status = Printexc.backtrace_status() in
  Printexc.record_backtrace true;
  test();
  Printexc.record_backtrace status

(* -------------------------------------------------------------------------- *)

let[@inline] now () =
  let open Unix in
  (times()).tms_utime

(* -------------------------------------------------------------------------- *)

(* [benchmark nt] benchmarks the GLR parser with the start symbol [nt]
   and with a number of sentences. *)

let k         = 2        (* number of repetitions at each size *)
let size      = 200      (* initial size *)
let threshold = if deterministic then 20000 else 100 (* final size *)

let benchmark nt =
  eprintf "size,speed,type\n%!";
  let module R = RandomSentenceGenerator.Make(Grammar) in
  assert (not verbose);
  let size = ref size in
  while !size < threshold do
    (* Generate [k] sentences. *)
    let sentences = List.init k @@ fun _ -> R.nonterminal nt !size in
    (* Measure their total size in tokens. *)
    let tokens = MList.sum (List.map List.length sentences) in
    let average_size = float tokens /. float k in
    (* Speeds will be expressed in tokens per microsecond. *)
    let speed time = 1.0e-6 *. float tokens /. time in
    (* Measure the time needed by the GLR parser on these sentences. *)
    let start = now() in
    List.iter (fun sentence -> ignore (run_candidate nt sentence)) sentences;
    let time = now() -. start in
    (* Emit data. *)
    eprintf "%.9f,%.9f,GLR\n%!" average_size (speed time);
    (* Measure the time needed by the LR parser on these sentences. *)
    if deterministic then begin
      let start = now() in
      List.iter (fun sentence -> ignore (run_reference nt sentence)) sentences;
      let time = now() -. start in
      eprintf "%.9f,%.9f,LR\n%!" average_size (speed time)
    end;
    (* Continue. *)
    size := !size + !size / 10;
  done

let benchmark () =
  (* For each start symbol [nt], benchmark this entry point. *)
  Lr1.entry |> ProductionMap.iter @@ fun _prod s ->
  let nt = Lr1.get_start s in
  benchmark nt
