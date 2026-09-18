(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Printf
open MiddleAPI

module Make (Lr1 : LR1) (X : sig
  include STRATEGY_SETTINGS
  include TRACE_SETTINGS
end) = struct
module Lr0 = Lr1.Lr0
module G = Lr0.G
open G
open X
open CST
let dummy_pos = Lexing.dummy_pos

module StackSymbolsShort =
  StackSymbols.Short(Lr1)()

(* -------------------------------------------------------------------------- *)

(* The signature [MenhirLib.EngineTypes.LOG] defines a set of logging hooks.
   We implement these hooks for use by the reference interpreter. *)

(* Our implementation is parameterized with [count]. *)

module MakeLoggingHooks (X : sig

  (* The reference [count] is used to count the number of messages,
     regardless of whether these messages are visible. Each message
     occupies one line. *)
  val count : int ref

end)
: MenhirLib.EngineTypes.LOG
  with type state := Lr1.node
   and type terminal := Terminal.t
   and type production := Production.t
= struct
  open X

  (* [emit] does nothing if [trace] is false. *)

  let emit format =
    ksprintf (fun s -> if trace then eprintf "%s%!" s) format

  (* [newline] increments [count], and performs the bounds check,
      even if [trace] is false. *)

  let newline() =
    incr count;
    emit "\n"

  let state s =
    emit "State %d:" (Lr1.encode s);
    newline()

  let shift tok s' =
    emit "Shifting (%s) to state %d" (Terminal.print tok) (Lr1.encode s');
    newline()

  let reduce_or_accept prod =
    match Production.test_start prod with
    | Some _ ->
       emit "Accepting";
       newline()
    | None ->
       emit "Reducing production %s" (Production.print prod);
       newline()

  let lookahead_token tok startp endp =
    emit "Lookahead token is now %s (%d-%d)"
      (Terminal.print tok)
      startp.Lexing.pos_cnum
      endp.Lexing.pos_cnum;
    newline()

  let initiating_error_handling () =
    emit "Initiating error handling";
    newline()

  let resuming_error_handling () =
    emit "Resuming error handling";
    newline()

  let handling_error s =
    emit "Handling error in state %d" (Lr1.encode s);
    newline()

end

(* -------------------------------------------------------------------------- *)

(* Set up all of the information required by the LR engine. *)

module T = struct

  type state =
      Lr1.node

  let number =
    Lr1.encode

  type token =
      Terminal.t

  type terminal =
      Terminal.t

  type nonterminal =
      Nonterminal.t

  type semantic_value =
      cst

  let token2terminal (token : token) : terminal =
    token

  let token2value (token : token) : semantic_value =
    CstTerminal token

  let error_terminal =
    Terminal.error

  let error_value =
    CstError

  let foreach_terminal =
    Terminal.foldx

  type production =
      Production.t

  let production_index = Production.encode
  let find_production  = Production.decode

  let default_reduction (s : state) defred nodefred env =
    match Lr1.test_default_reduction s with
    | Some (prod, _) ->
        defred env prod
    | None ->
        nodefred env

  let action (s : state) (tok : terminal) value shift reduce fail env =

    (* Check whether [s] has an outgoing shift transition along [tok]. *)

    match SymbolMap.find (Symbol.T tok) (Lr1.transitions s) with

    | (s' : state) ->

        (* There is such a transition. Return either [ShiftDiscard] or
           [ShiftNoDiscard], depending on the existence of a default
           reduction on [#] at [s']. *)

        let discard =
          match Lr1.test_default_reduction s' with
          | Some (_, toks) when TerminalSet.mem Terminal.sharp toks ->
              false
          | _ ->
              true
        in
        shift env discard tok value s'

    (* There is no such transition. Look for a reduction. *)

    | exception Not_found ->

        match TerminalMap.find tok (Lr1.reductions s) with
        | prods ->
            (* There is a reduction. *)
            let prod = MList.single prods in
            reduce env prod
        | exception Not_found ->
            (* There is no transition and no reduction. Fail. *)
            fail env

  let maybe_shift_t (s : state) (tok : terminal) : state option =
    SymbolMap.find_opt (Symbol.T tok) (Lr1.transitions s)

  let may_reduce_prod (s : state) (tok : terminal) (prod : production) =
    match Lr1.test_default_reduction s with
    | Some (prod', _) ->
        prod = prod'
    | None ->
        try
          let prod' = MList.single (TerminalMap.find tok (Lr1.reductions s)) in
          prod = prod'
        with Not_found ->
          false

  let goto_nt (s : state) (nt : nonterminal) : state =
    try
      SymbolMap.find (Symbol.N nt) (Lr1.transitions s)
    with Not_found ->
      assert false

  let goto_prod (s : state) (prod : production) : state =
    goto_nt s (Production.nt prod)

  let maybe_goto_nt (s : state) (nt : nonterminal) : state option =
    SymbolMap.find_opt (Symbol.N nt) (Lr1.transitions s)

  open MenhirLib.EngineTypes

  exception Error
  exception Abort

  (* By convention, a semantic action returns a new stack. It does not
     affect [env]. *)

  let lhs =
    Production.nt

  let is_start =
    Production.is_start

  type semantic_action =
      (state, semantic_value, token) env -> (state, semantic_value) stack

  let semantic_action (prod : production) : semantic_action =
    fun env ->
      assert (not (Production.is_start prod));

      (* Reduce. Pop a suffix of the stack, and use it to construct a
         new concrete syntax tree node. *)

      let n = Production.length prod in

      let values : semantic_value array =
        Array.make n CstError (* dummy *)
      and startp =
        ref dummy_pos
      and endp =
        ref dummy_pos
      and current =
        ref env.current
      and stack =
        ref env.stack
      in

      (* We now enter a loop to pop [k] stack cells and (after that) push
         a new cell onto the stack. *)

      (* This loop does not update [env.current]. Instead, the state in
         the newly pushed stack cell will be used (by our caller) as a
         basis for a goto transition, and [env.current] will be updated
         (if necessary) then. *)

      for k = n downto 1 do

        (* Fetch a semantic value. *)

        values.(k - 1) <- !stack.semv;

        (* Pop one cell. The stack must be non-empty. As we pop a cell,
           change the automaton's current state to the one stored within
           the cell. (It is sufficient to do this only when [k] is 1,
           since the last write overwrites any and all previous writes.)
           If this is the first (last) cell that we pop, update [endp]
           ([startp]). *)

        let next = !stack.next in
        assert (!stack != next);
        if k = n then begin
          endp := !stack.endp
        end;
        if k = 1 then begin
          current := !stack.state;
          startp := !stack.startp
        end;
        stack := next

      done;

      (* Done popping. *)

      (* As a special case, under the simplified strategy, if the production
         that we are reducing involves the [error] token, then we assume that
         the semantic action aborts the parser. We might otherwise enter an
         infinite loop. *)

      if strategy = `Simplified
      && not (Production.error_free prod) then
        raise Abort;

      (* Construct and push a new stack cell. The associated semantic
         value is a new concrete syntax tree. *)

      {
        state = !current;
        semv = CstNonTerminal (prod, values);
        startp = !startp;
        endp = !endp;
        next = !stack
      }

  let may_reduce node prod =
    Lr1.NodeSet.mem node (Lr1.reduction_sites prod)

end

(* -------------------------------------------------------------------------- *)

(* This is the interpreter's main entry point. *)

let interpret count nt lexer lexbuf =

  (* Instantiate the LR engine. *)

  let module E =
    MenhirLib.Engine.Make (struct
      include T
      (* The logging hooks. *)
      module Log = MakeLoggingHooks(struct
        let count = count
      end)
      (* We want our logging hooks to be called. *)
      let log = true
    end)
  in

  (* Run it. *)

  try
    let cst = E.entry strategy (Lr1.start_node nt) lexer lexbuf in
    Some cst
  with T.Error | T.Abort ->
    None

(* -------------------------------------------------------------------------- *)

(* This is a second entry point. It is used internally by [LRijkstra]
   to check that the sentences that [LRijkstra] produces do lead to an
   error in the expected state. *)

type spurious_reduction =
  Lr1.node * Production.t

type target =
  Lr1.node * spurious_reduction list

type check_error_path_outcome =
  (* Bad: the input was read past its end. *)
| OInputReadPastEnd
  (* Bad: a syntax error occurred before all of the input was read. *)
| OInputNotFullyConsumed
  (* Bad: the parser unexpectedly accepted (part of) this input. *)
| OUnexpectedAccept
  (* Good: a syntax error occurred after reading the last input token. We
     report in which state the error took place, as well as a list of spurious
     reductions. A non-default reduction that takes place after looking at the
     last input token (i.e., the erroneous token) is spurious. Furthermore, any
     reduction that takes place after a spurious reduction is itself spurious.
     We note that a spurious reduction can take place only in a non-canonical
     LR automaton. *)
| OK of target

let check_error_path count nt input =

  (* Instantiate the LR engine. *)

  let module E =
    MenhirLib.Engine.Make (struct
      include T
      (* The logging hooks. *)
      module Log = MakeLoggingHooks(struct
        let count = count
      end)
      (* We want our logging hooks to be called. *)
      let log = true
    end)
  in

  (* Determine the initial state. *)

  let entry = Lr1.start_node nt in

  (* This function helps extract the current parser state out of [env].
     It may become unnecessary if the [Engine] API offers it. *)

  let current env =
    (* Peek at the stack. If empty, then we must be in the initial state. *)
    match E.top env with
    | None ->
        entry
    | Some (E.Element (s, _, _, _)) ->
        s
  in

  (* Set up a function that delivers tokens one by one. *)

  let input = ref input in
  let next () =
    match !input with
    | [] ->
        None
    | t :: ts ->
        input := ts;
        Some t
  in

  let looking_at_last_token () : bool =
    !input = []
  in

  (* Run. We wish to stop at the first error (without handling the error
     in any way) and report in which state the error occurred. A clean way
     of doing this is to use the incremental API, as follows. The main loop
     resembles the [loop] function in [Engine]. *)

  (* Another reason why we write our own loop is that we wish to detect
     spurious reductions. We accumulate these reductions in [spurious], a
     (reversed) list of productions. *)

  let rec loop (checkpoint : cst E.checkpoint) (spurious : spurious_reduction list) =
    match checkpoint with
    | E.InputNeeded _ ->
        begin match next() with
        | None ->
            OInputReadPastEnd
        | Some t ->
            let checkpoint = E.offer checkpoint (t, dummy_pos, dummy_pos) in
            loop checkpoint spurious
        end
    | E.Shifting _ ->
        loop (E.resume ~strategy checkpoint) spurious
    | E.AboutToReduce (env, prod) ->
        (* If we have requested the last input token and if this is not
           a default reduction, then this is a spurious reduction.
           Furthermore, if a spurious reduction has taken place already,
           then this is also a spurious reduction. *)
        let spurious =
          if looking_at_last_token() && not (E.env_has_default_reduction env)
          || spurious <> []
          then
            (current env, prod) :: spurious
          else
            spurious
        in
        loop (E.resume ~strategy checkpoint) spurious
    | E.HandlingError env ->
        (* Check that all of the input has been read. Otherwise, the error
           has occurred sooner than expected. *)
        if !input = [] then
          (* Return the current state and the list of spurious reductions. *)
          OK (current env, List.rev spurious)
        else
          OInputNotFullyConsumed
    | E.Accepted _ ->
        (* The parser has succeeded. This is unexpected. *)
        OUnexpectedAccept
    | E.Rejected ->
        (* The parser rejects this input. This should not happen; we
           should observe [HandlingError _] first. *)
        assert false
  in

  loop (E.start entry dummy_pos) []

(* -------------------------------------------------------------------------- *)

(* [interpret_error_sentence] interprets a sentence, expecting it to end
   in an error. Failure or success is reported via two continuations. *)

let interpret_error_sentence sentence fail succeed =
  let nt = Sentence.start sentence
  and (_, terminals) = sentence in
  let count = ref 0 (* unused *) in
  match check_error_path count nt terminals with
  | OInputReadPastEnd ->
      fail "no syntax error occurs."
  | OInputNotFullyConsumed ->
      fail "a syntax error occurs before the last token is reached."
  | OUnexpectedAccept ->
      fail "no syntax error occurs; in fact, this input is accepted."
  | OK target ->
      succeed target

(* -------------------------------------------------------------------------- *)

(* This default error message is produced by [--list-errors] when it creates a
   [.messages] file, and is recognized by [--compare-errors] when it compares
   two such files. *)

let default_message =
  "<YOUR SYNTAX ERROR MESSAGE HERE>\n"

(* -------------------------------------------------------------------------- *)

(* [print_messages_auto (sentence, target)] displays the sentence [sentence],
   leading to the state [target]. It then displays a bunch of auto-generated
   comments. *)

let print_messages_auto (sentence, target) =

  (* Print the sentence. *)
  print_string (Sentence.print `Abstract sentence);

  (* If a token alias has been defined for every terminal symbol, then
     we can convert this sentence into concrete syntax. Do so. We make
     a few assumptions about the concrete syntax of the language:
       1. It is permitted to insert one space between two tokens;
       2. No token contains a newline character.
          (Our lexer enforces this assumption.)
     The name of the start symbol cannot be printed in a meaningful
     manner, so it is omitted. *)
  if Terminal.every_token_has_an_alias then
    printf
      "##\n\
       ## Concrete syntax: %s\n"
      (Sentence.print `Concrete sentence)
  ;

  (* Show which state this sentence leads to. *)
  let (s', spurious) = target in
  printf
    "##\n\
     ## Ends in an error in state: %d.\n\
     ##\n\
     %s##\n"
    (Lr1.encode s')
    (* [Lr0.ALR1.print] prints just the state's kernel, not its closure.
       Displaying the closure could be helpful, but is usually intolerably
       verbose. *)
    (Lr0.ALR1.print "## " (Lr1.state s'))
  ;

  (* Show the known suffix of the stack in this state. *)
  printf
    "## The known suffix of the stack is as follows:\n\
     ##%s\n\
     ##\n"
    (Symbol.print_array' false (StackSymbolsShort.node_shape s'))
  ;

  (* If interpreting this sentence causes spurious reductions (that is,
     reductions that take place after the last terminal symbol has been
     shifted), say so, and show them. *)
  if spurious <> [] then begin
    printf
      "## WARNING: This example involves spurious reductions.\n\
       ## This implies that, although the LR(1) items shown above provide an\n\
       ## accurate view of the past (what has been recognized so far), they\n\
       ## may provide an INCOMPLETE view of the future (what was expected next).\n"
    ;
    List.iter (fun (s, prod) ->
      printf
        "## In state %d, spurious reduction of production %s\n"
        (Lr1.encode s)
        (Production.print prod)
    ) spurious;
    printf "##\n"
  end

(* -------------------------------------------------------------------------- *)

(* [print_messages_item] displays one entry in the [.messages] file format. *)

let print_messages_item (sentence, target) =
  (* Print the sentence, followed with auto-generated comments. *)
  print_messages_auto (sentence, target);
  (* Then, print a proposed error message, between two blank lines. *)
  printf "\n%s\n" default_message

(* -------------------------------------------------------------------------- *)

end (* Make *)
