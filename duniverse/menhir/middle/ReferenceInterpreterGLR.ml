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
open MenhirGLR

(* The [error] token is not supported, so is there no [strategy] setting. *)

(* There is no [trace] setting. *)

let verbose =
  false

let vprintf format =
  if verbose then
    eprintf format
  else
    ikfprintf (fun _ -> ()) stderr format

module Make (Lr1 : LR1) = struct
module Lr0 = Lr1.Lr0
module G = Lr0.G
open G

(* -------------------------------------------------------------------------- *)

(* Set up a way of accessing the input stream. *)

(* -------------------------------------------------------------------------- *)

(* Set up all of the information required by the GLR engine. *)

module Data = struct

  type state = int
  type nonterminal = Nonterminal.t
  type production = int
  type token = Terminal.t
  type semv = CST.cst

  exception Error of (state, semv) GSS.node list

  module Semv = struct
    let token2value t =
      CST.CstTerminal t
    let merge nt v1 v2 _input _date1 _date2 =
      CST.disj nt v1 v2
  end

  module Production = struct

    let encode, decode = Production.(encode, decode)

    let start =
      Production.start

    let length prod =
      Production.length (decode prod)

    let lhs prod =
      Production.nt (decode prod)

    let print prod =
      Production.print (decode prod)

    let action prod _input path =
      let prod = decode prod in
      assert (not (Production.is_start prod));
      let n = Production.length prod in
      let dummy = CST.CstError in
      let semvs = Array.make n dummy in
      let p = ref path in
      for i = 0 to n-1 do
        match !p with
        | Path.Empty _ -> assert false
        | Path.Edge (edge, path) ->
        semvs.(i) <- edge.semv;
        p := path;
      done;
      assert (match !p with Path.Empty _ -> true | Path.Edge _ -> false);
      CST.CstNonTerminal (prod, semvs)

  end

  module State = struct

    let encode, decode = Lr1.(encode, decode)

    let n = Lr1.n

    (* If [tabulate] is true, we tabulate the shift and reduction actions in
       three two-dimensional tables, [shifts], [reductions], and [goto]. This
       makes a measurable difference in performance, of approximately 30%. *)

    let tabulate =
      true

    type 'a matrix =
      'a array array Lazy.t

    let force =
      Lazy.force

    let[@inline] shift s t =
      SymbolMap.find_opt (Symbol.T t) (Lr1.transitions s)
      |> Option.map encode

    let shifts : state option matrix =
      lazy (Lr1.init @@ fun s -> Terminal.init @@ fun t -> shift s t)

    let foreach_shift (s : state) input yield =
      (* First determine whether the state [s] has a default reduction. *)
      if Lr1.test_default_reduction (decode s) = None then
      (* There is no default reduction. Request the lookahead symbol. *)
      let t = Input.lookahead input in
      vprintf "Testing whether state %d can shift %s... " s (Terminal.print t);
      (* Test whether [s] has an outgoing shift transition along [t]. *)
      let action : state option =
        if tabulate then (force shifts).(s).(Terminal.encode t)
        else shift (decode s) t
      in
      match action with
      | Some s' ->
          vprintf "yes, to state %d\n%!" s';
          yield s'
      | None ->
          vprintf "no\n%!"

    (* The function [maybe_lookahead] is used to simulate the manner
       in which Menhir traditionally invokes the lexer: when there is
       a default reduction, the lexer is called, except in the case of
       a default reduction on [#]. *)

    (* This simulation is currently turned off. We do not offer the
       possibility of simulating Menhir's traditional behavior. *)

    let[@inline] maybe_lookahead ts input =
      if false then (* turned off *)
        if not (TerminalSet.mem Terminal.sharp ts) then
          ignore (Input.lookahead input)

    let[@inline] reduce s t =
      try TerminalMap.find t (Lr1.reductions s) with Not_found -> []

    let[@inline] reduce s t =
      reduce s t |> List.map Production.encode

    let reductions : production list matrix =
      lazy (Lr1.init @@ fun s -> Terminal.init @@ fun t -> reduce s t)

    let foreach_reduction (s : state) input yield =
      vprintf "Testing whether state %d can reduce... " s;
      match Lr1.test_default_reduction (decode s) with
      | Some (prod, ts) ->
          let prod = Production.encode prod in
          maybe_lookahead ts input;
          vprintf "yes (default reduction)\n%!";
          yield prod
      | None ->
          let t = Input.lookahead input in
          let actions : production list =
            if tabulate then (force reductions).(s).(Terminal.encode t)
            else reduce (decode s) t
          in
          match actions with
          | [] ->
              vprintf "no (no reductions on %s)\n%!" (Terminal.print t);
              ()
          | prods ->
              vprintf "yes (%d reductions on %s)\n%!"
                (List.length prods) (Terminal.print t);
              List.iter yield prods

    let goto : state matrix =
      lazy (
        Lr1.init @@ fun s ->
        Nonterminal.init @@ fun nt ->
        try
          SymbolMap.find (Symbol.N nt) (Lr1.transitions s)
          |> encode
        with Not_found ->
          (-1) (* dummy *)
      )

    let goto (s : state) nt : state =
      if tabulate then
        (force goto).(s).(Nonterminal.encode nt)
      else
        SymbolMap.find (Symbol.N nt) (Lr1.transitions (decode s))
        |> encode

    let unique_action
    : [`Shift of state | `Reduce of production | `Fail | `Fork] matrix
    = lazy (
      Lr1.init @@ fun s ->
      Terminal.init @@ fun t ->
      (* Get the shift action, if there is one. *)
      let shift : state option =
        if tabulate then (force shifts).(encode s).(Terminal.encode t)
        else shift s t
      in
      (* Get the reduction actions. *)
      let prods : production list =
        if tabulate then (force reductions).(encode s).(Terminal.encode t)
        else reduce s t
      in
      (* Determine whether there is exactly one action. *)
      match shift, prods with
      | None, [] ->
          `Fail
      | Some s', [] ->
          `Shift s'
      | None, [prod] ->
          `Reduce prod
      | Some _, _ :: _
      | None, _ :: _ :: _ ->
          `Fork
    )

    let unique_action (s : state) input =
      match Lr1.test_default_reduction (decode s) with
      | Some (prod, ts) ->
          let prod = Production.encode prod in
          maybe_lookahead ts input;
          `Reduce prod
      | None ->
          let t = Input.lookahead input in
          (force unique_action).(s).(Terminal.encode t)

  end (* State *)

end

(* -------------------------------------------------------------------------- *)

(* Instantiate the GLR engine. *)

module GLR =
  GLR.Make(Data)

let interpret nt lexer lexbuf =
  let state = Lr1.start_node nt in
  let s = Lr1.encode state in
  let input = Input.create lexer lexbuf in
  try
    let cst = GLR.start s input in
    Some cst
  with Data.Error _ ->
    None

(* -------------------------------------------------------------------------- *)

end (* Make *)
