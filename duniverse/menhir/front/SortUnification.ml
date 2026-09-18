(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* This module has global state (a generator of unification variables; a
   memoized generator of names for unification variables). This global state
   currently cannot be reset. This should not be a problem, as this is
   almost not observable. (If this module was heavily used and never reset
   then we might end up running out of memory or out of names.) *)

(* -------------------------------------------------------------------------- *)

(* This is the syntax of sorts in the form expected by the unification
   algorithm in [Unifier]. *)

module S = struct

  type 'a structure =
    | Arrow of 'a list

  let map f (Arrow xs) =
    Arrow (List.map f xs)

  let iter f (Arrow xs) =
    List.iter f xs

  exception Iter2

  let iter2 f (Arrow xs1) (Arrow xs2) =
    let n1 = List.length xs1
    and n2 = List.length xs2 in
    if n1 = n2 then
      List.iter2 f xs1 xs2
    else
      raise Iter2

end

include S

(* -------------------------------------------------------------------------- *)

(* Instantiate the unification algorithm. *)

include Unifier.Make(S)

(* A sort (with unification variables) is a deep term. *)

type usort =
  term

(* -------------------------------------------------------------------------- *)

(* Sort constructors. *)

let[@inline] arrow (args : variable list) : variable =
  fresh (Some (Arrow args))

let star : variable =
  arrow []

let[@inline] fresh () =
  fresh None

(* Sort accessors. *)

let domain (x : variable) : variable list option =
  match structure x with
  | Some (Arrow xs) ->
      Some xs
  | None ->
      None

(* -------------------------------------------------------------------------- *)

(* Converting between sorts and ground sorts. *)

let rec ground s =
  match s with
  | TVar _ ->
      (* All variables are replaced with [*]. *)
      Sort.star
  | TNode (Arrow ss) ->
      Sort.arrow (List.map ground ss)

let rec unground s =
  let ss = Sort.domain s in
  TNode (Arrow (List.map unground ss))

(* -------------------------------------------------------------------------- *)

(* A name generator for unification variables. *)

let make_gensym () : unit -> string =
  let c = ref 0 in
  let gensym () =
    let n = MInt.postincrement c in
    Printf.sprintf "%c%s"
      (char_of_int (Char.code 'a' + n mod 26))
      (let d = n / 26 in if d = 0 then "" else string_of_int d)
  in
  gensym

(* A memoized name generator. *)

let make_name () : int -> string =
  let gensym = make_gensym() in
  Fix.Memoize.Int.memoize (fun _x -> gensym())

(* -------------------------------------------------------------------------- *)

(* A printer. *)

let rec print name (b : Buffer.t) (sort : usort) =
  match sort with
  | TVar x ->
      Printf.bprintf b "%s" (name x)
  | TNode (S.Arrow []) ->
      Printf.bprintf b "*"
  | TNode (S.Arrow (sort :: sorts)) ->
      (* Always parenthesize the domain, so there is no ambiguity. *)
      Printf.bprintf b "(%a%a) -> *"
        (print name) sort
        (print_comma_sorts name) sorts

and print_comma_sorts name b sorts =
  List.iter (print_comma_sort name b) sorts

and print_comma_sort name b sort =
  Printf.bprintf b ", %a" (print name) sort

let print sort : string =
  let b = Buffer.create 32 in
  print (make_name()) b sort;
  Buffer.contents b
