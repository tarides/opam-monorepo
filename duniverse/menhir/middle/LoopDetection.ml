(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open MiddleAPI

module Make
(G : GRAMMAR)
= struct
open G

(* -------------------------------------------------------------------------- *)

(* Let us write A -> alpha when there exists a production A -> alpha, and let
   us write beta => gamma when the sentential form beta expands (in one step)
   to gamma. *)

(* According to Aho and Ullman ("The Theory of Parsing, Translation, and
   Compiling -- Volume I: Parsing", page 150), a grammar is cycle-free if
   there is no derivation of the form A =>+ A. In other words, there is a
   cycle when a nonterminal symbol A expands in one or more steps to itself. *)

(* Under the assumption that every nonterminal symbol is reachable and
   generates a nonempty language, the presence of a cycle implies that the
   grammar is infinitely ambiguous: for some inputs, there is an infinite
   number of parse trees. *)

(* We reject such a grammar, on two grounds: first, it seems pathological,
   and is likely the result of a mistake; second, the algorithm that we use
   to speed up closure computations (in [Lr0.Closure]) does not tolerate the
   presence of certain cycles. *)

(* Let us define a relation R as follows: A R B holds if and only if there is
   a production A -> alpha B beta where alpha and beta are nullable. Then, it
   is not difficult to see that the relations =>+ and R+ coincide. That is, to
   check that a grammar is cycle-free, it suffices to check that the relation
   R is acyclic. *)

(* The relation R is defined as follows. Upon first reading, take
   [require_nullable_suffix] to be [true] and
   [require_nonempty_prefix] to be [false]. *)

let nullable_suffix prod i =
  let nullable, _ = Analysis.nullable_first prod i in
  nullable

let successors
  ~require_nullable_suffix
  ~require_nonempty_prefix
  (yield : Production.t -> Nonterminal.t -> unit)
  (nt : Nonterminal.t) : unit
=
  Production.iternt nt @@ fun prod ->
  let rhs = Production.rhs prod in
  let n = Array.length rhs in
  let nullable_prefix = ref true in
  let i = ref 0 in
  while !nullable_prefix && !i < n do
    match rhs.(MInt.postincrement i) with
    | Symbol.T _   ->
        nullable_prefix := false
    | Symbol.N nt' ->
        if (not require_nullable_suffix || nullable_suffix prod !i)
        && (not require_nonempty_prefix || !i > 1) then
          yield prod nt';
        nullable_prefix := Analysis.nullable nt'
  done

(* This adapter hides [prod] from the user function [yield]. *)

let adapt successors yield nt =
  successors (fun _prod nt' -> yield nt') nt

(* A detailed explanation of cycles whose length is greater than one. *)

let show_cycle nts nt =
  assert (List.hd nts = nt);
  if List.length nts = 1 then "" else begin
    let nts = Array.of_list (nts @ [nt]) in
    let i = ref 0 in
    let next () = Nonterminal.print false nts.(MInt.postincrement i)
    and finished () = !i = Array.length nts in
    MString.with_buffer 1024 @@ fun b ->
    let out format = Printf.bprintf b format in
    out "%s" (next());
    while not (finished()) do
      out " expands to %s" (next());
      if finished() then out ".\n" else out ",\nwhich"
    done
  end

let fail main nts nt =
  let positions = List.flatten (List.map Nonterminal.positions nts) in
  Report.error main positions
    "the grammar is cyclic:\n\
     the nonterminal symbol %s expands to itself.\n%s\
     A cyclic grammar is ambiguous."
    (Nonterminal.print false nt)
    (show_cycle nts nt)

(* To detect a cycle in a relation, we use the combinator [defensive_fix] that
   is provided by the library Fix. We define a function of type [Nonterminal.t
   -> unit] that computes nothing but calls itself recursively according to
   the pattern defined by the relation R. Then, we evaluate this function
   everywhere. If there is a cycle, it is detected and reported. *)

(* The claim that "a cyclic grammar is ambiguous" implicitly assumes that
   every nonterminal symbol is reachable and inhabited. *)

let detect_cycle main =
  let module M = Fix.Memoize.ForNumberedType(Nonterminal) in
  let successors_R =
    successors ~require_nullable_suffix:true ~require_nonempty_prefix:false
    |> adapt
  in
  let check = M.defensive_fix successors_R in
  try
    Nonterminal.iter check
  with M.Cycle (nts, nt) ->
    fail main nts nt

(* -------------------------------------------------------------------------- *)

(* Another anomaly that we wish to detect is hidden left recursion. In the
   paper "Looping LR parsers" (1988), Soisalon-Soininen and Tarhio define
   hidden left recursion (although they do not explicitly use this term)
   and point out that: 1- a grammar that has hidden left recursion cannot
   be LR(k) for any k, and (worse) 2- if the shift/reduce conflict that it
   causes is resolved in favor in reduction, then the deterministic parser
   that is constructed can diverge by entering an infinite sequence of
   reductions. Conversely, they show if a grammar exhibits no cycle and
   no hidden left recursion, then the parser must terminate, regardless of
   how conflicts are resolved. *)

(* One possible definition of hidden left recursion, given by Nederhof and
   Sarbo in the paper "Increasing the Applicability of LR Parsing" (1993), is
   the existence of a production A -> B alpha where B is nullable and alpha
   expands (in zero or more steps) to A beta. *)

(* Let us define a relation S as follows. A S B holds if and only if there is
   a production A -> alpha B beta where alpha is nullable. This relation can
   be viewed as the disjoint union of two smaller relations L and H, defined
   as follows:

   - A L B holds if and only if there is a production A ->       B beta;
   - A H B holds if and only if there is a production A -> alpha B beta
                             where alpha is nullable but is not epsilon.

   A cycle in the relation L is fine: it represents ordinary left recursion.
   A cycle that involves at least one H edge and any number of L and H edges,
   however, denotes hidden left recursion. *)

(* To detect hidden left recursion in linear time, we first compute the
   strongly connected components of the relation S. Then, we check every edge
   in the relation H. If the source and destination vertices of this edge lie
   in the same component, then we have detected hidden left recursion. *)

module N = struct
  type node = Nonterminal.t
  let n = Nonterminal.n
  let index = Nonterminal.encode
  let iter = Nonterminal.iter
end

let rec detect_hidden_left_recursion main =
  (* Compute the components of S, that is, L U H. *)
  let module T = Fix.SCC.Run (struct
    include N
    let successors =
      successors ~require_nullable_suffix:false ~require_nonempty_prefix:false
      |> adapt
  end) in
  (* Enumerate every edge in H. *)
  let successors_H =
    successors ~require_nullable_suffix:false ~require_nonempty_prefix:true in
  Nonterminal.iter @@ fun nt ->
  nt |> successors_H @@ fun prod nt' ->
  (* If the source vertex [nt] and the destination vertex [nt'] lie in
     the same component, then we have detected hidden left recursion. *)
  if T.representative nt = T.representative nt' then
    fail main prod

(* An error message. *)

and fail main prod =
  let nt, rhs = Production.def prod in
  let positions = Production.positions prod in
  Report.error main positions
    "the grammar exhibits hidden left recursion: in the production\n\
     %s,\n\
     the nonterminal symbol %s is nullable,\n\
     and the remainder of the right-hand side expands to a sentential form\n\
     that begins with the nonterminal symbol %s.\n\
     This implies that the grammar is not LR(k) for any k."
    (Production.print prod)
    (Symbol.print false rhs.(0))
    (Symbol.print false (Symbol.N nt))
      (* Furthermore, this creates a shift/reduce conflict, which (if resolved
         in favor of reduction) can cause the parser to diverge. *)

(* -------------------------------------------------------------------------- *)

(* Hidden right recursion can also be problematic, for the following reasons:

   - It is a symptom of a serious ambiguity,
     which may be the result of a design mistake.
     This said, it can also be intentional: a typical example is

       instr ::= IF expr THEN instr (ELSE instr)?

     which is ambiguous, but where the programmer intends to resolve
     the shift/reduce conflict in the favor of shifting.

   - It breaks certain variants of the GLR algorithm, such as Tomita's
     Algorithm 1. See Scott and Johnstone, "Right Nulled GLR Parsers" (2006),
     section 3.4, "Right Nullable Rules and Hidden Right Recursion".

   In McPeak's GLR algorithm (Elkhound), right nullable rules create the need
   for the auxiliary function [enqueueLimitedReductions], which is a source of
   inefficiency. Our workaround is to expand away all right nullable suffixes;
   see [ExpandNullable]. In this case there is no need to forbid hidden right
   recursion; once right nullable suffixes are expanded away, it is transformed
   into ordinary right recursion. *)

(* The detection algorithm is the same as for hidden left recursion.
   We define two relations R and H as follows:

   - A R B holds if and only if there is a production A -> alpha B     ;
   - A H B holds if and only if there is a production A -> alpha B beta
                             where beta is nullable but is not epsilon.

   A cycle in the relation R is fine: it represents ordinary right recursion.
   A cycle that involves at least one H edge and any number of R and H edges
   denotes hidden right recursion. *)

let successors
  ~require_nonempty_suffix
  (yield : Production.t -> Nonterminal.t -> unit)
  (nt : Nonterminal.t) : unit
=
  Production.iternt nt @@ fun prod ->
  let rhs = Production.rhs prod in
  let n = Array.length rhs in
  let i = ref (n-1) in
  while 0 <= !i && nullable_suffix prod (!i+1) do
    let nonempty_suffix = !i < n-1 in
    match rhs.(MInt.postdecrement i) with
    | Symbol.T _   ->
        ()
    | Symbol.N nt' ->
        if (not require_nonempty_suffix || nonempty_suffix) then
          yield prod nt'
  done

let rec detect_hidden_right_recursion main =
  (* Compute the components of R U H. *)
  let module T = Fix.SCC.Run (struct
    include N
    let successors = successors ~require_nonempty_suffix:false |> adapt
  end) in
  (* Enumerate every edge in H. *)
  let successors_H = successors ~require_nonempty_suffix:true in
  Nonterminal.iter @@ fun nt ->
  nt |> successors_H @@ fun prod nt' ->
  (* If the source and destination vertices of this edge lie in the
     same component of R U H then there is hidden right recursion. *)
  if T.representative nt = T.representative nt' then
    fail main prod

and fail main prod =
  let nt, rhs = Production.def prod in
  let n = Array.length rhs in
  let positions = Production.positions prod in
  Report.error main positions
    "the grammar exhibits hidden right recursion: in the production\n\
     %s,\n\
     the last symbol (%s) is nullable,\n\
     and the rest of the right-hand side expands to a sentential form\n\
     that ends with the symbol %s."
    (Production.print prod)
    (Symbol.print false rhs.(n-1))
    (Symbol.print false (Symbol.N nt))

(* -------------------------------------------------------------------------- *)

end (* Make *)
