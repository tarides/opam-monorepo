(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Located
open PlainSyntax
open PlainSyntaxAccessors
module M = StringMap

type policy =
  [ `ExpandNullableSymbols
  | `ExpandNullableSuffixParticipants
  | `ExpandNullableSuffixes ]

module Make
(X : sig
  open Report
  val main: channel
  val policy: policy
  val grammar: grammar

  (***When [forceful] is [true], any %prec annotations and %on_error_reduce
      declarations that seem problematic are silently erased. This is used
      by TestGLR. When [forceful] is [false], which is the normal setting,
      these problematic annotations give rise to errors. *)
  val forceful: bool

end) ()
= struct
open X

(* -------------------------------------------------------------------------- *)

(* We need to know which symbols (and productions) are nullable. *)

module Nullable =
  Nullable.Make(X)

let nullable_producer (producer : producer) : bool =
  Nullable.symbol producer.prod_symbol

(* We have to be careful: the above functions offer an analysis of the
   original grammar. They cannot be applied to symbols that exist only
   in the transformed grammar. *)

(* -------------------------------------------------------------------------- *)

(* Compute the set of "participants", that is, the set of nullable nonterminal
   symbols N that occur in the right nullable suffix of some production.

   When the policy is [`ExpandNullableSuffixParticipants], these symbols are
   marked %inline, therefore fully expanded away.

   When the policy is [`ExpandNullableSuffixes], all occurrences of these
   symbols inside a right nullable suffix are marked %inlined and expanded
   away. Their other (non-problematic) occurrences are retained. *)

let participants : StringSet.t =
  MRef.with_state StringSet.empty @@ fun participants ->
  foreach_branch grammar @@ fun branch ->
  let producers = Array.of_list branch.producers in
  let n = Array.length producers in
  let i = ref (n-1) in
  while 0 <= !i && nullable_producer producers.(!i) do
    let nt = producers.(!i).prod_symbol in
    assert (is_nonterminal grammar nt);
    participants := StringSet.add nt !participants;
    decr i
  done

let is_participant (nt : nonterminal) : bool =
  StringSet.mem nt participants

(* -------------------------------------------------------------------------- *)

(* Statistics. *)

let is_right_nullable (branch : branch) : bool =
  let rhs = branch.producers in
  rhs <> [] &&
  Nullable.symbol (MList.last rhs).prod_symbol

let count_nullable_symbols () : int =
  MInt.with_counter @@ fun c ->
  foreach_nonterminal grammar @@ fun nt ->
  assert (is_nonterminal grammar nt);
  if Nullable.symbol nt then incr c

let total_production_length () : int =
  MInt.with_counter @@ fun c ->
  foreach_branch grammar @@ fun branch ->
  c := !c + List.length branch.producers

let count_productions () : int =
  MInt.with_counter @@ fun c ->
  foreach_branch grammar @@ fun _branch ->
  incr c

let count_right_nullable_productions () : int =
  MInt.with_counter @@ fun c ->
  foreach_branch grammar @@ fun branch ->
  if is_right_nullable branch then incr c

let info aux =
  let cns = count_nullable_symbols()
  and cp = count_productions()
  and crnp = count_right_nullable_productions() in
  Report.log aux
    "%d out of %d nonterminal symbols are nullable."
    cns (List.length (nonterminals grammar));
  Report.log aux
    "There are %d productions, whose total length is %d."
    cp (total_production_length());
  Report.log aux
    "%d out of %d productions are right nullable."
    crnp cp;
  Report.log aux
    "%d out of %d nullable nonterminal symbols occur in a nullable suffix."
    (StringSet.cardinal participants) cns;
  ()

(* -------------------------------------------------------------------------- *)

module Run () = struct

(* -------------------------------------------------------------------------- *)

(* The functions [use] and [used] keep track of the names that have been
   used already (and must not be used again). *)

(* There are similar functions in [JoinGrammars]. *)

let (use : nonterminal -> nonterminal), (used : nonterminal -> bool) =
  let used = ref (StringSet.of_list (nonterminals grammar)) in
  let use nt = used := StringSet.add nt !used; nt
  and used nt = StringSet.mem nt !used in
  use, used

(* [fresh candidate] generates a fresh name (which has not been used
   already) and records the fact that this name has been used. The
   parameter [candidate] is a candidate for the new name. *)

let fresh : string -> nonterminal =
  let c = ref 0 in
  let rec fresh base =
    let nt = base ^ string_of_int (MInt.preincrement c) in
    if used nt then fresh base (* try again *)
    else use nt
  in
  let fresh candidate =
    if used candidate then fresh candidate else use candidate
  in fresh

(* -------------------------------------------------------------------------- *)

(* Naming conventions. *)

(* For each nullable terminal N in the grammar, we create two new symbols Ne,
   and Nne. The symbol Ne represents the "empty part" of N; the symbol [Nne]
   represents the "nonempty part" of N. *)

let empty nt =
  "empty_" ^ nt
  (* This symbol will be %inlined away, so its name does not matter. *)

let nonempty nt =
  "nonempty_" ^ nt
  (* This symbol will not be %inlined away, so its name does matter. *)

(* Apply the above conventions while avoiding name collisions. *)

let empty, nonempty =
  let module M = Fix.Memoize.ForType(String) in
  M.memoize (fun nt -> fresh (empty nt)),
  M.memoize (fun nt -> fresh (nonempty nt))

(* [rename_producer f producer] applies the transformation [f] to
   the producer [producer], which must be a nonterminal symbol. *)

let rename_producer (f : nonterminal -> nonterminal) producer : producer =
  let prod_symbol = producer.prod_symbol in
  assert (is_nonterminal grammar prod_symbol);
  assert (Nullable.symbol prod_symbol);
  let prod_symbol = f prod_symbol in
  { producer with prod_symbol }

let empty_producer, nonempty_producer =
  rename_producer empty,
  rename_producer nonempty

let empty_producers : producers -> producers =
  List.map empty_producer

(* Let us use the word "primed" to mean "marked %inlined". *)

(* [prime] marks a producer as %inlined. *)

let prime (producer : producer) : producer =
  { producer with prod_inlined = true }

(* -------------------------------------------------------------------------- *)

(* For each nullable terminal N in the grammar, we create two new symbols Ne,
   and Nne. We want Ne to represent the empty part of N, and Nne to represent
   the nonempty part of N. So, Ne should generate just the empty word; Nne
   should generate only nonempty words.

   Furthermore, we change the definition of N.

   Finally, we mark some use sites of N as %inlined.

   The transformed grammar is as follows:

   N   := Ne | Nne

   %inline
   Ne  := <copy the nullable productions of N
           where every symbol A is replaced with Ae>

   Nne := <copy the non-nullable productions of N>
        | <copy the nullable non-epsilon productions of N
           where one symbol A is replaced with Ane>
        # where, in all of the above productions,
          some symbols A are marked %inlined
          as per the policy described below

   The symbol Ne generates the language {epsilon}: it nullable and nonempty.
   The symbol Nne is not nullable. It can be empty.

   The symbol Nne cannot be marked %inline because it can be recursive.
   For example, if N is a (recursive) possibly empty list
            then Nne is a (recursive) nonempty list.

   The symbols Ne are marked %inline: we want to expand them away.
   A cycle between these symbols cannot exist because that would
   imply the existence of a cycle in the original grammar.

   Marking a use site of N as %inlined in a right-hand side causes the host
   production to be split in two parts, where one part uses Ne and the other
   part uses Nne; then the first part is simplified again as Ne is inlined
   away.

   Now, which occurrences of the symbol N do we wish to mark %inlined?

   One policy, named [`ExpandNullableSymbols], is to mark all occurrences of N.
   This yields a grammar where no symbol is nullable.

   A less aggressive policy, named [`ExpandNullableSuffixes], is to mark
   an occurrence of N as %inlined only if this occurrence is part of the right
   nullable suffix of a production. This yields a grammar where no production is
   right nullable.

   A middle ground, named [`ExpandNullableSuffixParticipants], is to mark all
   occurrences of N as %inlined if at least one occurrence of N is part of the
   right nullable suffix of a production. This is equivalent to to marking N
   itself as %inline. This also yields a grammar where no production is right
   nullable.

   It may be the case that the symbol Nne has zero productions or,
   more generally, has some productions, all of which generate the
   empty language. Therefore we post-process the grammar using [RemoveVoid].  *)

(* -------------------------------------------------------------------------- *)

(* Before performing the transformation, check that several preconditions are
   met. In particular, we want to guarantee that calling [Inline.inline]
   cannot fail, so we must check that the %inline and %inlined annotations
   that we introduce are acceptable. *)

(* We allow a symbol [N] that is expanded away to carry a %merge function.
   This %merge function is then inherited by the symbols [Ne] and [Nne]. If
   the symbol [N] itself survives (which is possible only when the policy is
   [`ExpandNullableSuffixes]) then it keeps its %merge function, too. *)

(* [should_mark_inline nt] determines whether the nonterminal symbol [nt]
   should be marked %inline, that is, eliminated. *)

let should_mark_inline nt =
  Nullable.symbol nt &&
  match X.policy with
  | `ExpandNullableSymbols ->
      true
  | `ExpandNullableSuffixParticipants ->
      is_participant nt
  | `ExpandNullableSuffixes ->
      false

let will_be_expanded_at_least_once nt =
  Nullable.symbol nt &&
  match X.policy with
  | `ExpandNullableSymbols ->
      true
  | `ExpandNullableSuffixParticipants
  | `ExpandNullableSuffixes ->
      is_participant nt

(* A start symbol cannot be marked %inline. *)

let () =
  grammar.start_symbols |> StringSet.iter @@ fun nt ->
  if should_mark_inline nt then
    Report.error main []
      "the start symbol %s is nullable, therefore cannot be eliminated."
      nt

(* If [forceful] is true, we forcefully transform the grammar to remove
   problematic %prec annotations. Otherwise, (below,) we detect these
   annotations and fail. *)

let grammar =
  if forceful then
    let rules =
      grammar.rules |> M.mapi @@ fun nt rule ->
      let branches =
        rule.branches |> List.map @@ fun branch ->
        if will_be_expanded_at_least_once nt || is_right_nullable branch then
          { branch with prec_annotation = None }
        else branch
      in { rule with branches }
    in { grammar with rules }
  else
    grammar

(* The productions of a nullable symbol (which, according to the desired
   policy, will be expanded away) must not carry %prec annotations. *)

let () =
  grammar.rules |> M.iter @@ fun nt rule ->
  if will_be_expanded_at_least_once nt then
    rule.branches |> List.iter @@ fun branch ->
    branch.prec_annotation |> Option.iter @@ fun prec ->
    Report.error main [ position prec ]
      "because the nullable symbol %s will be expanded away,\n\
       its productions must not carry %%prec annotations."
      nt

(* The right nullable productions must not carry %prec annotations.
   (It might make sense to allow them to carry such an annotation;
   however, currently, this will cause an error while inlining.) *)

let () =
  foreach_branch grammar @@ fun branch ->
  if is_right_nullable branch then
    branch.prec_annotation |> Option.iter @@ fun prec ->
    Report.error main [ position prec ]
      "because this production has a nullable suffix,\n\
       which will be expanded away, it must not carry a %%prec annotation."

(* If [forceful] is true, we forcefully transform the grammar to remove
   problematic %on_error_reduce declarations. Otherwise, (below,) we detect
   these declarations and fail. *)

let grammar =
  if forceful then
    let on_error_reduce =
      grammar.on_error_reduce |> M.filter @@ fun nt _level ->
      not (will_be_expanded_at_least_once nt)
    in { grammar with on_error_reduce }
  else
    grammar

(* A symbol that will be expanded away should not be mentioned in an
   %on_error_reduce declaration. (We are perhaps too conservative and signal
   this error as soon as one occurrence of this symbol is expanded away.) *)

let () =
  grammar.on_error_reduce |> M.iter @@ fun nt _level ->
  if will_be_expanded_at_least_once nt then
    Report.error main []
      "because the nullable symbol %s will be expanded away,\n\
       it must not appear in an %%on_error_reduce declaration."
      nt

(* The [error] token should not be used. Its meaning is very fragile, so it
   may not be preserved by the transformation. Furthermore, its presence may
   disturb the computation of nullable symbols. *)

let () =
  if grammar_uses_error_token grammar then
    Report.error main []
      "this transformation does not support the special token error."
      (* If we were more brave, we would extract the source location
         where the [error] token is used. *)
      (* The message is a bit obscure, but this seems acceptable, as
         this transformation is undocumented anyway. *)
      (* When --GLR is selected, all productions that use the [error]
         token are removed before this transformation is performed. *)

(* -------------------------------------------------------------------------- *)

(* This dummy production level should be irrelevant; it is used only to
   decorate branches that will be inlined away. *)

let dummy_production_level =
  ProductionLevel (InputFile.builtin, 0)

(* [unit_branch nt] constructs a branch (that is, the right-hand side of a
   production) of length 1, whose symbol is the nonterminal symbol [nt]. *)

let unit_branch (nt : nonterminal) : branch =
  let x = "_1" in
  let prod_id = Located.locate Range.dummy x
  and prod_symbol = nt
  and prod_attributes = []
  and prod_inlined = false in
  let producer = { prod_id; prod_symbol; prod_attributes; prod_inlined } in
  let branch_position = Range.dummy
  and producers = [ producer ]
  and action = Action.make 0 (StringSet.singleton x) [] (IL.EVar x)
  and prec_annotation = None
  and production_level = dummy_production_level
  and br_attributes = [] in
  { branch_position; producers; action;
    prec_annotation; production_level; br_attributes }

let split_branches (nt : nonterminal) : branches =
  [ unit_branch (empty nt);
    unit_branch (nonempty nt) ]

let split_rule (nt : nonterminal) (rule : rule) : rule =
  let branches = split_branches nt
  and positions = rule.positions (* debatable *)
  and inline_flag = should_mark_inline nt
  and attributes = [] in
  let merge = if inline_flag then None else rule.merge in
  { branches; positions; inline_flag; attributes; merge }

let empty_branch (branch : branch) : branch option =
  if Nullable.production branch then
    (* Every symbol in the right-hand side is nonterminal and nullable. *)
    (* Keep this branch, replacing every symbol A with Ae. *)
    Some (transform_branch_producers empty_producers branch)
  else
    (* Drop this branch. *)
    None

let empty_branches (branches : branches) : branches =
  MList.filter_map empty_branch branches

let empty_rule (rule : rule) : rule =
  let branches = empty_branches rule.branches
  and positions = rule.positions (* debatable *)
  and inline_flag = true
  and attributes = []
  and merge = rule.merge in
  { branches; positions; inline_flag; attributes; merge }

let prime_suffix i (producers : producers) : producers =
  assert (0 <= i && i <= List.length producers);
  MList.take i producers @ List.map prime (MList.drop i producers)

let rightmost_non_nullable_producer_index (producers : producers) : int =
  let producers = Array.of_list producers in
  let n = Array.length producers in
  MRef.with_state (n-1) @@ fun i ->
  while nullable_producer producers.(!i) do decr i done;
  assert (0 <= !i && !i < n && not (nullable_producer producers.(!i)))

let prime_right_nullable_suffix (branch : branch) : branch =
  assert (not (Nullable.production branch));
  branch |> transform_branch_producers @@ fun producers ->
  let i = rightmost_non_nullable_producer_index producers in
  prime_suffix (i+1) producers

let prime_right_nullable_suffixes (rule : rule) : rule =
  let branches = List.map prime_right_nullable_suffix rule.branches in
  { rule with branches }

let nonempty_branch (branch : branch) : branches =
  if Nullable.production branch then
    (* This branch is nullable. *)
    let n = List.length branch.producers in
    if n = 0 then
      (* This is an epsilon branch. Drop it. *)
      []
    else
      (* This is a nullable non-epsilon branch.
         For each symbol A in the right-hand side,
         keep a copy of this branch where A is replaced with Ane. *)
      MList.init n @@ fun i ->
      branch |> transform_branch_producers @@ fun producers ->
      MList.update i nonempty_producer producers
      |> prime_suffix (i+1)
  else
    (* This branch is not nullable. Keep it. *)
    [ prime_right_nullable_suffix branch ]

let nonempty_branches (branches : branches) : branches =
  List.flatten (List.map nonempty_branch branches)

let nonempty_rule (rule : rule) : rule =
  let branches = nonempty_branches rule.branches
  and positions = rule.positions (* debatable *)
  and inline_flag = false
  and attributes = []
  and merge = rule.merge in
  { branches; positions; inline_flag; attributes; merge }

let rules : rule M.t =
  M.fold (fun nt rule rules ->
    (* For each definition of a symbol N in the grammar, *)
    (* If N is nullable then *)
    if Nullable.symbol nt then
      rules
      (* redefine the symbol N as Ne | Nne, *)
      |> M.add nt (split_rule nt rule)
      (* define the symbol Ne, *)
      |> M.add (empty nt) (empty_rule rule)
      (* and define the symbol Nne. *)
      |> M.add (nonempty nt) (nonempty_rule rule)
    else
      (* If N is not nullable, just keep its definition. *)
      rules
      |> M.add nt (prime_right_nullable_suffixes rule)
  ) grammar.rules M.empty

(* For each %type declaration in the grammar, create %type declarations
   for N and Nne. The symbol Ne does not need %type declarations because
   it is marked %inline. *)

let types : ocamltype M.t =
  M.fold (fun nt ty types ->
    if Nullable.symbol nt then
      types
      |> M.add nt ty
      |> M.add (nonempty nt) ty
    else
      types
      |> M.add nt ty
  ) grammar.types M.empty

let grammar =
  { grammar with rules; types }
  |> RemoveVoid.transform
  |> Inlining.inline main
  |> Trim.trim Report.null (* do not warn about unreachable symbols *)

end (* Run *)

end (* Make *)

(* -------------------------------------------------------------------------- *)

(* Re-package [Make] as a function. *)

let transformation policy =
  match policy with
  | `ExpandNullableSymbols ->
      "eliminating all nullable symbols"
  | `ExpandNullableSuffixParticipants ->
      "eliminating the symbols that participate in right nullable suffixes"
  | `ExpandNullableSuffixes ->
      "eliminating right nullable suffixes"

let transform main aux forceful policy grammar =
  let module M = Make(struct
    let main = main
    let policy = policy
    let grammar = grammar
    let forceful = forceful
  end)() in
  (* Display information messages about the original grammar. *)
  M.info aux;
  (* Transform the grammar. *)
  Report.log aux "Now %s." (transformation policy);
  let module R = M.Run() in
  let grammar = R.grammar in
  (* Display information messages about the transformed grammar. *)
  let module M = Make(struct
    let main = main
    let policy = policy
    let grammar = grammar
    let forceful = false
  end)() in
  M.info aux;
  grammar

(* -------------------------------------------------------------------------- *)

(* The least aggressive expansion policy, [`ExpandNullableSuffixes], can
   create conflicts by destroying left factors. Here is an example.

   Suppose that the nonterminal symbol N has two rules that begin in
   the same way:

   N := A B
      | A B C

   Suppose that A and B are nullable, but C is not. `ExpandNullableSuffixes
   must then expand away A and B in the first rule, but not in the second
   rule. The transformation produces these new rules:

   Nne :=
      | Ane
      | Bne
      | Ane Bne
      | A B C
   A := ε
      | Ane

   The left factor A B has been lost: some rules for Nne begin with Ane,
   while another rule begins with A. This creates a shift/reduce conflict.
   If the lookahead symbol is in the set FIRST(Bne), then it is permitted
   to shift (because of the item Nne → . Bne), and it is permitted to
   reduce the production A → ε (because of the item Nne → . A B C,
   which implies the presence of the item A → . ε).

   The slightly more aggressive policy `ExpandNullableSuffixParticipants
   is intended to avoid this problem. *)

(* At this time I do not know whether the policies [`ExpandNullableSymbols]
   and [`ExpandNullableSuffixParticipants] can create conflicts. I would
   hope not, but I am not certain. *)
