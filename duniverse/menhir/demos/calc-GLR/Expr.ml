(* -------------------------------------------------------------------------- *)

(* This is abstract syntax for our tiny expression language. *)

(* The type ['self expr] is parameterized; this is an open recursion. *)

(* The recursion is closed in the definition of the type [uexpr].
   We obtain expressions where each node carries a [uid] field. *)

(* The purpose of placing a unique identifier in each node is to allow us to
   detect sharing, that is, to recognize that these are not abstract syntax
   trees but abstract syntax DAGs, and to traverse them in linear time. *)

open Lexing

type range =
  position * position

type unop =
  | UNeg

type binop =
  | Add
  | Neg
  | Mul
  | Div

type 'self expr =
  | EIntConst of int
  | EUnOp of unop * 'self
  | EBinOp of 'self * binop * 'self
  | EDisj of range * 'self list

type uexpr =
  { uid: int; expr: uexpr expr }

(* -------------------------------------------------------------------------- *)

(* Generic traversal combinators for expressions. *)

let iter yield (e : uexpr) =
  match e.expr with
  | EIntConst _ ->
      ()
  | EUnOp (_, e) ->
      yield e
  | EBinOp (e1, _, e2) ->
      yield e1; yield e2
  | EDisj (_, es) ->
      List.iter yield es

let fold yield (e : uexpr) accu =
  match e.expr with
  | EIntConst _ ->
      accu
  | EUnOp (_, e) ->
      let accu = yield e accu in
      accu
  | EBinOp (e1, _, e2) ->
      let accu = yield e1 accu in
      let accu = yield e2 accu in
      accu
  | EDisj (_, es) ->
      List.fold_right yield es accu
        (* order is not consistent with [iter] *)

let list_sum yield xs =
  List.fold_left (fun s x -> s + yield x) 0 xs

let sum yield (e : uexpr) =
  match e.expr with
  | EIntConst _ ->
      0
  | EUnOp (_, e) ->
      yield e
  | EBinOp (e1, _, e2) ->
      yield e1 + yield e2
  | EDisj (_, es) ->
      list_sum yield es

(* -------------------------------------------------------------------------- *)

(* The constructor [make] assigns a fresh ID to a new node. *)

let current =
  ref 0

let make : uexpr expr -> uexpr =
  fun expr ->
    let uid = !current in
    incr current;
    { uid; expr }

(* -------------------------------------------------------------------------- *)

(* A smart constructor for disjunctions. *)

(* We assume that [e1] can be a disjunction, whose range must then be [range],
   where [e2] cannot be a disjunction. *)

let disj (range : range) (e1 : uexpr) (e2 : uexpr) : uexpr expr =
  assert (match e2.expr with EDisj _ -> false | _ -> true);
  match e1.expr with
  | EDisj (range', es) ->
      assert (range = range');
      EDisj (range, e2 :: es)
  | _ ->
      EDisj (range, e2 :: e1 :: [])

(* -------------------------------------------------------------------------- *)

(* The presence of unique IDs lets us equip the type [uexpr] with efficient
   [equal] and [hash] functions, therefore lets us memoize functions whose
   argument type is [uexpr]. *)

module UExpr = struct
  type t = uexpr
  let equal e1 e2 =
    e1.uid = e2.uid
  let hash e =
    Hashtbl.hash e.uid
end

module H =
  Hashtbl.Make(UExpr)

module M =
  Fix.Memoize.ForHashedType(UExpr)

(* -------------------------------------------------------------------------- *)

(* The [size] function computes the tree size of an abstract syntax DAG,
   that is, its size, when it is viewed as a tree. *)

(* This definition is open (parameterized with itself). *)

let usize usize (e : uexpr) : int =
  1 + sum usize e

(* Closing the definition by using [let rec] yields a naive definition,
   whose runtime complexity is exponential. *)

let rec naive_tree_size (e : uexpr) =
  usize naive_tree_size e

(* Closing the definition by using [M.fix] yields a smart definition,
   whose runtime complexity is linear, because the tree size function
   is memoized at every DAG node. *)

(* This function still computes a tree size, not a DAG size.
   It computes the same result as [naive_tree_size] above. *)

let tree_size (e : uexpr) =
  M.fix usize e

(* -------------------------------------------------------------------------- *)

(* The DAG size of a DAG is the number of its nodes. *)

(* To compute the DAG size, we perform a graph traversal (here, DFS). *)

(* This cannot be expressed as an instance [M.fix] at type [uexpr -> int]
   because the first visit of a node should return a non-zero size whereas the
   following visists of this node should return zero.

   This could be expressed as an instance of [M.fix] at type [uexpr -> unit]
   if one accumulates size information as a side effect in a reference. It
   could even be expressed as an instance of [foreach_DAG_node] (below). But
   one must then use [iter] instead of [sum]; this is somewhat less elegant. *)

let dag_size (e : uexpr) =
  let marked = Hashtbl.create 1023 in
  let rec visit (e : uexpr) =
    if Hashtbl.mem marked e then 0 else
    let () = Hashtbl.add marked e () in
    1 + sum visit e
  in
  visit e

(* -------------------------------------------------------------------------- *)

(* [foreach_DAG_node e] enumerates the nodes in a DAG in linear time. *)

(* The nodes are enumerated bottom-up. *)

let foreach_DAG_node (e : uexpr) (yield : uexpr -> unit) : unit =
  let visit : uexpr -> unit =
    M.fix @@ fun (visit : uexpr -> unit) (e : uexpr) ->
    iter visit e;
    yield e
  in
  visit e

(* -------------------------------------------------------------------------- *)

(* [disjunctions e] counts the number of disjunctions in the expression [e]. *)

let[@inline] with_state v action =
  let s = ref v in
  action s;
  !s

let disjunctions e =
  with_state 0 @@ fun c ->
  foreach_DAG_node e @@ fun e ->
  match e.expr with
  | EDisj _ -> incr c
  | _       -> ()

(* [show_disjunctions text e] prints the ranges (intervals) where an ambiguous
   subexpression of the expression [e] has been detected. *)

(* It is questionable whether this is a good idea, but this is a demo. *)

(* A child of a disjunction node can be another disjunction node, whose
   range is the same. As a result, a range can be shown several times. *)

let[@inline] linenum (startp, _endp) =
  startp.pos_lnum

let[@inline] column (startp, _endp) =
  startp.pos_cnum - startp.pos_bol

let[@inline] end_column (startp, endp) =
  endp.pos_cnum - startp.pos_bol (* intentionally [startp.pos_bol] *)

let[@inline] fragment text (startp, endp) =
  String.sub text startp.pos_cnum (endp.pos_cnum - startp.pos_cnum)

let show_disjunction text range es =
  Printf.printf
    "At line %d, characters %d-%d: ambiguous subexpression (%d readings):\n%s\n"
    (linenum range) (column range) (end_column range)
    (List.length es)
    (fragment text range)

let show_disjunctions text e =
  foreach_DAG_node e @@ fun e ->
  match e.expr with
  | EDisj (range, es) -> show_disjunction text range es
  | _                 -> ()
