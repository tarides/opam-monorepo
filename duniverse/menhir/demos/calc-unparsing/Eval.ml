(* An evaluator of abstract syntax trees. *)

open AST

type value =
  int

let eval_unop op (v : value) =
  match op with
  | UNeg ->
      -v

let eval_binop op (v1 : value) (v2 : value) =
  match op with
  | BAdd ->
      v1 + v2
  | BSub ->
      v1 - v2
  | BMul ->
      v1 * v2
  | BDiv ->
      v1 / v2

let rec eval (e : expr) : value =
  match e with
  | EConst i ->
      i
  | EUnOp (op, e) ->
      eval_unop op (eval e)
  | EBinOp (e1, op, e2) ->
      eval_binop op (eval e1) (eval e2)
