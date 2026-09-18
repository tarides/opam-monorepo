open Parser.DCST

(* Converting an AST to a DCST. *)

(* This code indicates where parentheses can be inserted. *)

let maybe_paren e =
  expr_choice e (paren e)

let rec expr (e : AST.expr) : expr =
  maybe_paren (match e with
  | EConst i ->
      int i
  | EUnOp (UNeg, e) ->
      neg (expr e)
  | EBinOp (e1, BAdd, e2) ->
      add (expr e1) (expr e2)
  | EBinOp (e1, BSub, e2) ->
      sub (expr e1) (expr e2)
  | EBinOp (e1, BMul, e2) ->
      mul (expr e1) (expr e2)
  | EBinOp (e1, BDiv, e2) ->
      div (expr e1) (expr e2)
  )

let main (e : AST.main) : main =
  eol (expr e)
