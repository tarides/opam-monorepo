(* Unary operators. *)

type unop =
  | UNeg

(* Binary operators. *)

type binop =
  | BAdd
  | BSub
  | BMul
  | BDiv

(* Expressions. *)

type expr =
  | EConst of int
  | EUnOp of unop * expr
  | EBinOp of expr * binop * expr

type main =
  expr
