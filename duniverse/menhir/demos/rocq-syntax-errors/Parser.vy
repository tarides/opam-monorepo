(* Grammar for little arithmetic expressions (+-*/) *)

%{

From Coq Require Import String.

Module Ast.

Parameter loc: Type.
Parameter string: Type.

Inductive expr :=
 | Num : string -> expr
 | Add : expr -> expr -> expr
 | Sub : expr -> expr -> expr
 | Mul : expr -> expr -> expr
 | Div : expr -> expr -> expr.
End Ast.

%}

%token<Ast.loc> ADD SUB MUL DIV LPAREN RPAREN EOF
%token<Ast.string * Ast.loc> NUM

%start<Ast.expr> parse_expr
%type<Ast.expr> p_expr
%type<Ast.expr> p_factor
%type<Ast.expr> p_atom

%%

parse_expr : p_expr EOF       { $1 }

p_atom :
| NUM                         { Ast.Num (fst $1) }
| LPAREN p_expr RPAREN        { $2 }

p_expr :
| p_factor                    { $1 }
| p_expr ADD p_factor         { Ast.Add $1 $3 }
| p_expr SUB p_factor         { Ast.Sub $1 $3 }

p_factor :
| p_atom                      { $1 }
| p_factor MUL p_atom         { Ast.Mul $1 $3 }
| p_factor DIV p_atom         { Ast.Div $1 $3 }
