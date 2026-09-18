%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL

(* We intentionally do not provide precedence declarations, so the grammar is
   highly ambiguous. This is NOT recommended practice. Here, the parsing time
   will be O(n^3) where n is the length of of the input, or more precisely,
   the length of the longest ambiguous input fragment. *)

%start <Expr.uexpr> main
%type  <Expr.uexpr> expr

%{
  open Expr
%}

%%

main:
| e = expr EOL
    { e }
(* Because [EOL] cannot appear inside an expression, the symbol [main] is not
   ambiguous. Therefore this %merge function is really unnecessary. *)
%merge { fun _e1 _e2 -> assert false }

expr:
| i = INT
    { make @@ EIntConst i }
| LPAREN e = expr RPAREN
    { e }
| e1 = expr op = binop e2 = expr
    { make @@ EBinOp (e1, op, e2) }
| op = unop e = expr
    { make @@ EUnOp (op, e) }
%merge
    { fun e1 e2 ->
        let range = ($startpos, $endpos) in
        make @@ disj range e1 e2 }

%inline unop:
  MINUS { UNeg }

%inline binop:
  PLUS  { Add }
| MINUS { Neg }
| TIMES { Mul }
| DIV   { Div }
