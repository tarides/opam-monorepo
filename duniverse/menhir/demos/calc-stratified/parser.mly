%token <int> INT
%token PLUS TIMES
%token LPAREN RPAREN
%token EOL

%start <int> main

%%

main:
| e = expr EOL
    { e }

(* A factor is atomic. *)

factor:
| i = INT
    { i }
| LPAREN e = expr RPAREN
    { e }

(* A term is a nonempty product of factors. *)

term:
| f = factor
    { f }
| t = term TIMES f = factor
    { t * f }

(* An expression is a nonempty sum of terms. *)

expr:
| t = term
    { t }
| e = expr PLUS t = term
    { e + t }
