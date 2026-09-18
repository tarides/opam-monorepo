%token <int> INT
%token PLUS TIMES
%token LPAREN RPAREN
%token EOL

%left PLUS        /* lowest precedence */
%left TIMES       /* higher precedence */

%start <int> main

%%

main:
| e = expr EOL
    { e }

expr:
| i = INT
    { i }
| LPAREN e = expr RPAREN
    { e }
| e1 = expr PLUS e2 = expr
    { e1 + e2 }
| e1 = expr TIMES e2 = expr
    { e1 * e2 }
