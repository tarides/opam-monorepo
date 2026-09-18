/* SPDX-License-Identifier: MIT */

%token <string> ID
%token <string> IDPLUS
%token <string option * string> REF
%token <string option * string> ADREF
%token WITH
%token AND
%token OR
%token LPAREN
%token RPAREN
%token EOF

%left OR
%left AND

%start main
%type <Types.t> main

%%

main: body EOF { $1 }

simple:
  | ID { Types.LicenseID $1 }
  | IDPLUS { Types.LicenseIDPlus $1 }
  | REF { let (document_ref, license_ref) = $1 in
          Types.LicenseRef {Types.document_ref; license_ref} }

addition:
  | ID { Types.Exception $1 }
  | ADREF { let (document_ref, addition_ref) = $1 in
            Types.AdditionRef {Types.document_ref; addition_ref} }

body:
  | simple { Types.Simple $1 }
  | simple WITH addition { Types.WITH ($1, $3) }
  | body AND body { Types.AND ($1, $3) }
  | body OR body { Types.OR ($1, $3) }
  | LPAREN body RPAREN { $2 }
