/* SPDX-License-Identifier: MIT */

%token <string> ID
%token <string> IDPLUS
%token <string option * string> REF
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
  | REF { let (doc, license) = $1 in
          Types.LicenseRef {Types.document_ref = doc; license_ref = license} }

body:
  | simple { Types.Simple $1 }
  | simple WITH ID { Types.WITH ($1, $3) }
  | body AND body { Types.AND ($1, $3) }
  | body OR body { Types.OR ($1, $3) }
  | LPAREN body RPAREN { $2 }
