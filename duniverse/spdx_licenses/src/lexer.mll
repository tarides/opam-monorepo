(* SPDX-License-Identifier: MIT *)

{

exception Error

}

let idstring = ['a'-'z' 'A'-'Z' '0'-'9' '-' '.']+
let blank = [' ' '\t' '\n']+

rule main = parse
  | blank { main lexbuf }
  | '(' { Parser.LPAREN }
  | ')' { Parser.RPAREN }
  | "WITH" | "with" { Parser.WITH }
  | "AND" | "and" { Parser.AND }
  | "OR" | "or" { Parser.OR }
  | ("DocumentRef-" (idstring as x) ':')? "LicenseRef-" (idstring as y) { Parser.REF (x, y) }
  | ("DocumentRef-" (idstring as x) ':')? "AdditionRef-" (idstring as y) { Parser.ADREF (x, y) }
  | idstring as id { Parser.ID id }
  | (idstring as id) '+' { Parser.IDPLUS id }
  | eof { Parser.EOF }
  | _ { raise Error }
