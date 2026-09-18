{
  open Lexing
  open Parser

  let get_position lexbuf = (lexbuf.lex_start_p, lexbuf.lex_curr_p)

  exception Error of string

}

rule token = parse
| [' ' '\t']
    { token lexbuf }
| '\n'
    { new_line lexbuf; token lexbuf }
| ['0'-'9']+
    { NUM ((lexeme lexbuf), (get_position lexbuf)) }
| '+'
    { ADD (get_position lexbuf) }
| '-'
    { SUB (get_position lexbuf) }
| '*'
    { MUL (get_position lexbuf) }
| '/'
    { DIV (get_position lexbuf) }
| '('
    { LPAREN (get_position lexbuf) }
| ')'
    { RPAREN (get_position lexbuf) }
| eof
    { EOF (get_position lexbuf) }
| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n"
     (Lexing.lexeme_start lexbuf))) }
