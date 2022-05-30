{
type t =
  | OPEN_PAREN
  | CLOSE_PAREN
  | COMMA
  | TEXT of Buffer.t

let pp fmt = function
  | OPEN_PAREN -> Fmt.pf fmt "OPEN_PAREN"
  | CLOSE_PAREN -> Fmt.pf fmt "CLOSE_PAREN"
  | COMMA -> Fmt.pf fmt "COMMA"
  | TEXT b ->
      let s = Buffer.contents b in
      Fmt.pf fmt "TEXT(%s)" s
}

rule tokenize acc = shortest
  | '[' {
    tokenize (OPEN_PAREN::acc) lexbuf }
  | ']' {
    tokenize (CLOSE_PAREN::acc) lexbuf }
  | ',' {
    tokenize (COMMA::acc) lexbuf }
  | _+ {
     let text = Lexing.lexeme lexbuf in
     (* Fmt.epr "Got text: %s\n" text; *)
     match acc with
     | TEXT buffer :: _ as acc -> 
         Buffer.add_string buffer text;
         tokenize acc lexbuf 
     | acc ->
         let buffer = Buffer.create 16 in
         Buffer.add_string buffer text;
         let token = TEXT buffer in
         tokenize (token::acc) lexbuf
  }
  | eof { List.rev acc }
