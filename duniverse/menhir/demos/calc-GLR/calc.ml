open Printf
open Expr

let read (line : string) : uexpr =
  let lexbuf = Lexing.from_string line in
  try
    current := 0; (* reset the counter of allocated nodes *)
    Parser.main Lexer.token lexbuf
  with
  | Lexer.Error msg ->
      eprintf "%s%!" msg;
      exit 1
  | Parser.Error _ ->
      eprintf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start lexbuf);
      exit 1

let diagnostics line (e : uexpr) : unit =
  (* Print the size of this expression. *)
  let n = dag_size e in
  printf "This expression has DAG size %d.\n" n;
  (* Print the number of disjunctions. *)
  printf "The number of disjunction nodes in this expression is %d.\n"
    (disjunctions e);
  (* Show each disjunction. (Of course this is tenable only if the number
     of disjunctions is very small. This number is cubic in the length of
     the input, so the input must be very, very small, or contain a lot
     of parentheses.) *)
  show_disjunctions line e;
  flush stdout

let process (optional_line : string option) =
  match optional_line with
  | None ->
      ()
  | Some line ->
      let e : uexpr = read line in
      diagnostics line e

let rec repeat channel =
  (* Attempt to read one line. *)
  let optional_line, continue = Lexer.line channel in
  process optional_line;
  if continue then
    repeat channel

let () =
  repeat (Lexing.from_channel stdin)
