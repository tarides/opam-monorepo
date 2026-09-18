open Printf
open Parser
open Parser.Ast
open Parser.MenhirLibParser.Inter
module L = MenhirLib.LexerUtil


(* run the parser *)
let run filename =
  let text, lexbuf = L.read filename in
  let rec compute_buffer () =
    let loop t = Buf_cons (t, Lazy.from_fun compute_buffer) in
    loop (Lexer.token lexbuf)
  in
  Parser.parse_expr
    (ExtrOcamlIntConv.nat_of_int 20)
    (Lazy.from_fun compute_buffer)

(* very simple evaluation of our expressions *)
let rec eval ast =
  match ast with
  | Num s -> int_of_string s
  | Add (a,b) -> (eval a) + (eval b)
  | Sub (a,b) -> (eval a) - (eval b)
  | Mul (a,b) -> (eval a) * (eval b)
  | Div (a,b) -> (eval a) / (eval b)

(* ------- Error Message Code ------- *)
(* retrive location information from our tokens,
 * provided by lexer
*)
let position token =
  match token with
  | NUM sp -> snd sp
  | ADD p | SUB p | MUL p
  | DIV p | LPAREN p
  | RPAREN p | EOF p -> p

(* pretty-print tokens *)
let format token =
  match token with
  | NUM sp -> fst sp
  | ADD _ -> "+"
  | SUB _ -> "-"
  | MUL _ -> "*"
  | DIV _ -> "/"
  | RPAREN _ -> ")"
  | LPAREN _ -> "("
  | EOF _ -> ""

(* get an OCaml int corresponding to the error-throwing state *)
let state_num s =
  let rocq_num = Parser.Aut.coq_N_of_state s in
  let state = ExtrOcamlIntConv.int_of_n rocq_num in
  state

let handle_syntax_error state token =
  (* Indicate where in the input file the error occurred. *)
  let positions = position token in
  let location = L.range positions in
  (* Show the token causing the error. *)
  let indication = sprintf "Syntax error at '%s'.\n" (format token) in
  (* Fetch an error message from the database. *)
  let message = ParserMessages.message (state_num state) in
  (* Show these three components. *)
  eprintf "%s%s%s%!" location indication message;
  exit 1

(* ------- Driver Code ------- *)
(* Similar to calc-syntax-errors, we take a file name on the command line. *)
let () =
  let filename = Sys.argv.(1) in
  let p =
    match (run filename) with
    | Parser.MenhirLibParser.Inter.Fail_pr_full (state, token) ->
        handle_syntax_error state token
    | Parser.MenhirLibParser.Inter.Timeout_pr ->
        assert false
    | Parser.MenhirLibParser.Inter.Parsed_pr (ast, _) ->
        ast
  in
  Printf.printf "%d\n" (eval p)
