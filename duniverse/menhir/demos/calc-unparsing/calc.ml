open Printf
open Lexing

(* -------------------------------------------------------------------------- *)

(* Parsing a piece of text. *)

(* We use Menhir's monolithic API. *)

let parse (text : string) : AST.main option =
  let lexbuf = from_string text in
  try
    Some (Parser.main Lexer.token lexbuf)
  with
  | Lexer.Error msg ->
      fprintf stderr "%s%!" msg;
      None
  | Parser.Error ->
      fprintf stderr "At offset %d: syntax error.\n%!" (lexeme_start lexbuf);
      None

(* -------------------------------------------------------------------------- *)

(* Processing an abstract syntax tree. *)

let process (m : AST.main) : unit =
  (* First, evaluate this AST and print its value, just for fun. *)
  let e = m in
  printf "This expression evaluates to %d\n%!" (Eval.eval e);
  (* Then, convert the AST to a DCST. *)
  let m : Parser.DCST.main = AST2DCST.main m in
  (* Convert the DCST to a CST. *)
  match Parser.Settle.main m with
  | None ->
      printf "Unexpected failure: unable to display this expression!\n%!"
  | Some (m : Parser.CST.main) ->
  (* Display this expression as a string. *)
  printf "This expression is printed as follows:\n%s%!"
    (CST2String.main m);
  (* Display this expression as a pretty-printed string. *)
  (* Use various page widths. *)
  [ 10; 20; 40; 80] |> List.iter @@ fun columns ->
    printf "This expression is pretty-printed in %d columns as follows:\n%!"
      columns;
    let d : Document.document = CST2Document.main m in
    Document.ToChannel.pretty 0.8 columns stdout d;
    flush stdout

(* -------------------------------------------------------------------------- *)

(* The interactive loop. *)

let maybe =
  Option.iter

let (>>) f g x =
  f x |> maybe g

let rec repeat channel =
  (* Attempt to read one line. *)
  let optional_line, continue = Lexer.line channel in
  (* Parse and process this line. *)
  optional_line |> maybe (parse >> process);
  (* If this line was nonempty, continue. *)
  if continue then
    repeat channel

let () =
  repeat (from_channel stdin)
