open Lexing
open Printf
module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil

(* In this demo, we have one grammar, stored in the file [parser.mly],
   out of which we create two parsers. Using Menhir's code back-end,
   we get a parser that is optimized for speed, in the module [Parser].
   Using Menhir's table back-end, we get a parser that supports Menhir's
   incremental API, in the module [UnitActionsParser].

   We first attempt to parse using the fast parser [Parser]. If this
   fails, then we cannot straight away produce a good syntax error
   message, because the exception [Parser.Error] is not informative
   enough. In that case, we parse again using [UnitActionsParser].
   When we hit the syntax error, Menhir's incremental API allows us
   to extract information out of the parser's state and stack. This
   lets us produce a good syntax error message.

   As its name suggests, the parser [UnitActionsParser] has empty
   semantic actions. It is created by a rule in the [dune] file.

   Of course, if performance is not crucial, then one can work with
   just one parser, compiled in --table mode. There is no obligation
   to compile the parser twice. *)

(* -------------------------------------------------------------------------- *)

(* This part concerns the code-based parser [Parser]. *)

(* The function call [attempt1 filename] returns normally only if a syntax
   error has occurred while parsing [filename]. In that case, it returns the
   content of the file. *)

let attempt1 filename : string =

  (* Read the file; allocate and initialize a lexing buffer. *)
  let text, lexbuf = L.read filename in
  (* Run the parser. *)
  match Parser.main Lexer.token lexbuf with

  | v ->
      (* Success. The parser has produced a semantic value [v]. *)
      printf "%d\n%!" v;
      exit 0

  | exception Lexer.Error msg ->
      (* A lexical error has occurred. *)
      eprintf "%s%!" msg;
      exit 1

  | exception Parser.Error ->
      (* A syntax error has occurred. *)
      text

(* -------------------------------------------------------------------------- *)

(* This part concerns the table-based parser [UnitActionsParser]. *)

module I = UnitActionsParser.MenhirInterpreter

(* [env checkpoint] extracts a parser environment out of a checkpoint,
   which must be of the form [HandlingError env]. *)

let env checkpoint =
  match checkpoint with
  | I.HandlingError env ->
      env
  | _ ->
      assert false

(* [state checkpoint] extracts the number of the current state out of a
   checkpoint. *)

let state checkpoint : int =
  I.current_state_number (env checkpoint)

(* [show text (pos1, pos2)] displays a range of the input text [text]
   delimited by the positions [pos1] and [pos2]. *)

let show text positions =
  E.extract text positions
  |> E.sanitize
  |> E.compress
  |> E.shorten 20 (* max width 43 *)

(* [get text checkpoint i] extracts and shows the range of the input text that
   corresponds to the [i]-th stack cell. The top stack cell is numbered zero. *)

let get text checkpoint i =
  match I.get i (env checkpoint) with
  | Some (I.Element (_, _, pos1, pos2)) ->
      show text (pos1, pos2)
  | None ->
      (* The index is out of range. This should not happen if [$i]
         keywords are correctly inside the syntax error message
         database. The integer [i] should always be a valid offset
         into the known suffix of the stack. *)
      "???"

(* [succeed v] is invoked when the parser has succeeded and produced a
   semantic value [v]. In our setting, this cannot happen, since the
   table-based parser is invoked only when we know that there is a
   syntax error in the input file. *)

let succeed _v =
  assert false

(* [fail text buffer checkpoint] is invoked when parser has encountered a
   syntax error. *)

let fail text buffer (checkpoint : _ I.checkpoint) =
  (* Indicate where in the input file the error occurred. *)
  let location = L.range (E.last buffer) in
  (* Show the tokens just before and just after the error. *)
  let indication = sprintf "Syntax error %s.\n" (E.show (show text) buffer) in
  (* Fetch an error message from the database. *)
  let message = ParserMessages.message (state checkpoint) in
  (* Expand away the $i keywords that might appear in the message. *)
  let message = E.expand (get text checkpoint) message in
  (* Show these three components. *)
  eprintf "%s%s%s%!" location indication message;
  exit 1

(* [attempt2 filename text] runs the parser. *)

let attempt2 filename text =
  (* Allocate and initialize a lexing buffer. *)
  let lexbuf = L.init filename (Lexing.from_string text) in
  (* Wrap the lexer and lexbuf together into a supplier, that is, a
     function of type [unit -> token * position * position]. *)
  let supplier = I.lexer_lexbuf_to_supplier Lexer.token lexbuf in
  (* Equip the supplier with a two-place buffer that records the positions
     of the last two tokens. This is useful when a syntax error occurs, as
     these are the token just before and just after the error. *)
  let buffer, supplier = E.wrap_supplier supplier in
  (* Fetch the parser's initial checkpoint. *)
  let checkpoint = UnitActionsParser.Incremental.main lexbuf.lex_curr_p in
  (* Run the parser. *)
  (* We do not handle [Lexer.Error] because we know that we will not
     encounter a lexical error during this second parsing run. *)
  I.loop_handle succeed (fail text buffer) supplier checkpoint

(* -------------------------------------------------------------------------- *)

(* In this demo, instead of reading one line at a time as in the other demos,
   we read all of our input in one go. This allows multi-line arithmetic
   expressions. We take a file name on the command line. *)

let () =
  let filename = Sys.argv.(1) in
  let text = attempt1 filename in
  attempt2 filename text
