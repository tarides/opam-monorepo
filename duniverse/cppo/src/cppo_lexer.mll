{
open Printf
open Lexing

open Cppo_types
open Cppo_parser

let pos1 lexbuf = lexbuf.lex_start_p
let pos2 lexbuf = lexbuf.lex_curr_p
let loc lexbuf = (pos1 lexbuf, pos2 lexbuf)

let lexer_error lexbuf descr =
  error (loc lexbuf) descr

let new_file lb name =
  lb.lex_curr_p <- { lb.lex_curr_p with pos_fname = name }

let lex_new_lines lb =
  let n = ref 0 in
  let s = lb.lex_buffer in
  for i = lb.lex_start_pos to lb.lex_curr_pos do
    if Bytes.get s i = '\n' then
      incr n
  done;
  let p = lb.lex_curr_p in
  lb.lex_curr_p <-
    { p with
        pos_lnum = p.pos_lnum + !n;
        pos_bol = p.pos_cnum
    }

let count_new_lines lb n =
  let p = lb.lex_curr_p in
  lb.lex_curr_p <-
    { p with
        pos_lnum = p.pos_lnum + n;
        pos_bol = p.pos_cnum
    }

(* must start a new line *)
let update_pos lb p added_chars added_breaks =
  let cnum = p.pos_cnum + added_chars in
  lb.lex_curr_p <-
    { pos_fname = p.pos_fname;
      pos_lnum = p.pos_lnum + added_breaks;
      pos_bol = cnum;
      pos_cnum = cnum }

let set_lnum lb opt_file lnum =
  let p = lb.lex_curr_p in
  let cnum = p.pos_cnum in
  let fname =
    match opt_file with
        None -> p.pos_fname
      | Some file -> file
  in
  lb.lex_curr_p <-
    { pos_fname = fname;
      pos_bol = cnum;
      pos_cnum = cnum;
      pos_lnum = lnum }

let shift lb n =
  let p = lb.lex_curr_p in
  lb.lex_curr_p <- { p with pos_cnum = p.pos_cnum + n }

let read_hexdigit c =
  match c with
      '0'..'9' -> Char.code c - 48
    | 'A'..'F' -> Char.code c - 55
    | 'a'..'z' -> Char.code c - 87
    | _ -> invalid_arg "read_hexdigit"

let read_hex2 c1 c2 =
  Char.chr (read_hexdigit c1 * 16 + read_hexdigit c2)

type env = {
  preserve_quotations : bool;
  mutable lexer : [ `Ocaml | `Test ];
  mutable line_start : bool;
  mutable in_directive : bool; (* true while processing a directive, until the
                                  final newline *)
  buf : Buffer.t;
  mutable token_start : Lexing.position;
  lexbuf : Lexing.lexbuf;
}

let new_line env =
  env.line_start <- true;
  count_new_lines env.lexbuf 1

let clear env = Buffer.clear env.buf

let add env s =
  env.line_start <- false;
  Buffer.add_string env.buf s

let add_char env c =
  env.line_start <- false;
  Buffer.add_char env.buf c

let get env = Buffer.contents env.buf

let long_loc e = (e.token_start, pos2 e.lexbuf)

let cppo_directives = [
  "def";
  "define";
  "elif";
  "else";
  "enddef";
  "endif";
  "error";
  "if";
  "ifdef";
  "ifndef";
  "include";
  "undef";
  "warning";
]

let is_reserved_directive =
  let tbl = Hashtbl.create 20 in
  List.iter (fun s -> Hashtbl.add tbl s ()) cppo_directives;
  fun s -> Hashtbl.mem tbl s

let assert_ocaml_lexer e lexbuf =
  match e.lexer with
  | `Test ->
      lexer_error lexbuf "Syntax error in boolean expression"
  | `Ocaml ->
      ()

}

(* standard character classes used for macro identifiers *)
let upper = ['A'-'Z']
let lower = ['a'-'z']
let digit = ['0'-'9']

let identchar = upper | lower | digit | [ '_' '\'' ]


(* iso-8859-1 upper and lower characters used for ocaml identifiers *)
let oc_upper = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let oc_lower = ['a'-'z' '\223'-'\246' '\248'-'\255']
let oc_identchar = oc_upper | oc_lower | digit | ['_' '\'']

(*
  Identifiers: ident is used for macro names and is a subset of oc_ident
*)
let ident = (lower | '_' identchar | upper) identchar*
let oc_ident = (oc_lower | '_' oc_identchar | oc_upper) oc_identchar*



let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let oct = ['0'-'7']
let bin = ['0'-'1']

let operator_char =
  [ '!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let infix_symbol =
  ['=' '<' '>' '@' '^' '|' '&' '+' '-' '*' '/' '$' '%'] operator_char*
let prefix_symbol = ['!' '?' '~'] operator_char*

let blank = [ ' ' '\t' ]
let space = [ ' ' '\t' '\r' '\n' ]

let line = ( [^'\n'] | '\\' ('\r'? '\n') )* ('\n' | eof)

let dblank0 = (blank | '\\' '\r'? '\n')*
let dblank1 = blank (blank | '\\' '\r'? '\n')*

(* We use two different lexers: [ocaml_token] is used for ordinary
   OCaml tokens; [test_token] is used inside the Boolean expression
   that follows an #if directive. The field [e.lexer] indicates which
   lexer is currently active. *)

rule line e = parse

  (* A directive begins with a # symbol, which must appear at the beginning
     of a line. *)
  | blank* "#" as s
    {
      assert_ocaml_lexer e lexbuf;
      clear e;
      (* We systematically set [e.token_start], so that [long_loc e] will
         correctly produce the location of the last token. *)
      e.token_start <- pos1 lexbuf;
      if e.line_start then (
        e.in_directive <- true;
        add e s;
        e.line_start <- false;
        directive e lexbuf
      )
      else
        TEXT (loc lexbuf, false, s)
    }

  | ""
      { clear e;
        (* We systematically set [e.token_start], so that [long_loc e] will
           correctly produce the location of the last token. *)
        e.token_start <- pos1 lexbuf;
        match e.lexer with
        | `Ocaml -> ocaml_token e lexbuf
        | `Test -> test_token e lexbuf }

and directive e = parse

  (* If #define <name> is immediately followed with an opening parenthesis
     (without any blank space) then this is interpreted as a parameterized
     macro definition. The formal parameters are parsed by the lexer. *)
  | blank* "define" dblank1 (ident as id) "("
      { let xs = formals1 lexbuf in
        assert (xs <> []);
        DEF (long_loc e, id, xs) }

  (* If #define <name> is not followed with an opening parenthesis then this
     is interpreted as an ordinary (non-parameterized) macro definition. *)
  | blank* "define" dblank1 (ident as id)
      { let xs = [] in
        DEF (long_loc e, id, xs) }

  (* #def is identical to #define, except it does not set [e.in_directive],
     so backslashes and newlines do not receive special treatment. The
     end of the macro definition must be explicitly signaled by #enddef. *)
  | blank* "def" dblank1 (ident as id) "("
      { e.in_directive <- false;
        let xs = formals1 lexbuf in
        assert (xs <> []);
        DEF (long_loc e, id, xs) }
  | blank* "def" dblank1 (ident as id)
      { e.in_directive <- false;
        let xs = [] in
        DEF (long_loc e, id, xs) }

  (* #enddef ends a definition, which (we expect) has been opened by #def.
     Because we use the same pair of tokens, namely [DEF] and [ENDEF], for
     both kinds of definitions (#define and #def), it is in fact possible to
     begin a definition with #define and end it with #enddef. We do not
     document this fact, and users should not rely on it. *)
  | blank* "enddef"
      { blank_until_eol e lexbuf;
        ENDEF (long_loc e) }

  | blank* "undef" dblank1 (ident as id)
      { blank_until_eol e lexbuf;
        UNDEF (long_loc e, id) }

  (* #scope opens a block, which we expect will be ended by #endscope.
     It does not set [e.in_directive], so backslashes and newlines do
     not receive special treatment. *)
  | blank* "scope" dblank0
      { e.in_directive <- false;
        SCOPE (long_loc e) }

  (* #endscope ends a block. *)
  | blank* "endscope"
      { blank_until_eol e lexbuf;
        ENDSCOPE (long_loc e) }

  | blank* "if" dblank1    { e.lexer <- `Test;
                             IF (long_loc e) }
  | blank* "elif" dblank1  { e.lexer <- `Test;
                             ELIF (long_loc e) }

  | blank* "ifdef" dblank1 (ident as id)
      { blank_until_eol e lexbuf;
        IFDEF (long_loc e, `Defined id) }

  | blank* "ifndef" dblank1 (ident as id)
      { blank_until_eol e lexbuf;
        IFDEF (long_loc e, `Not (`Defined id)) }

  | blank* "ext" dblank1 (ident as id)
      { blank_until_eol e lexbuf;
        clear e;
        let s = read_ext e lexbuf in
        EXT (long_loc e, id, s) }

  | blank* "define" dblank1 oc_ident
  | blank* "undef" dblank1 oc_ident
  | blank* "ifdef" dblank1 oc_ident
  | blank* "ifndef" dblank1 oc_ident
  | blank* "ext" dblank1 oc_ident
      { error (loc lexbuf)
          "Identifiers containing non-ASCII characters \
           may not be used as macro identifiers" }

  | blank* "else"
      { blank_until_eol e lexbuf;
        ELSE (long_loc e) }

  | blank* "endif"
      { blank_until_eol e lexbuf;
        ENDIF (long_loc e) }

  | blank* "include" dblank0 '"'
      { clear e;
        eval_string e lexbuf;
        blank_until_eol e lexbuf;
        INCLUDE (long_loc e, get e) }

  | blank* "error" dblank0 '"'
      { clear e;
        eval_string e lexbuf;
        blank_until_eol e lexbuf;
        ERROR (long_loc e, get e) }

  | blank* "warning" dblank0 '"'
      { clear e;
        eval_string e lexbuf;
        blank_until_eol e lexbuf;
        WARNING (long_loc e, get e) }

  | blank* (['0'-'9']+ as lnum) dblank0 '\r'? '\n'
      { e.in_directive <- false;
        new_line e;
        let here = long_loc e in
        let fname = None in
        let lnum = int_of_string lnum in
        (* Apply line directive regardless of possible #if condition. *)
        set_lnum lexbuf fname lnum;
        LINE (here, None, lnum) }

  | blank* (['0'-'9']+ as lnum) dblank0 '"'
      { clear e;
        eval_string e lexbuf;
        blank_until_eol e lexbuf;
        let here = long_loc e in
        let fname = Some (get e) in
        let lnum = int_of_string lnum in
        (* Apply line directive regardless of possible #if condition. *)
        set_lnum lexbuf fname lnum;
        LINE (here, fname, lnum) }

  | blank*
      { e.in_directive <- false;
        add e (lexeme lexbuf);
        TEXT (long_loc e, true, get e) }

  | blank* (['a'-'z']+ as s)
      { if is_reserved_directive s then
          error (loc lexbuf) "cppo directive with missing or wrong arguments";
        e.in_directive <- false;
        add e (lexeme lexbuf);
        TEXT (long_loc e, false, get e) }


and blank_until_eol e = parse
    blank* eof
  | blank* '\r'? '\n' { new_line e;
                        e.in_directive <- false }
  | ""                { lexer_error lexbuf "syntax error in directive" }

and read_ext e = parse
    blank* "#" blank* "endext" blank* ('\r'? '\n' | eof)
      { let s = get e in
        clear e;
        new_line e;
        e.in_directive <- false;
        s }

  | (blank* as a) "\\" ("#" blank* "endext" blank* '\r'? '\n' as b)
      { add e a;
        add e b;
        new_line e;
        read_ext e lexbuf }

  | [^'\n']* '\n' as x
      { add e x;
        new_line e;
        read_ext e lexbuf }

  | eof
      { lexer_error lexbuf "End of file within #ext ... #endext" }

and ocaml_token e = parse
    "__LINE__"
      { e.line_start <- false;
        CURRENT_LINE (loc lexbuf) }

  | "__FILE__"
      { e.line_start <- false;
        CURRENT_FILE (loc lexbuf) }

  | ident as s
      { e.line_start <- false;
        IDENT (loc lexbuf, s) }

  | oc_ident as s
      { e.line_start <- false;
        TEXT (loc lexbuf, false, s) }

  | ident as s "("
      { e.line_start <- false;
        FUNIDENT (loc lexbuf, s) }

  | "'\n'"
  | "'\r\n'"
      { new_line e;
        TEXT (loc lexbuf, false, lexeme lexbuf) }

  | "("       { e.line_start <- false; OP_PAREN (loc lexbuf) }
  | ")"       { e.line_start <- false; CL_PAREN (loc lexbuf) }
  | ","       { e.line_start <- false; COMMA (loc lexbuf) }

  | "\\)"     { e.line_start <- false; TEXT (loc lexbuf, false, " )") }
  | "\\,"     { e.line_start <- false; TEXT (loc lexbuf, false, " ,") }
  | "\\("     { e.line_start <- false; TEXT (loc lexbuf, false, " (") }
  | "\\#"     { e.line_start <- false; TEXT (loc lexbuf, false, " #") }

  | '`'
  | "!=" | "#" | "&" | "&&" | "(" |  "*" | "+" | "-"
  | "-." | "->" | "." | ".. :" | "::" | ":=" | ":>" | ";" | ";;" | "<"
  | "<-" | "=" | ">" | ">]" | ">}" | "?" | "??" | "[" | "[<" | "[>" | "[|"
  | "]" | "_" | "`" | "{" | "{<" | "|" | "|]" | "}" | "~"
  | ">>"
  | prefix_symbol
  | infix_symbol
  | "'" ([^ '\'' '\\']
         | '\\' (_ | digit digit digit | 'x' hex hex)) "'"

      { e.line_start <- false;
        TEXT (loc lexbuf, false, lexeme lexbuf) }

  | blank+
      { TEXT (loc lexbuf, true, lexeme lexbuf) }

  | '\\' ('\r'? '\n' as nl)

      {
        new_line e;
        if e.in_directive then
          TEXT (loc lexbuf, true, nl)
        else
          TEXT (loc lexbuf, false, lexeme lexbuf)
      }

  | '\r'? '\n'
      {
        new_line e;
        if e.in_directive then (
          e.in_directive <- false;
          ENDEF (loc lexbuf)
        )
        else
          TEXT (loc lexbuf, true, lexeme lexbuf)
      }

  | "(*"
      { clear e;
        add e "(*";
        e.token_start <- pos1 lexbuf;
        comment (loc lexbuf) e 1 lexbuf }

  | '"'
      { clear e;
        add e "\"";
        e.token_start <- pos1 lexbuf;
        string e lexbuf;
        e.line_start <- false;
        TEXT (long_loc e, false, get e) }

  | "<:"
  | "<<"
      { if e.preserve_quotations then (
          clear e;
          add e (lexeme lexbuf);
          e.token_start <- pos1 lexbuf;
          quotation e lexbuf;
          e.line_start <- false;
          TEXT (long_loc e, false, get e)
        )
        else (
          e.line_start <- false;
          TEXT (loc lexbuf, false, lexeme lexbuf)
        )
      }


  | '-'? ( digit (digit | '_')*
         | ("0x"| "0X") hex (hex | '_')*
         | ("0o"| "0O") oct (oct | '_')*
         | ("0b"| "0B") bin (bin | '_')* )

  | '-'? digit (digit | '_')* ('.' (digit | '_')* )?
      (['e' 'E'] ['+' '-']? digit (digit | '_')* )?
      { e.line_start <- false;
        TEXT (loc lexbuf, false, lexeme lexbuf) }

  | blank+
      { TEXT (loc lexbuf, true, lexeme lexbuf) }

  | _
      { e.line_start <- false;
        TEXT (loc lexbuf, false, lexeme lexbuf) }

  (* At the end of the file, the lexer normally produces EOF. However,
     if we are currently inside a definition (opened by #define) then
     the lexer produces ENDEF followed by EOF. *)
  | eof
      { if e.in_directive then (e.in_directive <- false; ENDEF (loc lexbuf))
        else EOF }


and comment startloc e depth = parse
    "(*"
      { add e "(*";
        comment startloc e (depth + 1) lexbuf }

  | "*)"
      { let depth = depth - 1 in
        add e "*)";
        if depth > 0 then
          comment startloc e depth lexbuf
        else (
          e.line_start <- false;
          TEXT (long_loc e, false, get e)
        )
      }
  | '"'
      { add_char e '"';
        string e lexbuf;
        comment startloc e depth lexbuf }

  | "'\n'"
  | "'\r\n'"
      { new_line e;
        add e (lexeme lexbuf);
        comment startloc e depth lexbuf }

  | "'" ([^ '\'' '\\']
         | '\\' (_ | digit digit digit | 'x' hex hex)) "'"
      { add e (lexeme lexbuf);
        comment startloc e depth lexbuf }

  | '\r'? '\n'
      {
        new_line e;
        add e (lexeme lexbuf);
        comment startloc e depth lexbuf
      }

  | [^'(' '*' '"' '\'' '\r' '\n']+
      {
        add e (lexeme lexbuf);
        comment startloc e depth lexbuf
      }

  | _
      { add e (lexeme lexbuf);
        comment startloc e depth lexbuf }

  | eof
      { error startloc "Unterminated comment reaching the end of file" }


and string e = parse
    '"'
      { add_char e '"' }

  | "\\\\"
  | '\\' '"'
      { add e (lexeme lexbuf);
        string e lexbuf }

  | '\\' '\r'? '\n'
      {
        add e (lexeme lexbuf);
        new_line e;
        string e lexbuf
      }

  | '\r'? '\n'
      {
          add e (lexeme lexbuf);
          new_line e;
          string e lexbuf
      }

  | _ as c
      { add_char e c;
        string e lexbuf }

  | eof
      { }


and eval_string e = parse
    '"'
      {  }

  | '\\' (['\'' '\"' '\\'] as c)
      { add_char e c;
        eval_string e lexbuf }

  | '\\' '\r'? '\n'
      { assert e.in_directive;
        eval_string e lexbuf }

  | '\r'? '\n'
      { assert e.in_directive;
        lexer_error lexbuf "Unterminated string literal" }

  | '\\' (digit digit digit as s)
      { add_char e (Char.chr (int_of_string s));
        eval_string e lexbuf }

  | '\\' 'x' (hex as c1) (hex as c2)
      { add_char e (read_hex2 c1 c2);
        eval_string e lexbuf }

  | '\\' 'b'
      { add_char e '\b';
        eval_string e lexbuf }

  | '\\' 'n'
      { add_char e '\n';
        eval_string e lexbuf }

  | '\\' 'r'
      { add_char e '\r';
        eval_string e lexbuf }

  | '\\' 't'
      { add_char e '\t';
        eval_string e lexbuf }

  | [^ '\"' '\\']+
      { add e (lexeme lexbuf);
        eval_string e lexbuf }

  | eof
      { lexer_error lexbuf "Unterminated string literal" }


and quotation e = parse
    ">>"
      { add e ">>" }

  | "\\>>"
      { add e "\\>>";
        quotation e lexbuf }

  | '\\' '\r'? '\n'
      {
        if e.in_directive then (
          new_line e;
          quotation e lexbuf
        )
        else (
          add e (lexeme lexbuf);
          new_line e;
          quotation e lexbuf
        )
      }

  | '\r'? '\n'
      {
        if e.in_directive then
          lexer_error lexbuf "Unterminated quotation"
        else (
          add e (lexeme lexbuf);
          new_line e;
          quotation e lexbuf
        )
      }

  | [^'>' '\\' '\r' '\n']+
      { add e (lexeme lexbuf);
        quotation e lexbuf }

  | eof
      { lexer_error lexbuf "Unterminated quotation" }

and test_token e = parse
    "true"    { TRUE }
  | "false"   { FALSE }
  | "defined" { DEFINED }
  | "("       { OP_PAREN (loc lexbuf) }
  | ")"       { CL_PAREN (loc lexbuf) }
  | "&&"      { AND }
  | "||"      { OR }
  | "not"     { NOT }
  | "="       { EQ }
  | "<"       { LT }
  | ">"       { GT }
  | "<>"      { NE }
  | "<="      { LE }
  | ">="      { GE }

  | '-'? ( digit (digit | '_')*
         | ("0x"| "0X") hex (hex | '_')*
         | ("0o"| "0O") oct (oct | '_')*
         | ("0b"| "0B") bin (bin | '_')* )
      { let s = Lexing.lexeme lexbuf in
        try INT (Int64.of_string s)
        with _ ->
          error (loc lexbuf)
            (sprintf "Integer constant %s is out the valid range for int64" s)
      }

  | "+"       { PLUS }
  | "-"       { MINUS }
  | "*"       { STAR }
  | "/"       { SLASH (loc lexbuf) }
  | "mod"     { MOD (loc lexbuf) }
  | "lsl"     { LSL }
  | "lsr"     { LSR }
  | "asr"     { ASR }
  | "land"    { LAND }
  | "lor"     { LOR }
  | "lxor"    { LXOR }
  | "lnot"    { LNOT }

  | ","       { COMMA (loc lexbuf) }

  | ident
      { IDENT (loc lexbuf, lexeme lexbuf) }

  | blank+                   { test_token e lexbuf }
  | '\\' '\r'? '\n'          { new_line e;
                               test_token e lexbuf }
  | '\r'? '\n'
  | eof        { assert e.in_directive;
                 e.in_directive <- false;
                 new_line e;
                 e.lexer <- `Ocaml;
                 ENDTEST (loc lexbuf) }
  | _          { error (loc lexbuf)
                   (sprintf "Invalid token %s" (Lexing.lexeme lexbuf)) }


(* Parse just an int or a tuple of ints *)
and int_tuple = parse
  | space* (([^'(']#space)+ as s) space* eof
                      { [Int64.of_string s] }

  | space* "("        { int_tuple_content lexbuf }

  | eof | _           { failwith "Not an int nor a tuple" }

and int_tuple_content = parse
  | space* (([^',' ')']#space)+ as s) space* ","
                      { let x = Int64.of_string s in
                        x :: int_tuple_content lexbuf }

  | space* (([^',' ')']#space)+ as s) space* ")" space* eof
                      { [Int64.of_string s] }

(* -------------------------------------------------------------------------- *)

(* Lists of formal macro parameters. *)

(* [formals1] recognizes a nonempty comma-separated list of formal macro
   parameters, ended with a closing parenthesis. *)

and formals1 = parse
  | blank+
      { formals1 lexbuf }
  | ")"
      { lexer_error lexbuf "A macro must have at least one formal parameter" }
  | ""
      { let x = formal lexbuf in
        formals0 [x] lexbuf }

(* [formals0 xs] recognizes a possibly empty list of comma-preceded formal
   macro parameters, ended with a closing parenthesis.
   [xs] is the accumulator. *)

and formals0 xs = parse
  | blank+
      { formals0 xs lexbuf }
  | ")"
      { List.rev xs }
  | ","
      { let x = formal lexbuf in
        formals0 (x :: xs) lexbuf }
  | _
  | eof
      { lexer_error lexbuf "Invalid formal parameter list: expected ',' or ')'" }

(* [formal] recognizes one formal macro parameter. It is either an identifier
   [x] or an identifier annotated with a shape [x : sh]. *)

and formal = parse
  | blank+
      { formal lexbuf }
  | (ident as x) blank* ":"
      { (x, shape lexbuf) }
  | ident as x
      { (x, base) }
  | _
  | eof
      { lexer_error lexbuf "Invalid formal parameter: expected an identifier" }

(* [shape] recognizes a shape. *)

and shape = parse
  | blank+
      { shape lexbuf }
  | "."
      (* The base shape can be written [] but we also allow .
         as a more readable alternative. *)
      { base }
  | "["
      { Shape (shapes [] lexbuf) }
  | _
  | eof
      { lexer_error lexbuf "Invalid shape: expected '.' or '[' or ']'" }
      (* A closing square bracket is valid if an opening square bracket
         has been entered. We could keep track of this via an additional
         parameter, but that seems overkill. *)

(* [shapes shs] recognizes a possibly empty list of shapes, ended with
   a closing square bracket. There is no separator between shapes.
   [shs] is the accumulator. *)

and shapes shs = parse
  | blank+
      { shapes shs lexbuf }
  | "]"
      { List.rev shs }
  | ""
      { let sh = shape lexbuf in
        shapes (sh :: shs) lexbuf }

(* -------------------------------------------------------------------------- *)

(* Initialization. *)

{
  let init ~preserve_quotations file lexbuf =
    new_file lexbuf file;
    {
      preserve_quotations = preserve_quotations;
      lexer = `Ocaml;
      line_start = true;
      in_directive = false;
      buf = Buffer.create 200;
      token_start = Lexing.dummy_pos;
      lexbuf = lexbuf;
    }

  let int_tuple_of_string s =
    try Some (int_tuple (Lexing.from_string s))
    with _ -> None
}
