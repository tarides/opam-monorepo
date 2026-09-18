(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

{

open Lexing
open Parser
open Keyword
open Attribute
open Located

(* ------------------------------------------------------------------------ *)

(* Failing. *)

(* [blame range format ...] reports an error at [range]. *)

let blame range =
  Report.Just.error [range]

(* [error lexbuf format ...] reports an error at the current token. *)

let error lexbuf =
  blame (Range.current lexbuf)

(* ------------------------------------------------------------------------ *)

(* [int_of_string] raises [Failure] if its argument is too large. This is
   not a problem in practice, but causes false positives when fuzzing
   Menhir. We hide the problem by failing gracefully. *)

let int_of_string range i =
  try
    int_of_string i
  with Failure _ ->
    blame range "unreasonably large integer."

(* ------------------------------------------------------------------------ *)

(* This wrapper saves the current lexeme start, invokes its argument,
   and restores it. This allows transmitting better positions to the
   parser. *)

let savestart lexbuf f =
  let startp = lexbuf.lex_start_p in
  let token = f lexbuf in
  lexbuf.lex_start_p <- startp;
  token

(* ------------------------------------------------------------------------ *)

(* Overwrites an old character with a new one at a specified
   offset in a [bytes] buffer. *)

let overwrite content offset c1 c2 =
  assert (Bytes.get content offset = c1);
  Bytes.set content offset c2

(* ------------------------------------------------------------------------ *)

(* Keyword recognition and construction. *)

(* A monster is a spot where we have identified a keyword in concrete syntax.
   We describe a monster as an object with the following methods: *)

type monster = {

  (* The position of the monster. *)
  range: Range.range;

  (* This method is passed an array of (optional) names for the producers,
     that is, the elements of the production's right-hand side. It is also
     passed a flag which tells whether [$i] syntax is allowed or disallowed.
     It may perform some checks and is allowed to fail. *)
  check: check;

  (* This method transforms the keyword (in place) into a conventional
     OCaml identifier. This is done by replacing '$', '(', and ')' with
     '_'. Bloody. The arguments are [ofs1] and [content]. [ofs1] is the
     offset where [content] begins in the source file. *)
  transform: int -> bytes -> unit;

  (* This is the keyword, in abstract syntax. *)
  keyword: keyword option;

  (* If this is a [$i] monster, then the identifier [_i] is stored here. *)
  oid: string option;

}

and check =
  [`DollarsDisallowed | `DollarsAllowed] ->
  string option array ->
  unit

(* No check. *)

let none : check =
  fun _ _ -> ()

(* ------------------------------------------------------------------------ *)

(* We check that every [$i] is within range. Also, we forbid using [$i]
   when a producer has been given a name; this is bad style and may be
   a mistake. (Plus, this simplifies our life, as we rewrite [$i] to [_i],
   and we would have to rewrite it to a different identifier otherwise.) *)

let check_dollar range i : check = fun dollars producers ->
  (* If [i] is out of range, say so. *)
  if not (0 <= i - 1 && i - 1 < Array.length producers) then
    blame range "$%d refers to a nonexistent symbol." i;
  (* If [$i] could be referred to via a name, say so. *)
  producers.(i - 1) |> Option.iter (fun x ->
    blame range "please do not say: $%d. Instead, say: %s." i x
  );
  (* If [$i] syntax is disallowed, say so. *)
  match dollars with
  | `DollarsDisallowed ->
      blame range "please do not use $%d. Instead, name this value." i
  | `DollarsAllowed ->
      ()

(* We check that every reference to a producer [x] in a position keyword,
   such as [$startpos(x)], exists. *)

let check_producer range x : check = fun _ producers ->
  if not (List.mem (Some x) (Array.to_list producers)) then
    blame range "%s refers to a nonexistent symbol." x

(* ------------------------------------------------------------------------ *)

(* The [$i] monster. *)

let dollar range i : monster =
  let check : check = check_dollar range i
  and transform ofs1 content =
    (* [$i] is replaced with [_i]. Thus, it is no longer a keyword. *)
    let pos = Range.startp range in
    let ofs = pos.pos_cnum - ofs1 in
    overwrite content ofs '$' '_'
  and keyword =
    None
  and oid =
    Some (Printf.sprintf "_%d" i)
  in
  { range; check; transform; keyword; oid }

(* ------------------------------------------------------------------------ *)

(* The position-keyword monster. The most horrible of all. *)

let position range
  (where : string)
  (flavor : string)
  (i : string option) (x : string option)
=
  let check_no_parameter () =
    if i <> None || x <> None then
      blame range "$%s%s does not take a parameter." where flavor
  in
  let ofslpar = (* offset of the opening parenthesis, if there is one *)
    1 + (* for the initial "$" *)
    String.length where +
    3   (* for "pos" or "ofs" or "loc" *)
  in
  let where =
    match where with
    | "symbolstart"
    | "s"           -> check_no_parameter(); WhereSymbolStart
    | "start"       -> WhereStart
    | "end"         -> WhereEnd
    | ""            -> WhereStart
    | _             -> assert false
  in
  let flavor =
    match flavor with
    | "pos"   -> FlavorPosition
    | "ofs"   -> FlavorOffset
    | "loc"   -> FlavorLocation
    | _       -> assert false
  in
  let subject, check =
    match i, x with
    | Some i, None ->
        let ii = int_of_string range i in
        if ii = 0 && where = WhereEnd then
          (* [$endpos($0)] *)
          Before, none
        else
          (* [$startpos($i)] is rewritten to [$startpos(_i)]. *)
          RightNamed ("_" ^ i), check_dollar range ii
    | None, Some x ->
        (* [$startpos(x)] *)
        RightNamed x, check_producer range x
    | None, None ->
        (* [$startpos] *)
        Left, none
    | Some _, Some _ ->
        assert false
  in
  let transform ofs1 content =
    let pos = Range.startp range in
    let ofs = pos.pos_cnum - ofs1 in
    overwrite content ofs '$' '_';
    let ofslpar = ofs + ofslpar in
    match i, x with
    | None, Some x ->
        overwrite content ofslpar '(' '_';
        overwrite content (ofslpar + 1 + String.length x) ')' '_'
    | Some i, None ->
        overwrite content ofslpar '(' '_';
        overwrite content (ofslpar + 1) '$' '_';
        overwrite content (ofslpar + 2 + String.length i) ')' '_'
    | _, _ ->
        ()
  in
  let keyword =
    Some (Position (subject, where, flavor))
  and oid =
    None
  in
  { range; check; transform; keyword; oid }

(* ------------------------------------------------------------------------ *)

(* In an OCaml header, there should be no monsters. This is just a sanity
   check. *)

let no_monsters monsters =
  match monsters with
  | [] ->
      ()
  | monster :: _ ->
      blame monster.range
        "a Menhir keyword cannot be used in an OCaml header."

(* ------------------------------------------------------------------------ *)

(* Gathering all of the identifiers in an array of optional identifiers. *)

let gather_oid xs oid =
  match oid with
  | Some x ->
      StringSet.add x xs
  | None ->
      xs

let gather_oids oids =
  Array.fold_left gather_oid StringSet.empty oids

(* Gathering all of the [oid] identifiers in a list of monsters. *)

let gather_monsters monsters =
  List.fold_left (fun xs monster ->
    gather_oid xs monster.oid
  ) StringSet.empty monsters

(* ------------------------------------------------------------------------ *)

(* Create a fragment. *)

let fragment pos1 pos2 =
  (* Read the specified chunk of the file. *)
  let content = InputFile.chunk (pos1, pos2) in
  (* Construct a fragment. *)
  locate (Range.make (pos1, pos2)) content

(* ------------------------------------------------------------------------ *)

(* Create a semantic action. *)

let transform ofs1 content monsters : string =
  match monsters with
  | [] ->
      content
  | _ :: _ ->
      let content = Bytes.of_string content in
      List.iter (fun monster -> monster.transform ofs1 content) monsters;
      Bytes.unsafe_to_string content

(* This reference can be modified from the outside. Ugly but convenient.
   It determines the priority of the semantic actions that we construct. *)
let priority =
  ref 0

let make_action pos1 pos2 monsters dollars producers =
  (* Check that the monsters are well-formed. *)
  List.iter (fun monster -> monster.check dollars producers) monsters;
  (* Gather all of the identifiers that the semantic action may use to refer
     to a semantic value. This includes the identifiers that are explicitly
     bound by the user (these appear in the array [producers]) and the
     identifiers [_i] when the semantic action uses [$i]. *)
  let ids = StringSet.union (gather_oids producers) (gather_monsters monsters) in
  (* Collect their keywords. *)
  let keywords = MList.filter_map (fun monster -> monster.keyword) monsters in
  (* Read the specified chunk of the file. *)
  let content = InputFile.chunk (pos1, pos2) in
  (* Transform the monsters, if there are any. *)
  let ofs1 = pos1.pos_cnum in
  let content = transform ofs1 content monsters in
  (* Construct a fragment. *)
  let fragment = locate (Range.make (pos1, pos2)) content in
  (* Add parentheses to delimit the semantic action. *)
  let fragment = Located.parenthesize fragment in
  (* Build a semantic action. *)
  Action.make !priority ids keywords (IL.ETextual fragment)

(* ------------------------------------------------------------------------ *)

(* OCaml's reserved words. *)

let table words =
  let table = Hashtbl.create 149 in
  List.iter (fun word -> Hashtbl.add table word ()) words;
  table

let reserved =
  table [
    "and";
    "as";
    "assert";
    "begin";
    "class";
    "constraint";
    "do";
    "done";
    "downto";
    "effect";
    "else";
    "end";
    "exception";
    "external";
    "false";
    "for";
    "fun";
    "function";
    "functor";
    "if";
    "in";
    "include";
    "inherit";
    "initializer";
    "lazy";
    "let";
    "match";
    "method";
    "module";
    "mutable";
    "new";
    "nonrec";
    "object";
    "of";
    "open";
    "or";
    "private";
    "rec";
    "sig";
    "struct";
    "then";
    "to";
    "true";
    "try";
    "type";
    "val";
    "virtual";
    "when";
    "while";
    "with";
    "lor";
    "lxor";
    "mod";
    "land";
    "lsl";
    "lsr";
    "asr";
  ]

(* ------------------------------------------------------------------------ *)

(* Menhir's percent-directives. *)

let table directives =
  let table = Hashtbl.create 149 in
  List.iter (fun (word, token) -> Hashtbl.add table word token) directives;
  table

let directives =
  table [
    "token", TOKEN;
    "type", TYPE;
    "left", LEFT;
    "right", RIGHT;
    "nonassoc", NONASSOC;
    "start", START;
    "prec", PREC;
    "public", PUBLIC;
    "parameter", PARAMETER;
    "inline", INLINE;
    "attribute", PERCENTATTRIBUTE;
    "on_error_reduce", ON_ERROR_REDUCE;
    "merge", MERGE;
  ]

}

(* ------------------------------------------------------------------------ *)

(* Patterns. *)

let newline = ('\010' | '\013' | "\013\010")

let whitespace = [ ' ' '\t' ]

let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']

let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']

let identchar = ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '0'-'9'] (* '\'' forbidden *)

let attributechar = identchar | '.'

let subject =
  '$' (['0'-'9']+ as i)
| ((lowercase identchar*) as x)

let poskeyword =
  '$'
  (
    (("symbolstart" | "start" | "end") as where) (("pos" | "ofs") as flavor)
  | (("s" | "") as where) ("loc" as flavor)
  )
  ( '(' subject ')' )?

let previouserror =
  "$previouserror"

let syntaxerror =
  "$syntaxerror"

(* ------------------------------------------------------------------------ *)

(* The lexer. *)

rule main = parse
| "%" (identchar+ as directive)
    { try Hashtbl.find directives directive
      with Not_found -> error lexbuf "unknown directive: %s." directive }
| "%%"
    { (* The token [PERCENTPERCENT] carries a fragment that contains
         everything that follows %% in the input file. This string
         must be created lazily. The parser decides (based on the
         context) whether this fragment is needed. If it is indeed
         needed, then constructing this fragment drives the lexer
         to the end of the file. *)
      PERCENTPERCENT (lazy (
        let openingpos = lexeme_end_p lexbuf in
        let closingpos = finish lexbuf in
        fragment openingpos closingpos
      )) }
| ";"
    { SEMI }
| ":"
    { COLON }
| ","
    { COMMA }
| "="
    { EQUAL }
| "("
    { LPAREN }
| ")"
    { RPAREN }
| "|"
    { BAR }
| "?"
    { QUESTION }
| "*"
    { STAR }
| "+"
    { PLUS }
| "~"
    { TILDE }
| "_"
    { UNDERSCORE }
| ":="
    { COLONEQUAL }
| "=="
    { EQUALEQUAL }
| "let"
    { LET }
| (lowercase identchar *) as id
    { if Hashtbl.mem reserved id then
        error lexbuf "this is an OCaml reserved word."
      else
        LID (locate (Range.current lexbuf) id)
    }
| (uppercase identchar *) as id
    { UID (locate (Range.current lexbuf) id) }
(* Quoted strings are used as aliases for tokens. *)
(* A quoted string is stored as is -- with the quotes
   and with its escape sequences. *)
| '"'
    { let buffer = Buffer.create 16 in
      let openingrange = Range.current lexbuf in
      let content = record_string openingrange buffer lexbuf in
      let id = Printf.sprintf "\"%s\"" content in
      let range = Range.make (Range.startp openingrange, lexbuf.lex_curr_p) in
      QID (locate range id) }
| "//" [^ '\010' '\013']* newline (* skip C++ style comment *)
| newline
    { new_line lexbuf; main lexbuf }
| whitespace+
    { main lexbuf }
| "/*"
    { comment (Range.current lexbuf) lexbuf; main lexbuf }
| "(*"
    { ocamlcomment (Range.current lexbuf) lexbuf; main lexbuf }
| "<"
    { savestart lexbuf (angled (Range.current lexbuf)) }
| "%{"
    { savestart lexbuf (fun lexbuf ->
        let openingrange = Range.current lexbuf in
        let startpos = lexeme_end_p lexbuf in
        let closingpos, monsters = action true openingrange [] lexbuf in
        no_monsters monsters;
        HEADER (fragment startpos closingpos)
      ) }
| "{"
    { savestart lexbuf @@ fun lexbuf ->
      let openingrange = Range.current lexbuf in
      let startpos = lexeme_end_p lexbuf in
      let closingpos, monsters = action false openingrange [] lexbuf in
      ACTION (make_action startpos closingpos monsters)
        (* Partial application: [dollars] and [producers] are supplied by the
           parser once they are available. *)
    }
| ('%'? as percent) "[@" (attributechar+ as key) whitespace*
    { let openingrange = Range.current lexbuf in
      let startpos = lexeme_end_p lexbuf in
      let closingpos = attribute openingrange lexbuf in
      let origin = Range.make (Range.startp openingrange, lexeme_end_p lexbuf) in
      let payload = InputFile.chunk (startpos, closingpos) in
      let attr : attribute = { key; payload; origin } in
      if percent = "" then
        (* No [%] sign: this is a normal attribute. *)
        ATTRIBUTE attr
      else
        (* A [%] sign is present: this is a grammar-wide attribute. *)
        GRAMMARATTRIBUTE attr
    }
| eof
    { EOF }
| _
    { error lexbuf "unexpected character(s)." }

(* ------------------------------------------------------------------------ *)

(* Skip C style comments. *)

and comment openingrange = parse
| newline
    { new_line lexbuf; comment openingrange lexbuf }
| "*/"
    { () }
| eof
    { blame openingrange "unterminated comment." }
| _
    { comment openingrange lexbuf }

(* ------------------------------------------------------------------------ *)

(* Collect a fragment delimited by angle brackets (for example, an OCaml
   type). Angle brackets can appear as part of O'Caml function types and
   variant types, so we must recognize them and *not* treat them as a
   closing bracket. *)

and angled openingrange = parse
| "->"
| "[>"
    { angled openingrange lexbuf }
| '>'
    { let openingpos = Range.endp openingrange
      and closingpos = lexeme_start_p lexbuf in
      let fragment = fragment openingpos closingpos in
      (* Note: no parentheses are inserted. This could be a point-free
         semantic action, which must consist of a single identifier. *)
      ANGLED fragment }
| "(*"
    { ocamlcomment (Range.current lexbuf) lexbuf;
      angled openingrange lexbuf }
| newline
    { new_line lexbuf; angled openingrange lexbuf }
| eof
    { blame openingrange "unterminated opening bracket." }
| _
    { angled openingrange lexbuf }

(* ------------------------------------------------------------------------ *)

(* Collect O'Caml code delimited by curly brackets. The monsters that are
   encountered along the way are accumulated in the list [monsters]. Nested
   curly brackets must be properly counted. Nested parentheses are also kept
   track of, so as to better report errors when they are not balanced. *)

and action percent openingrange monsters = parse
| '{'
    { let _, monsters = action false (Range.current lexbuf) monsters lexbuf in
      action percent openingrange monsters lexbuf }
| ("}" | "%}") as delimiter
    { match percent, delimiter with
      | true, "%}"
      | false, "}" ->
          (* This is the delimiter we were instructed to look for. *)
          lexeme_start_p lexbuf, monsters
      | _, _ ->
          (* This is not it. *)
          blame openingrange "unbalanced opening brace."
    }
| '('
    { let _, monsters = parentheses (Range.current lexbuf) monsters lexbuf in
      action percent openingrange monsters lexbuf }
| '$' (['0'-'9']+ as i)
    { let i = int_of_string (Range.current lexbuf) i in
      let monster = dollar (Range.current lexbuf) i in
      action percent openingrange (monster :: monsters) lexbuf }
| poskeyword
    { let monster = position (Range.current lexbuf) where flavor i x in
      action percent openingrange (monster :: monsters) lexbuf }
| previouserror
    { error lexbuf "$previouserror is no longer supported." }
| syntaxerror
    { error lexbuf "$syntaxerror is no longer supported." }
| '"'
    { string (Range.current lexbuf) lexbuf;
      action percent openingrange monsters lexbuf }
| "'"
    { char lexbuf;
      action percent openingrange monsters lexbuf }
| "(*"
    { ocamlcomment (Range.current lexbuf) lexbuf;
      action percent openingrange monsters lexbuf }
| newline
    { new_line lexbuf;
      action percent openingrange monsters lexbuf }
| ')'
| eof
    { blame openingrange "unbalanced opening brace." }
| _
    { action percent openingrange monsters lexbuf }

(* ------------------------------------------------------------------------ *)

(* Inside a semantic action, we keep track of nested parentheses, so as to
   better report errors when they are not balanced. *)

and parentheses openingrange monsters = parse
| '('
    { let _, monsters = parentheses (Range.current lexbuf) monsters lexbuf in
      parentheses openingrange monsters lexbuf }
| ')'
    { lexeme_start_p lexbuf, monsters }
| '{'
    { let _, monsters = action false (Range.current lexbuf) monsters lexbuf in
      parentheses openingrange monsters lexbuf }
| '$' (['0'-'9']+ as i)
    { let i = int_of_string (Range.current lexbuf) i in
      let monster = dollar (Range.current lexbuf) i in
      parentheses openingrange (monster :: monsters) lexbuf }
| poskeyword
    { let monster = position (Range.current lexbuf) where flavor i x in
      parentheses openingrange (monster :: monsters) lexbuf }
| previouserror
    { error lexbuf "$previouserror is no longer supported." }
| syntaxerror
    { error lexbuf "$syntaxerror is no longer supported." }
| '"'
    { string (Range.current lexbuf) lexbuf;
      parentheses openingrange monsters lexbuf }
| "'"
    { char lexbuf; parentheses openingrange monsters lexbuf }
| "(*"
    { ocamlcomment (Range.current lexbuf) lexbuf;
      parentheses openingrange monsters lexbuf }
| newline
    { new_line lexbuf;
      parentheses openingrange monsters lexbuf }
| '}'
| eof
    { blame openingrange "unbalanced opening parenthesis." }
| _
    { parentheses openingrange monsters lexbuf }

(* ------------------------------------------------------------------------ *)

(* Collect an attribute payload, which is terminated by a closing square
   bracket. Nested square brackets must be properly counted. Nested curly
   brackets and nested parentheses are also kept track of, so as to better
   report errors when they are not balanced. *)

and attribute openingrange = parse
| '['
    { let _ = attribute (Range.current lexbuf) lexbuf in
      attribute openingrange lexbuf }
| ']'
    { lexeme_start_p lexbuf }
| '{'
    { let _, _ = action false (Range.current lexbuf) [] lexbuf in
      attribute openingrange lexbuf }
| '('
    { let _, _ = parentheses (Range.current lexbuf) [] lexbuf in
      attribute openingrange lexbuf }
| '"'
    { string (Range.current lexbuf) lexbuf;
      attribute openingrange lexbuf }
| "'"
    { char lexbuf; attribute openingrange lexbuf }
| "(*"
    { ocamlcomment (Range.current lexbuf) lexbuf;
      attribute openingrange lexbuf }
| newline
    { new_line lexbuf;
      attribute openingrange lexbuf }
| '}'
| ')'
| eof
    { blame openingrange "unbalanced opening bracket." }
| _
    { attribute openingrange lexbuf }

(* ------------------------------------------------------------------------ *)

(* Skip O'Caml comments. Comments can be nested and can contain
   strings or characters, which must be correctly analyzed. (A string
   could contain begin-of-comment or end-of-comment sequences, which
   must be ignored; a character could contain a begin-of-string
   sequence.) *)

and ocamlcomment openingrange = parse
| "*)"
    { () }
| "(*"
    { ocamlcomment (Range.current lexbuf) lexbuf;
      ocamlcomment openingrange lexbuf }
| '"'
    { string (Range.current lexbuf) lexbuf;
      ocamlcomment openingrange lexbuf }
| "'"
    { char lexbuf; ocamlcomment openingrange lexbuf }
| newline
    { new_line lexbuf;
      ocamlcomment openingrange lexbuf }
| eof
    { blame openingrange "unterminated OCaml comment." }
| _
    { ocamlcomment openingrange lexbuf }

(* ------------------------------------------------------------------------ *)

(* Skip O'Caml strings. *)

and string openingrange = parse
| '"'
    { () }
| '\\' newline
| newline
    { new_line lexbuf;
      string openingrange lexbuf }
| '\\' _
    (* Upon finding a backslash, skip the character that follows,
       unless it is a newline. Pretty crude, but should work. *)
    { string openingrange lexbuf }
| eof
    { blame openingrange "unterminated OCaml string." }
| _
    { string openingrange lexbuf }

(* ------------------------------------------------------------------------ *)

(* Recording an OCaml string. (This is used for token aliases.) *)

and record_string openingrange buffer = parse
| '"'
    { Buffer.contents buffer }
| ('\\' ['\\' '\'' '"' 't' 'b' 'r' ' ']) as sequence
    { (* This escape sequence is recognized as such, but not decoded. *)
      Buffer.add_string buffer sequence;
      record_string openingrange buffer lexbuf }
| '\\' 'n'
    (* We disallow this escape sequence in a token alias because we wish
       to use this string (unescaped) when we print a concrete sentence
       in a [.messages] file (see [Messages]), and we want this sentence
       to fit on a single line. *)
    { error lexbuf "'\\n' is not permitted in a token alias." }
| '\\' _
    { error lexbuf "illegal backslash escape in string." }
| newline
    { error lexbuf "illegal newline in string." }
| eof
    { blame openingrange "unterminated string." }
| _ as c
    { Buffer.add_char buffer c;
      record_string openingrange buffer lexbuf }

(* ------------------------------------------------------------------------ *)

(* Skip O'Caml characters. A lone quote character is legal inside
   a comment, so if we don't recognize the matching closing quote,
   we simply abandon. *)

and char = parse
| '\\'? newline "'"
   { new_line lexbuf }
| [^ '\\' '\''] "'"
| '\\' _ "'"
| '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
| '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
| ""
   { () }

(* ------------------------------------------------------------------------ *)

(* Read until the end of the file. This is used after finding a %%
   that marks the end of the grammar specification. We update the
   current position as we go. This allows us to build a fragment
   for the postlude. *)

and finish = parse
| newline
    { new_line lexbuf; finish lexbuf }
| eof
    { lexeme_start_p lexbuf }
| _
    { finish lexbuf }
