/******************************************************************************/
/*                                                                            */
/*                                    Menhir                                  */
/*                                                                            */
/*   Copyright Inria. All rights reserved. This file is distributed under     */
/*   the terms of the GNU General Public License version 2, as described in   */
/*   the file LICENSE.                                                        */
/*                                                                            */
/******************************************************************************/

/* This is two parsers in one. */

/* This parser is used to read the sentences provided on the standard input
   channel when [--interpret] is set. The entry point is [optional_sentence]. */

/* It is used also to read a [.messages] file. The entry point is [entry]. */

/* This parser must be compatible with both ocamlyacc and menhir, so we use
   $ notation, do not use Menhir's standard library, and collect positions
   manually. */

/* -------------------------------------------------------------------------- */
/* Tokens. */

%token COLON EOF EOL
%token<RawSentence.raw_symbol> TERMINAL
%token<RawSentence.raw_symbol> NONTERMINAL
%token<string>                 COMMENT
  /* only manually-written comments, beginning with a single # */

/* -------------------------------------------------------------------------- */
/* Types. */

%{


%}

%type <RawSentence.raw_sentence>
  sentence

%type <RawSentence.raw_sentence option>
  optional_sentence

%type<RawSentence.raw_entry>
  entry

%start optional_sentence
%start entry

%%

/* -------------------------------------------------------------------------- */

/* An entry is a list of sentences or comments. */
entry: sentences_or_comments EOF
  { $1 }

/* A list of sentences or comments. */
sentences_or_comments:
  { [] }
| sentence sentences_or_comments
    { OrComment.Thing   $1 :: $2 }
| COMMENT  sentences_or_comments
    { OrComment.Comment $1 :: $2 }

/* An optional sentence. */
optional_sentence:
| EOF
    { None }
| sentence
    { Some $1 }

/* A sentence is a pair of an optional non-terminal start symbol and a list
   of terminal symbols. It is terminated by a newline. */
sentence:
| NONTERMINAL COLON terminals EOL
    { Some $1, $3 }
| terminals EOL
    { None, $1 }

/* A list of terminal symbols. */
terminals:
|
    { [] }
| TERMINAL terminals
    { $1 :: $2 }
