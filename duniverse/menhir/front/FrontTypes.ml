(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

include BaseTypes

type attribute =
  Attribute.attribute

type attributes =
  Attribute.attributes

(**A terminal symbol. *)
type terminal =
  string

(**A nonterminal symbol. *)
type nonterminal =
  string

(**A terminal or nonterminal symbol. *)
type symbol =
  string

(**A list of symbols. *)
type symbols =
  symbol list

(**In a somewhat fragile convention, in a partial grammar, a reference to a
   terminal symbol either is a normal identifier [LID], in which case it is
   the name of the terminal symbol, or is a quoted identifier [QID], in which
   case it is a token alias.

   Token aliases are eliminated by replacing them with the corresponding
   terminal symbols very early on during the joining of the partial grammars;
   see the module [ExpandTokenAliases].

   In a complete grammar, there are no token aliases any longer. That is,
   we keep track of the aliases that have been declared (they can be found
   via the field [tk_alias]), but we never use them, since they have been
   eliminated up front. *)
type alias =
  string option

(**An identifier is used to refer to a symbol's semantic value. *)
type identifier =
  string

(**A file name. *)
type filename =
  string

(**A semantic action. *)
type action =
  Action.t

(**A %merge function is a fragment of OCaml code. *)
type merge_fun =
  Action.t

(**The associativity status of a terminal symbol. *)
type associativity =
  | LeftAssoc
  | RightAssoc
  | NonAssoc
  | UndefinedAssoc

(**The precedence level of a terminal symbol.

   The special value [UndefinedPrecedence] is used for the special tokens
   [error] and [#]. It is also used for pseudo-tokens (which are not declared,
   but appear in a [%prec] annotation). A normal token receives a precedence
   level of the form [PrecedenceLevel (file, level)], where [file]
   is an input file and [level] is an integer level. Within each file, later
   declarations receive greater levels, which represent higher priority. Two
   declarations that originate in different files are incomparable. *)
type precedence_level =
  | UndefinedPrecedence
  | PrecedenceLevel of (InputFile.file * int) located

(**A "production level" is used to solve reduce/reduce conflicts. It reflects
   which production appears first in the grammar. See [ParserAux]. *)
type production_level =
  | ProductionLevel of InputFile.file * int

(**A [%prec] annotation is optional. A production can carry at most one.
   If there is one, it is a symbol name. See [ParserAux]. *)
type prec_annotation =
  symbol located option

(**A level is attached to every [%on_error_reduce] declaration. It is used
   to decide what to do when several such declarations are applicable in a
   single state. *)
type on_error_reduce_level =
  production_level (* we re-use the above type, to save code *)

(**The properties of a terminal symbol. *)
type properties =
  {

    tk_ocamltype     : ocamltype option;
    (**The OCaml type of this terminal symbol can be provided via a [%type]
       declaration. It is optional. *)

    tk_position      : range;
    (**The position where this terminal symbol is declared. *)

    tk_alias         : alias;
    (**A "token alias" can be declared for this terminal symbol. It is
       optional. *)

    tk_attributes    : attributes;
    (**The attributes attached with this terminal symbol. *)

    tk_associativity : associativity;
    (**The associativity status of this terminal symbol. *)

    tk_precedence    : precedence_level;
    (**The precedence level of this terminal symbol. *)

    tk_is_declared   : bool;
    (**Whether a declaration of this terminal symbol has been found.
       When no declaration exists for a terminal symbol, then it is
       considered a "pseudo-token". In such a case, it is really just
       a name for a precedence level; it can appear in a [%prec]
       annotation. *)

  }
