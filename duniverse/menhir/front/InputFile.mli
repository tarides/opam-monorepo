(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Lexing

(**This module defines a type of input files, which is used to record the
   origin of certain elements (productions, declarations, etc.). Some of its
   operations involve global state: they keep track of which input file is
   currently being read. *)

(**An input file. *)
type file

(**[name file] returns the name of the input file [file]. *)
val name: file -> string

(**[equal] determines whether two input files are the same file. *)
val equal: file -> file -> bool

(**[compare] is a total order on input files. It reflects the ordering of
   these files on Menhir's command line. Ideally, it should NOT be used. *)
val compare: file -> file -> int

(**The fictitious input file [builtin] is used as the origin of the start
   productions. This technical detail is probably irrelevant entirely. *)
val builtin: file

(**[with_file_content name content f] creates a fresh input file with name
   [name] and makes it the current input file while the action [f] runs. The
   function [f] is applied to a fresh lexing buffer that has been initialized
   with [name] and [content]. The function [f] may call {!current} to obtain
   the current input file, and may call {!chunk} to retrieve a segment of
   [content]. *)
val with_file_content: string -> string -> (lexbuf -> 'a) -> 'a

(**[current()] indicates which input file is currently being read. It can be
   called only while a call to [with_file_content] is ongoing. *)
val current: unit -> file

(**[chunk pos1 pos2] extracts a chunk out of the current input file, delimited
   by the positions [pos1] and [pos2]. It can be called only while a call to
   [with_file_content] is ongoing. *)
val chunk: position * position -> string
