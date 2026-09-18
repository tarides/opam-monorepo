(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module contains most of the code that deals with [.messages] files. *)

open FrontAPI
open MiddleAPI

module Make (Lr1 : LR1) (X : sig
  include COMMENT_SETTINGS
  include STRATEGY_SETTINGS
end) : sig

(**The name of a [.messages] file. *)
type filename =
  string

(**[compile_errors filename] compiles the error message table found in the
   file [filename] down to OCaml code. The code is printed on the standard
   output channel. *)
val compile_errors: filename -> unit

(**[compare_errors filename1 filename2] compares the error message tables
   found in the files [filename1] and [filename2]. It performs an inclusion
   test: every state in the domain of the left-hand table must also appear in
   the domain of the right-hand table and must be associated in both tables
   with the same error message. If this is not the case, errors and warnings
   are emitted. *)
val compare_errors: filename -> filename -> unit

(**[merges_errors filename1 filename2] merges the error message tables found
   in the files [filename1] and [filename2]. The result, a new message table,
   is printed on the standard output channel. *)
val merge_errors: filename -> filename -> unit

(**[update_errors filename] updates the error message table found in the file
   [filename]. The auto-generated comments, which are marked with [##], are
   re-generated; the rest of the file is untouched. The result, a new message
   table, is printed on the standard output channel. *)
val update_errors: filename -> unit

(**[echo_errors concrete filename] prints the error sentences found in the
   file [filename] to the standard output channel. The error messages and
   comments found in this file are not printed. If [concrete] is [true] then
   every sentence is followed with an auto-generated comment that shows its
   concrete syntax. *)
val echo_errors: bool -> filename -> unit

end
