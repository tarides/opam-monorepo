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

(* 2011/10/19: do not use [Filename.basename]. The [#] annotations that
   we insert in the [.ml] file must retain their full path. This does
   mean that the [#] annotations depend on how menhir is invoked -- e.g.
   [menhir foo/bar.mly] and [cd foo && menhir bar.mly] will produce
   different files. Nevertheless, this seems useful/reasonable. *)

(* This also influences the type error messages produced by [--infer]. *)

(* 2016/08/25: in principle, the order in which file names appear on the
   command line (when there are several of them) does not matter. It is
   however used in [BasicPrinter] (see the problem description
   there). For this reason, we define a type [input_file] which includes
   the file's name as well as its index on the command line. *)

(* An input file has a name and an index (which is used by [compare]). *)

type file = { name: string; index: int }

let[@inline] name input =
  input.name

let[@inline] equal file1 file2 =
  file1.index = file2.index
    (* could also use physical equality [file1 == file2] *)

let[@inline] compare file1 file2 =
  Int.compare file1.index file2.index
    (* Ideally, this function should NOT be used, as it reflects the
       order of the input files on the command line. As of 2016/08/25,
       it is used by [BasicPrinter], for lack of a better
       solution. *)

let builtin =
  let name = "<builtin>"
  and index = -1 in
  { name; index }

(* Global state. *)

let current : (file * string) option ref =
  ref None

let next : int ref =
  ref 0

let with_file_content name content f =
  assert (!current = None);
  let index = MInt.postincrement next in
  let file = { name; index } in
  current := Some (file, content);
  let lexbuf = from_string content in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };
  let result = f lexbuf in
  current := None; (* avoid memory leak *)
  result

let chunk (pos1, pos2) =
  let ofs1 = pos1.pos_cnum
  and ofs2 = pos2.pos_cnum in
  let _file, content = Option.get !current in
  let len = ofs2 - ofs1 in
  String.sub content ofs1 len

let[@inline] current () =
  let file, _content = Option.get !current in
  file
