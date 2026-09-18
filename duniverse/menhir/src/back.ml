(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* Driver for the back-end. *)

(* Force the side effects in [Freeze] to take place first. *)

let () =
  Freeze.force()

module Interface =
  Interface.Make(Settings)

(* Define an .ml file writer . *)

let write program =
  let module P = ILPrinter.Make (struct
    let filename = Settings.base ^ ".ml"
    let f = open_out filename
    let print_line_directives =
      (* 2017/05/09: always include line number directives in generated .ml
         files. Indeed, they affect the semantics of [assert] instructions
         in the semantic actions. *)
      (* 2011/10/19: do not use [Filename.basename]. The line number
         directives that we insert in the [.ml] file must retain their full
         path. This does mean that the line number directives depend on how
         menhir is invoked -- e.g. [menhir foo/bar.mly] and [cd foo && menhir
         bar.mly] will produce different files. Nevertheless, this seems
         useful/reasonable. *)
      Some filename
    include Settings
  end) in
  P.program program

(* Construct and print the code using an appropriate back-end. *)

let () =
  let mli = Settings.base ^ ".mli" in
  match Settings.backend with
  | `Interpret _
  | `InterpretGLR _
  | `InterpretError ->
      (* These cases are handled in Freeze. *)
      assert false
  | `TableBackend ->
      let module B = TableBackend.Run () in
      Time.time "Printing" @@ fun () ->
      write B.program;
      Interface.write Front.grammar mli ()
  | `GLRBackend ->
      let module B = GLRBackend.Run () in
      Time.time "Printing" @@ fun () ->
      write B.program;
      Interface.write Front.grammar mli ()
  | `RocqBackend ->
      let module B = RocqBackend.Run () in
      Time.time "Printing" @@ fun () ->
      let filename = Settings.base ^ ".v" in
      let f = open_out filename in
      B.write_all f
  | `CodeBackend ->
      let module B = CodeBackend.Run () in
      Time.time "Printing" @@ fun () ->
      write B.program;
      Interface.write Front.grammar mli ()
  | `NoBackend ->
      (* If no back-end was selected then we must have exited earlier. *)
      assert false

let finished =
  true
