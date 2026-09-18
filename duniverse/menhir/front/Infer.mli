(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module offers type inference and dependency computation facilities.
   Both facilities concern the OCaml code contained in the semantic actions.
   Both involve generating mock [.ml] and [.mli] files, which are passed to
   the tools [ocamlc] or [ocamldep] to obtain type information or dependency
   information. *)

open PlainSyntax
open FrontAPI

module Make
(G : sig val grammar : grammar end)
(X : sig
  include API_SETTINGS
  include BASE_SETTINGS
  include COMMENT_SETTINGS
  include EXN_SETTINGS
  include INFER_SETTINGS
  include TOKEN_TYPE_SETTINGS
end)
: sig

  (**[infer()] infers the OCaml types of the semantic actions and returns a
     new grammar, augmented with a [%type] declaration for every nonterminal
     symbol. To perform type inference, the OCaml compiler is invoked. This
     function corresponds to Menhir's [--infer] option. It is deprecated: the
     preferred method, which is used by Dune, is to use [--infer-write-query]
     and [--infer-read-reply]. *)
  val infer: unit -> grammar

  (**[depend mode] computes the dependencies (in the sense of [make]) induced by
     the semantic actions and prints these dependencies on the standard output
     channel. If [mode] is [`Depend], then [ocamldep]'s output is postprocessed;
     otherwise it is echoed unchanged. This function corresponds to Menhir's
     [--depend] and [--raw-depend] commands. It is deprecated: the preferred
     method, which is used by Dune, is to use [--infer-write-query]. *)
  val depend: [`Depend | `RawDepend] -> unit

  (**[write_query filename] writes the semantic actions to a mock [.ml] file
     named [filename]. This file can then be submitted to the OCaml compiler
     for type inference or to [ocamldep] for dependency computation. This
     function corresponds to [--infer-write-query]. *)
  val write_query: string -> unit

  (**[read_reply filename] reads the types inferred by the OCaml compiler for
     the mock [.ml] file described above. It returns a new grammar, augmented
     with a [%type] declaration for every nonterminal symbol. *)
  val read_reply: string -> grammar

end
