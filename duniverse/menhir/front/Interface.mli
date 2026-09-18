(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module constructs the interface (that is, the [.mli] file)
   of the generated parser. It also provides the definition of the
   internal submodule [Basics]. *)

open IL
open PlainSyntax
open FrontAPI

(**The internal submodule [Basics] exists both when the code back-end
   is used and when the table back-end is used. *)
module Basics (X : sig
  include EXN_SETTINGS
  include TOKEN_TYPE_SETTINGS
end) : sig

  (**[call_stop s] is a call to the internal function [stop], which raises the
     exception [Error]. If [exn_carries_state] is [true], the exception
     [Error] carries an integer parameter, a state number [s]. This state
     number must be passed to [call_stop]. If [exn_carries_state] is [false]
     then [call_stop] ignores its argument. *)
  val call_stop: int -> expr

  (**[basics_submodule_name] is the name of the internal submodule [Basics]. *)
  val basics_submodule_name: string

  (**The structure items [basics_submodule_def grammar] define and include the
     internal submodule [Basics]. This submodule defines the exception
     [Error], the function [stop], and the type [token]. *)
  val basics_submodule_def: grammar -> structure

end (* Basics *)

module Make (X : sig
  include API_SETTINGS
  include COMMENT_SETTINGS
  include EXN_SETTINGS
  include TOKEN_TYPE_SETTINGS
end) : sig

  (**[incremental_submodule_name] is the the name of the submodule that
     contains the entry points of the incremental API. *)
  val incremental_submodule_name: string

  (**[tcheckpoint] constructs an application of the parameterized type
     ['a checkpoint], which is defined in the submodule [Interpreter].
     This type exists in the incremental API only. *)
  val tcheckpoint: typ -> typ

  (**[write grammar filename ()] writes the interface of the generated
     parser to the [.mli] file named [filename]. *)
  val write: grammar -> string -> unit -> unit

end (* Make *)
