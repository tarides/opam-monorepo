(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module implements severe conflict resolution. That is, all of the
   shift/reduce, reduce/reduce, and end-of-stream conflicts that exist in the
   automaton are resolved. Shift/reduce conflicts are resolved by removing a
   reduction, thereby favoring shifting. Reduce/reduce conflicts are solved by
   obeying [Precedence.reduce_reduce] and possibly making an arbitrary choice.
   End-of-stream conflicts are solved by removing a reduction, thereby
   favoring inspecting the next input symbol. In summary, no transitions are
   removed; some reductions are removed. *)

open Report
open MiddleAPI

module Run
(A : LR1_AUTOMATON)
(Precedence : sig
  open A.Lr0.G

  (**[reduce_reduce prod1 prod2] determines how a reduce/reduce conflict
     between the productions [prod1] and [prod2] should be resolved. *)
  val reduce_reduce:
    Production.t -> Production.t -> Production.t option

end)
(E : sig

  (**This channel is used to emit warnings, both during conflict
     resolution and when [diagnostics()] is invoked. *)
  val main: channel

end)
: sig

  include LR1_AUTOMATON with module Lr0 = A.Lr0
                         and type node = A.node

  (**[diagnostics()] emits a warning if at least one severe conflict
     has been resolved. *)
  val diagnostics: unit -> unit

end
