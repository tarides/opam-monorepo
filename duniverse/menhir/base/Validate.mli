(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module offers facilities for validating possibly incorrect data. *)

open Report

(**[Invalid] can be raised by a validator function. *)
exception Invalid

(**A validator function expects a reporting channel and an argument of type
   ['a], which it must transform into a result of type ['b]. An error message
   can be emitted by using [Report.signal]. When the input data is invalid, a
   validator function can raise [Invalid], but this is not required: it can
   also return a default value. *)
type ('a, 'b) validator =
  channel -> 'a -> 'b

(**[robustly validate] transforms the validator [validate] into a robust
   validator, which cannot raise [Invalid], but can return [None]. *)
val robustly: ('a, 'b) validator -> ('a, 'b option) validator

(**[option validate] is a list validator. If validating the content raises
   [Invalid] then this exception is propagated. *)
val option: ('a, 'b) validator -> ('a option, 'b option) validator

(**[list validate] is a list validator. If validating an element raises
   [Invalid] then this exception is propagated. *)
val list: ('a, 'b) validator -> ('a list, 'b list) validator

(**[robust_list validate] is a robust list validator. It cannot raise [Invalid],
   but dismisses invalid elements, so the output list can be shorter than
   the input list. *)
val robust_list: ('a, 'b) validator -> ('a list, 'b list) validator
