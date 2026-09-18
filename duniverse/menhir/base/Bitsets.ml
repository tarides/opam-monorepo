(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* The library bitsets, which is found in the subdirectory bitsets/, has been
   renamed vendored_bitsets so as to prevent Dune from complaining about a
   conflict with a copy of bitsets that might be installed on the user's
   system. *)

(* As a result, the library is now accessible under the name Vendored_bitsets.
   Because we do not want to pollute Menhir's sources with this name, we
   define the module Bitsets as an alias for Vendored_bitsets. *)

include Vendored_bitsets
