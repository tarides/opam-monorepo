(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Report
open PlainSyntax

(**This expansion algorithm offers three policies. The most agressive policy,
   [`ExpandNullableSymbols], expands away (eliminates) all nullable symbols,
   everywhere. The least aggressive policy, [`ExpandNullableSuffixes], expands
   away just the occurrences of nullable symbols that form a nullable suffix
   of a production. This policy can create conflicts. The last policy,
   [`ExpandNullableSuffixParticipants], determines which nullable symbols
   participate in a nullable suffix of a production and expands away all
   occurrences of these symbols. Regardless of which policy is used, the
   transformed grammar has no right nullable productions: that is, the last
   symbol in every production is not nullable. *)
type policy =
  [ `ExpandNullableSymbols
  | `ExpandNullableSuffixParticipants
  | `ExpandNullableSuffixes ]

(**[transform main aux forceful policy grammar] transforms the grammar by
   expanding away some or all nullable symbols, following the policy [policy].
   The flag [forceful] determines whether problematic %prec annotations and
   problematic %on_error_reduce declarations should be forcefully removed;
   this flag should normally be [false]. Errors and warnings are reported via
   the channel [main]. Information messages about the grammar are emitted on
   the channel [aux]. *)
val transform: channel -> channel -> bool -> policy -> grammar -> grammar
