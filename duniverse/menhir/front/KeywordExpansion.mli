(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module expands away the keywords [$startpos] and [$endpos] as well as
   the entire [ofs] family of keywords. Doing this early simplifies some
   aspects later on, in particular inlining. *)

open PlainSyntax

(**[expand] expands away the keywords [$startpos] and [$endpos] in semantic
   actions. The entire [ofs] family of keywords is also expanded away. This
   transformation does NOT affect %merge functions, where we allow the
   keywords [$startpos] and [$endpos] and do not allow any other keyword. *)
val expand: grammar -> grammar
