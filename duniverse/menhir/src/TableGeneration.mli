(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open IL

(**This module offers support for marshaling data (typically, tables of
   various kinds) as IL code. *)

(**We write ['a matrix] for a rectangular matrix, that is, an array of
   arrays of identical lengths. *)
type 'a matrix =
  'a array array

module Make (X : sig
  open Report

  (**[runtimelib] is the name of the runtime library that contains the modules
     [LinearizedArray] and [PackedIntArray]. It can be for example [MenhirLib]
     or [MenhirGLR]. *)
  val runtimelib: string

  (**The channel [info] is used to emit information messages about the
     tables that we construct. *)
  val info: channel

end) : sig

(**[define_and_measure x e] prints a measure of the table encoded by the
   expression [e] on the channel [info], then returns [def x e]. *)
val define_and_measure: var -> expr -> valdef

(**The following functions return sequences of definitions. These are lists of
   independent (non-recursive) definitions. Each definition can refer to the
   names introduced by the previous definitions. The last definition in the
   list is the main definition.

   The first parameter of each function is [name]. The function prints a
   measure of the data, under the name [name], on the channel [info].

   The second parameter of each function is [x]. This is the name of the
   main definition. *)

(**[marshal_1D_array] marshals an integer array. It constructs an accessor
   function [x : int -> int]. *)
val marshal_1D_array: string -> var -> int array -> valdefs

(**[marshal_irregular_2D_array] marshals an array of integer arrays of
   possibly different lengths. It constructs an accessor function
   [x : int -> int list]. *)
val marshal_irregular_2D_array: string -> var -> int array array -> valdefs

(**[marshal_irregular_3D_array] marshals a matrix of integer arrays of
   possibly different lengths. It constructs an accessor function
   [x : int -> int -> int list]. *)
val marshal_irregular_3D_array: string -> var -> int array matrix -> valdefs

(**[marshal_2D_matrix] marshals an integer matrix. It constructs an accessor
   function [x : int -> int -> int]. *)
val marshal_2D_matrix: string -> var -> int matrix -> valdefs

(**[marshal_2D_sparse_matrix] marshals a sparse 2D matrix. In a sparse matrix,
   some entries are considered insignificant: the user promises to never
   access these entries, so it permitted to overlap them with other entries.
   The parameter [insignificant] determines whether an entry is insignificant.
   (This must be determined based on the content of this entry, not based on
   its coordinates. This is a mistake, but this will do for now.) The result
   is an accessor function [x : int -> int -> int]. *)
val marshal_2D_sparse_matrix:
  string -> var -> (int -> bool) -> int matrix -> valdefs

end (* Make *)
