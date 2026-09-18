(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Printf
open IL
open ILConstruction
module LinearizedArray = MenhirLib.LinearizedArray
module PackedIntArray = MenhirLib.PackedIntArray

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

end) = struct

(* We rely on the modules [PackedIntArray] and [LinearizedArray], which exist
   both in MenhirLib and MenhirGLR. They are identical, so we can use either of
   them at table generation time, regardless of which one is used at runtime.
   The parameter [runtimelib] indicates which library we should refer to. *)

(* -------------------------------------------------------------------------- *)

(* Statistics. *)

(* Integer division, rounded up. *)

let div a b =
  if a mod b = 0 then a / b else a / b + 1

(* [size unboxed e] provides a rough measure of the size of the data encoded
   by the IL expression [e], in words. The parameter [unboxed] is true if we
   have already counted 1 for the pointer to the object. *)

let rec size unboxed (e : expr) =
  match e with
  | EIntConst _
  | ETuple []
  | EData (_, []) ->
      if unboxed then 0 else 1
  | EStringConst s ->
      1 + div (String.length s * 8) Sys.word_size
  | ETuple es
  | EData (_, es)
  | EArray es ->
      1 + List.length es + List.fold_left (fun s e -> s + size true e) 0 es
  | _ ->
      assert false (* not implemented *)

let size e =
  size false e

let measure name (def : valdef) =
  Report.log X.info
    "The %s table occupies roughly %d bytes."
    name (size def.valval * (Sys.word_size / 8));
  def

let define_and_measure x e =
  measure x (def x e)

(* -------------------------------------------------------------------------- *)

(* Operations on 2D matrices. *)

let matrix_fold_left f accu matrix =
  Array.fold_left (fun accu row ->
    Array.fold_left f accu row
  ) accu matrix

let matrix_height matrix =
  Array.length matrix

let matrix_width matrix =
  assert (matrix_height matrix > 0);
  let n = Array.length matrix.(0) in
  assert (Array.for_all (fun row -> Array.length row = n) matrix);
  n

let matrix_size matrix =
  let m = matrix_height matrix in
  if m = 0 then 0 else
  let n = matrix_width matrix in
  m * n

(* [population] counts the number of significant entries
   in a two-dimensional matrix. *)

let population insignificant (matrix : int array array) =
  matrix_fold_left (fun population entry ->
    if insignificant entry then population else population + 1
  ) 0 matrix

let flatten (matrix : 'a array array) : 'a array =
  matrix |> Array.to_list |> Array.concat

(* -------------------------------------------------------------------------- *)

(* Auxiliary functions for code generation. *)

let i, j, k = "i", "j", "k"

let binop op e1 e2 =
  EApp (EVar op, [ e1; e2])

let add =
  binop "(+)"

let mul =
  binop "(*)"

(* [unflatten n get] generates the IL expression [fun i j -> get (n * i + j)].
   It transforms an access into a 2D matrix into an access into a 1D array. *)

let unflatten (n : int) (get : var) : expr =
  efun [ PVar i; PVar j ] @@
  EApp (EVar get, [ add (mul (EIntConst n) (EVar i)) (EVar j) ])

(* [row_displacement_decode get_displacement get_data] generates
   the IL expression:
     fun i j ->
       let k = RowDisplacementDecode.decode (get_displacement i) in
       get_data (k + j)
   This is a copy of [RowDisplacementDecode.get],
   specialized with [get_displacement] and [get_data]. *)

let row_displacement_decode get_displacement get_data : expr =
  let get_displacement e = eapp (EVar get_displacement) [e]
  and get_data e = eapp (EVar get_data) [e] in
  let decode = sprintf "%s.RowDisplacementDecode.decode" X.runtimelib in
  let decode e = eapp (EVar decode) [e] in
  efun [ PVar i; PVar j ] @@
  let i, j = EVar i, EVar j in
  blet [ PVar k, decode (get_displacement i) ] @@
  let k = EVar k in
  get_data (add k j)

(* [read_row_via get_data get_entry] generates the IL expression
   [fun i -> LinearizedArray.read_row_via get_data get_entry i]. *)

let read_row_via get_data get_entry : expr =
  let read_row_via = sprintf "%s.LinearizedArray.read_row_via" X.runtimelib in
  efun [ PVar i ] @@
  EApp (EVar read_row_via, [ EVar get_data; EVar get_entry; EVar i ])

(* -------------------------------------------------------------------------- *)

(* Table compression. *)

(* We say that a matrix is sparse when some of its entries are insignificant.
   An entry is insignificant if it is never accessed. *)

(* A sparse two-dimensional table is turned into a one-dimensional table
   via [compress]. *)

(* [compress] is our preferred table compression algorithm. It uses either
   [RowDisplacementEncodeClassic] or [RowDisplacementEncode]. These two
   algorithms produce compressed tables in a common format, so they can be
   used interchangeably. *)

(* An irregular two-dimensional table can be made one-dimensional by
   flattening: see [LinearizedArray]. *)

(* A one-dimensional table is packed via [PackedIntArray]. *)

let compress =
  if Settings.pack_classic then
    RowDisplacementEncodeClassic.compress
  else
    RowDisplacementEncode.compress

(* -------------------------------------------------------------------------- *)

(* 1D array: accessor function [x : int -> int]. *)

let marshal_1D_array name x (a : int array) =
  let (bits : int), (text : string) = PackedIntArray.pack a in
  [
    (* Define a table [x]. *)
    measure name @@ def x @@ (EStringConst text);
    (* Define an accessor function, also named [x], as [fun i -> get x i]
       where [get] is [PackedIntArray.get?] with a suitable bit width. *)
    def_inline x @@ efun [PVar i] @@
    EApp (
      EVar (sprintf "%s.PackedIntArray.get%d" X.runtimelib bits),
      [ EVar x; EVar i ]
    )
  ]

(* 2D matrix: accessor function [x : int -> int -> int]. *)

let marshal_2D_matrix name x (m : int matrix) =
  (* View the table as a one-dimensional array, and marshal it. *)
  let n = matrix_width m in
  marshal_1D_array name x (flatten m) @
  (* Use the table width [n] in the definition of an accessor function. *)
  [ def_inline x @@ unflatten n x ]

(* 2D sparse matrix: accessor function [x : int -> int -> int]. *)

let marshal_2D_sparse_matrix name x insignificant (m : int matrix) =
  let displacement, data = compress insignificant 0 m in
  Report.log X.info
    "The %s table has %d entries; %d non-zero; %d compressed."
    name (matrix_size m) (population insignificant m)
    (Array.length displacement + Array.length data);
  (* Define an accessor function for the displacement table. *)
  let x_disp = x ^ "_displacement" in
  marshal_1D_array (name ^ " displacement") x_disp displacement @
  (* Define an accessor function for the data table. *)
  let x_data = x ^ "_data" in
  marshal_1D_array (name ^ " data") x_data data @
  (* Combine the two to obtain an accessor function [x] for the matrix. *)
  [ def_inline x @@ row_displacement_decode x_disp x_data ]

(* Array of arrays: accessor function [x : int -> int list]. *)

let marshal_irregular_2D_array name x (a : int array array) =
  let data, entry = LinearizedArray.make a in
  (* Define an accessor function for the data table. *)
  let x_data = x ^ "_data" in
  marshal_1D_array (name ^ " data") x_data data @
  (* Define an accessor function for the entry table. *)
  let x_entry = x ^ "_entry" in
  marshal_1D_array (name ^ " entry") x_entry entry @
  (* Combine them to obtain an accessor function [x] for the 2D table. *)
  [ def_inline x @@ read_row_via x_data x_entry ]

(* Matrix of arrays: accessor function [x : int -> int -> int list]. *)

let marshal_irregular_3D_array name x (m : int array matrix) =
  (* View the 2D matrix as a 1D array, and marshal it. *)
  let n = matrix_width m in
  marshal_irregular_2D_array name x (flatten m) @
  (* Use the table width [n] in the definition of an accessor function. *)
  [ def_inline x @@ unflatten n x ]

end (* Make *)
