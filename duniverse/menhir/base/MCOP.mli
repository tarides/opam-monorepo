(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module solves the Matrix Chain Ordering Problem (MCOP). The problem is
   to find how to associate a chain of matrix multiplications so as to
   minimize the number of scalar multiplications. *)

(**A problem instance is an array of integers, representing the dimensions of
   the matrices. For example, a product of two matrices [M1[i, j] x M2[j, k]]
   is described by the array [[|i; j; k|]]. In general, the multiplication of
   [n] matrices is represented by an array of length [n+1]. *)
type problem =
  int array

(**A solution is a tree whose internal nodes represent matrix multiplications
   and whose leaves represent matrices. Only the shape of the binary tree
   matters: a decoration of type ['a] is provided as a convenience for the
   user.

   The functions that construct solutions construct solutions of type [int
   solution]. The integer decoration represents the index of the matrix in the
   chain. *)
type 'a solution =
  | Matrix of 'a
  | Product of 'a solution * 'a solution

(**[map] transforms the decorations carried by a solution. *)
val map : ('a -> 'b) -> 'a solution -> 'b solution

(**The exception [Empty] is raised when an empty problem is submitted,
   as it has no solution. *)
exception Empty

(**[left] produces a trivial left-leaning solution. *)
val left : problem -> int solution

(**[right] produces a trivial left-leaning solution. *)
val right : problem -> int solution

(**[approx] is the linear-time algorithm described by Francis Y. Chin in the
   paper "An O(n) algorithm for determining a near-optimal computation order
   of matrix chain products". This algorithm computes an approximate solution
   which performs at most 25% more operations than the optimal one, and much
   less in practice. *)
val approx : problem -> int solution

(**[optimal] computes an optimal solution, in cubic time, using dynamic
   programming. A formal description can be found in "Introduction to
   Algorithms" by Cormen et al. *)
val optimal : problem -> int solution

(* An algorithm which computes an optimal solution in time O(n log n) is described
   in "Computation of Matrix Chain Products" by Hu and Shing:
   - "Part I", doi 10.1.1.695.2923
   - "Part II", doi 10.1.1.695.4875
   This algorithm is more involved and has not been implemented here. *)
