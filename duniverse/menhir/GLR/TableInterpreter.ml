(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

open TableFormat

module[@inline] Make (T : TABLES) = struct

  type state = int
  type token = T.token
  type nonterminal = int
  type semv = Obj.t
  type production = int

  exception Error = T.Error

  module Semv = struct
    let token2value = T.token2value
    let merge = T.merge
  end

  module Production = struct

    let start = T.start
    let length = T.length
    let lhs = T.lhs

    let[@inline] action prod =
      (* Indexing into the array [T.semantic_action] is off by [T.start],
         because the start productions do not have entries in this array. *)
      T.semantic_action.(prod - T.start)

    let print _prod =
      "<cannot show production>"

  end (* Production *)

  module State = struct

    let n = T.n

    (* Decoding the default reduction table. *)

    let[@inline] decode_DefRed c = c - 1
    let dr_NoDefRed = 0

    (* Decoding the shift bitmap. *)

    let sb_DoNotFail = 0

    (* Decoding the shift table. *)

    let s_Fail = 0
    let[@inline] decode_Shift c = c - 1

    let foreach_shift (s : state) input yield =
      (* First determine whether the state [s] has a default reduction. *)
      (* If there is one, then there are no transitions. *)
      if T.default_reduction s = dr_NoDefRed then
      (* There is no default reduction. Request the lookahead symbol. *)
      let token = Input.lookahead input in
      let t = T.token2terminal token in
      (* Consult the shift bitmap. *)
      if T.shift_bitmap s t = sb_DoNotFail then
      (* The shift bitmap says "go". Consult the shift table. *)
      let c = T.shift s t in
      assert (c <> s_Fail);
      let s' = decode_Shift c in
      (* Yield this transition. *)
      yield s'

    let foreach_reduction (s : state) input yield =
      (* First determine whether the state [s] has a default reduction. *)
      let c = T.default_reduction s in
      if c <> dr_NoDefRed then yield (decode_DefRed c) else
      (* There is no default reduction. Request the lookahead symbol. *)
      let token = Input.lookahead input in
      let t = T.token2terminal token in
      (* Consult the reductions table. *)
      List.iter yield (T.reductions s t)

    (* Decoding the goto table. *)

    let encode_NoGoto = 0
    let decode_Goto c = c - 1

    let[@inline] goto state nt =
      let c = T.goto state nt in
      assert (c > encode_NoGoto);
      decode_Goto c

    (* Decoding the unique action bitmap. *)

    let uab_Fail = 1

    (* Decoding the unique action table. *)

    let ua_ReduceOpcode = 0b01
    let ua_ShiftOpcode = 0b10
    let ua_Fork = 0b11
    let ua_Fail = 0b00

    let unique_action (s : state) input =
      (* First determine whether the state [s] has a default reduction. *)
      let c = T.default_reduction s in
      if c <> dr_NoDefRed then `Reduce (decode_DefRed c) else
      (* There is no default reduction. Request the lookahead symbol. *)
      let token = Input.lookahead input in
      let t = T.token2terminal token in
      (* Consult the unique action bitmap. *)
      if T.unique_action_bitmap s t = uab_Fail then `Fail else
      (* Consult the unique action table.  *)
      let action = T.unique_action s t in
      assert (action <> ua_Fail);
      if action = ua_Fork then `Fork else
      let opcode = action land 0b11
      and param = action lsr 2 in
      assert (opcode = ua_ShiftOpcode || opcode = ua_ReduceOpcode);
      if opcode = ua_ShiftOpcode then `Shift param else `Reduce param

  end (* State *)

end
