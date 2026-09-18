(* This module extends [PPrint] with a notion of fragility. Fragility
   determines whether a space must (or need not) be inserted in the middle
   when two documents are concatenated. *)

type document

val fragile: PPrint.document -> document
val robust: PPrint.document -> document

val empty: document
val (^^): document -> document -> document
val group: document -> document
val nest: int -> document -> document
val space: document
val break: int -> document
val string: string -> document
val int: int -> document

module ToChannel : sig
  val pretty: float -> int -> out_channel -> document -> unit
end

module ToBuffer : sig
  val pretty: float -> int -> Buffer.t -> document -> unit
end
