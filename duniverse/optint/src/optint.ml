(** Extraction of [Stdlib.Sys.Immediate64] for pre-4.10 compatibility.
    [Immediate64] was originally authored by Jeremie Dimino <jeremie@dimino.org>,
    and is licensed along with the OCaml compiler system under LGPLv2. See the
    {{:https://github.com/ocaml/ocaml/blob/trunk/LICENSE} compiler license} for
    details.

    For soundness of the [@@immediate64] annotation, we ensure to use the boxed
    representation only when not on 64-bit platforms, but we need to use The
    Force to convince the type system of this fact. *)
module Immediate64 = struct
  module type Non_immediate = sig
    type t
  end

  module type Immediate = sig
    type t [@@immediate]
  end

  module Make (Immediate : Immediate) (Non_immediate : Non_immediate) = struct
    type t [@@immediate64]

    type 'a repr =
      | Immediate : Immediate.t repr
      | Non_immediate : Non_immediate.t repr

    external magic : _ repr -> t repr = "%identity"

    let repr =
      if Sys.word_size = 64 then magic Immediate else magic Non_immediate
  end
end

module Conditional = struct
  type ('t, 'u, 'v) t =
    | True : ('t, 't, _) t (** therefore ['t] = ['u] *)
    | False : ('t, _, 't) t (** therefore ['t] = ['v] *)
end

module Optint = struct
  include Immediate64.Make (Optint_native) (Optint_emul)

  module type S = Integer_interface.S with type t := t

  let impl : (module S) =
    match repr with
    | Immediate -> (module Optint_native : S)
    | Non_immediate -> (module Optint_emul : S)

  include (val impl : S)

  let is_immediate : (t, int, int32) Conditional.t =
    match repr with
    | Immediate -> True
    | Non_immediate -> False
end

module Int63 = struct
  include Immediate64.Make (Int63_native) (Int63_emul)

  module type S = Integer_interface.S with type t := t

  let impl : (module S) =
    match repr with
    | Immediate -> (module Int63_native : S)
    | Non_immediate -> (module Int63_emul : S)

  include (val impl : S)

  module Boxed = Int63_emul

  let is_immediate : (t, int, Boxed.t) Conditional.t =
    match repr with
    | Immediate -> True
    | Non_immediate -> False
end

include Optint
