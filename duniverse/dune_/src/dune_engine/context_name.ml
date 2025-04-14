open Import
include String

let default = "default"

let build_dir t = Path.Build.relative Path.Build.root t

let is_default = String.equal default

include (
  Stringlike.Make (struct
    type nonrec t = t

    let description_of_valid_string = None

    let hint_valid = None

    let to_string t = t

    let module_ = "Context_name"

    let description = "context name"

    let of_string_opt name =
      if
        name = ""
        || String.is_prefix name ~prefix:"."
        || name = "log" || name = "install" || String.contains name '/'
        || String.contains name '\\'
      then None
      else Some name
  end) :
    Stringlike_intf.S with type t := t)

let target t ~toolchain = sprintf "%s.%s" (to_string t) (to_string toolchain)

module Infix = Comparator.Operators (String)
module Top_closure = Top_closure.Make (String.Set) (Monad.Id)
