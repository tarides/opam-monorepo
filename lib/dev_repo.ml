open Import

type t = string

let compare = String.compare
let from_string s = s
let to_string t = t
let pp = Fmt.string

let rec repeat_while_some x ~f =
  match f x with None -> x | Some x -> repeat_while_some x ~f

(* Attempt to split a string by calling [split s], then choose a side of the
   result with [side], returning [s] if a split is not possible *)
let try_split_side s ~(split : string -> (string * string) option) ~side =
  Base.Option.value_map (split s) ~f:side ~default:s

let repo_name t =
  Uri.of_string t |> Uri.path
  |> repeat_while_some ~f:(Base.String.chop_suffix ~suffix:"/")
  |> try_split_side ~split:(Base.String.rsplit2 ~on:'/') ~side:snd
  |> repeat_while_some ~f:(Base.String.chop_prefix ~prefix:".")
  |> try_split_side ~split:(Base.String.lsplit2 ~on:'.') ~side:fst
  |> function
  | "" ->
      Rresult.R.error_msgf
        "unexpected empty string while computing name for dev_repo: \"%s\"" t
  | non_empty -> Ok non_empty

module Map = Map.Make (struct
  type nonrec t = t

  let compare = compare
end)
