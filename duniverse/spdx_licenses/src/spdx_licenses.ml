(* SPDX-License-Identifier: MIT *)

type user_defined_license = Types.user_defined_license = {
  document_ref : string option;
  license_ref : string;
}

type simple_license = Types.simple_license =
  | LicenseID of string
  | LicenseIDPlus of string
  | LicenseRef of user_defined_license

type t = Types.t =
  | Simple of simple_license
  | WITH of simple_license * string
  | AND of t * t
  | OR of t * t

type error = [
  | `InvalidLicenseID of string
  | `InvalidExceptionID of string
  | `ParseError
]

let ( >>= ) = Result.bind
let ( >|= ) x f = Result.map f x

let valid_license_ids = LicenseIDs.list
let valid_exception_ids = ExceptionIDs.list

let uppercased_valid_license_ids =
  List.map (fun x -> (x, String.uppercase_ascii x)) valid_license_ids

let uppercased_valid_exception_ids =
  List.map (fun x -> (x, String.uppercase_ascii x)) valid_exception_ids

let normalize_license_id id =
  let eq = String.equal (String.uppercase_ascii id) in
  match List.find (fun (_, up) -> eq up) uppercased_valid_license_ids with
  | (x, _) -> Ok x
  | exception Not_found -> Error (`InvalidLicenseID id)

let normalize_exception_id id =
  let eq = String.equal (String.uppercase_ascii id) in
  match List.find (fun (_, up) -> eq up) uppercased_valid_exception_ids with
  | (x, _) -> Ok x
  | exception Not_found -> Error (`InvalidExceptionID id)

let normalize_simple = function
  | LicenseID id -> normalize_license_id id >|= fun id -> LicenseID id
  | LicenseIDPlus id -> normalize_license_id id >|= fun id -> LicenseIDPlus id
  | LicenseRef _ as x -> Ok x

let rec normalize = function
  | Simple license ->
      normalize_simple license >|= fun license ->
      Simple license
  | WITH (simple, exc) ->
      normalize_simple simple >>= fun simple ->
      normalize_exception_id exc >|= fun exc ->
      WITH (simple, exc)
  | AND (x, y) ->
      normalize x >>= fun x ->
      normalize y >|= fun y ->
      AND (x, y)
  | OR (x, y) ->
      normalize x >>= fun x ->
      normalize y >|= fun y ->
      OR (x, y)

let parse s =
  let lexbuf = Lexing.from_string s in
  match Parser.main Lexer.main lexbuf with
  | license -> normalize license
  | exception (Lexer.Error | Parsing.Parse_error) -> Error `ParseError

let user_defined_license_to_string = function
  | {document_ref = None; license_ref} ->
      "LicenseRef-"^license_ref
  | {document_ref = Some document_ref; license_ref} ->
      "DocumentRef-"^document_ref^":"^"LicenseRef-"^license_ref

let simple_to_string = function
  | LicenseID x -> x
  | LicenseIDPlus x -> x^"+"
  | LicenseRef user_def -> user_defined_license_to_string user_def

let to_string =
  let rec aux ~prev = function
    | Simple x -> simple_to_string x
    | WITH (x, exc) -> simple_to_string x^" WITH "^exc
    | AND (x, y) ->
        let s = aux ~prev:`AND x^" AND "^aux ~prev:`AND y in
        begin match prev with
        | (`None | `AND) -> s
        | `OR -> "("^s^")"
        end
    | OR (x, y) ->
        let s = aux ~prev:`OR x^" OR "^aux ~prev:`OR y in
        begin match prev with
        | (`None | `OR) -> s
        | `AND -> "("^s^")"
        end
  in
  aux ~prev:`None
