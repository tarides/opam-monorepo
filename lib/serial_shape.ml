open Import

module Conv = struct
  type ('repr, 'true_type) t = {
    from_repr : 'repr -> ('true_type, Rresult.R.msg) result;
    to_repr : 'true_type -> 'repr;
    equal : ('true_type -> 'true_type -> bool) option;
    pp : 'true_type Fmt.t option;
  }

  let make ~from_repr ~to_repr ?equal ?pp () = { from_repr; to_repr; equal; pp }
end

type _ t =
  | Sbool : bool t
  | Sstring : string t
  | Slist : 'a t -> 'a list t
  | Spair : 'a t * 'b t -> ('a * 'b) t
  | Choice2 : 'a t * 'b t -> [ `C1 of 'a | `C2 of 'b ] t
  | Choice3 : 'a t * 'b t * 'c t -> [ `C1 of 'a | `C2 of 'b | `C3 of 'c ] t
  | Conv : ('repr, 'true_type) Conv.t * 'repr t -> 'true_type t

let bool = Sbool
let string = Sstring
let list s = Slist s
let pair s s' = Spair (s, s')
let choice2 s s' = Choice2 (s, s')
let choice3 s s' s'' = Choice3 (s, s', s'')
let conv conv s = Conv (conv, s)

let rec shallow_description : type a. a t -> string = function
  | Slist _ -> "a list"
  | Spair _ -> "a pair"
  | Sstring -> "a string"
  | Sbool -> "a boolean"
  | Choice2 (s, s') ->
      let d = shallow_description s in
      let d' = shallow_description s' in
      Printf.sprintf "%s or %s" d d'
  | Choice3 (s, s', s'') ->
      let d = shallow_description s in
      let d' = shallow_description s' in
      let d'' = shallow_description s'' in
      Printf.sprintf "%s, %s or %s" d d' d''
  | Conv (_, s) -> shallow_description s

let rec from_opam_val :
    type a. a t -> OpamParserTypes.FullPos.value -> (a, Rresult.R.msg) result =
 fun shape value ->
  let open Result.O in
  let parse_error () =
    let expected = shallow_description shape in
    Opam.Pos.unexpected_value_error ~expected value
  in
  match (shape, value) with
  | Sbool, { pelem = Bool b; _ } -> Ok b
  | Sstring, { pelem = String s; _ } -> Ok s
  | Spair (s, s'), { pelem = List { pelem = [ v; v' ]; _ }; _ } ->
      let* fst = from_opam_val s v in
      let* snd = from_opam_val s' v' in
      Ok (fst, snd)
  | Slist s, { pelem = List _; _ } ->
      Opam.Value.List.from_value (from_opam_val s) value
  | Conv (conv, s), value -> (
      let* repr = from_opam_val s value in
      let res = conv.from_repr repr in
      match res with
      | Ok _ as res -> res
      | Error (`Msg msg) -> Opam.Pos.value_errorf ~value "%s" msg)
  | Choice2 (s, s'), value -> (
      match from_opam_val s value with
      | Ok c1 -> Ok (`C1 c1)
      | Error _ -> (
          match from_opam_val s' value with
          | Ok c2 -> Ok (`C2 c2)
          | Error _ -> parse_error ()))
  | Choice3 (s, s', s''), value -> (
      match from_opam_val s value with
      | Ok c1 -> Ok (`C1 c1)
      | Error _ -> (
          match from_opam_val s' value with
          | Ok c2 -> Ok (`C2 c2)
          | Error _ -> (
              match from_opam_val s'' value with
              | Ok c3 -> Ok (`C3 c3)
              | Error _ -> parse_error ())))
  | _ -> parse_error ()

let rec to_opam_val : type a. a t -> a -> OpamParserTypes.FullPos.value =
 fun shape value ->
  match (shape, value) with
  | Sbool, b -> Opam.Pos.with_default (OpamParserTypes.FullPos.Bool b)
  | Sstring, s -> Opam.Value.String.to_value s
  | Spair (s, s'), (v, v') ->
      let fst = to_opam_val s v in
      let snd = to_opam_val s' v' in
      Opam.Value.List.to_value Fun.id [ fst; snd ]
  | Slist s, l -> Opam.Value.List.to_value (to_opam_val s) l
  | Conv (conv, s), value ->
      let repr = conv.to_repr value in
      to_opam_val s repr
  | Choice2 (s, _), `C1 v -> to_opam_val s v
  | Choice2 (_, s), `C2 v -> to_opam_val s v
  | Choice3 (s, _, _), `C1 v -> to_opam_val s v
  | Choice3 (_, s, _), `C2 v -> to_opam_val s v
  | Choice3 (_, _, s), `C3 v -> to_opam_val s v

let unmatched_list_delimiter ~delim =
  Rresult.R.error_msgf "unmatched list delimiter '%c'" delim

type tokbuf = { buffer : string; pos : int }

let tokbuf_of_string buffer = { buffer; pos = 0 }

let peek_char { buffer; pos } =
  match buffer.[pos] with c -> Some c | exception Invalid_argument _ -> None

let next tokbuf = { tokbuf with pos = tokbuf.pos + 1 }

type token = OPEN_PAREN | CLOSE_PAREN | COMMA | TEXT of string

let remaining { buffer; pos } =
  String.sub ~pos ~len:(String.length buffer - pos) buffer

let until_next ~chars ({ buffer; pos } as tokbuf) =
  let rec find index =
    match buffer.[index] with
    | c -> (
        match List.mem c ~set:chars with
        | true ->
            let tokbuf = { buffer; pos = index } in
            (Some index, tokbuf)
        | false -> find (index + 1))
    | exception Invalid_argument _ -> (None, tokbuf)
  in
  match find pos with
  | Some index, tokbuf ->
      let text = String.sub buffer ~pos ~len:(index - pos) in
      (Some (TEXT text), tokbuf)
  | None, tokbuf -> (None, tokbuf)

let rec tokenize acc s =
  match peek_char s with
  | None -> List.rev acc
  | Some '[' -> tokenize (OPEN_PAREN :: acc) (next s)
  | Some ']' -> tokenize (CLOSE_PAREN :: acc) (next s)
  | Some ',' -> tokenize (COMMA :: acc) (next s)
  | Some _ -> (
      match until_next ~chars:[ '['; ']'; ',' ] s with
      | Some token, s -> tokenize (token :: acc) s
      | None, s ->
          let t = TEXT (remaining s) in
          List.rev (t :: acc))

let add_to_front_buffer s = function
  | [] ->
      let buf = Buffer.create 16 in
      Buffer.add_string buf s;
      [ buf ]
  | buf :: _ as v ->
      Buffer.add_string buf s;
      v

let split_list s =
  let open Result.O in
  let buf = tokbuf_of_string s in
  let tokens = tokenize [] buf in
  let level, lst =
    List.fold_left
      ~f:(fun (level, res) token ->
        match res with
        | Error _ -> (level, res)
        | Ok acc -> (
            match token with
            | OPEN_PAREN ->
                let acc = add_to_front_buffer "[" acc in
                (level + 1, Ok acc)
            | CLOSE_PAREN -> (
                match level > 0 with
                | true ->
                    let acc = add_to_front_buffer "]" acc in
                    (level - 1, Ok acc)
                | false -> (level, unmatched_list_delimiter ~delim:']'))
            | COMMA -> (
                match level = 0 with
                | true ->
                    let buf = Buffer.create 16 in
                    let acc = buf :: acc in
                    (level, Ok acc)
                | false ->
                    let acc = add_to_front_buffer "," acc in
                    (level, Ok acc))
            | TEXT s ->
                let acc = add_to_front_buffer s acc in
                (level, Ok acc)))
      ~init:(0, Ok []) tokens
  in
  let* lst = lst in
  match level = 0 with
  | false -> unmatched_list_delimiter ~delim:'['
  | true -> Ok (List.rev_map ~f:Buffer.contents lst)

let parse_list s =
  let len = String.length s in
  if len = 0 then Ok []
  else
    match (s.[0], s.[len - 1]) with
    | '[', ']' -> split_list (String.sub s ~pos:1 ~len:(len - 2))
    | ('[' as delim), _ -> unmatched_list_delimiter ~delim
    | _ -> Rresult.R.error_msg "list or pairs must be delimited by '[' and ']'"

let rec cmdliner_parse : type a. a t -> string -> (a, Rresult.R.msg) result =
 fun shape value ->
  let open Result.O in
  let parse_error () =
    let expected = shallow_description shape in
    Rresult.R.error_msgf "Expected %s but got: %s" expected value
  in
  match (shape, value) with
  | Sbool, "true" -> Ok true
  | Sbool, "false" -> Ok false
  | Sstring, s -> Ok s
  | Spair (s, s'), value -> (
      let* l = parse_list value in
      match l with
      | [ v; v' ] ->
          let* fst = cmdliner_parse s v in
          let* snd = cmdliner_parse s' v' in
          Ok (fst, snd)
      | _ -> parse_error ())
  | Slist s, value ->
      let* l = parse_list value in
      Result.List.map l ~f:(cmdliner_parse s)
  | Conv (conv, s), value ->
      let* repr = cmdliner_parse s value in
      conv.from_repr repr
  | Choice2 (s, s'), value -> (
      match cmdliner_parse s value with
      | Ok c1 -> Ok (`C1 c1)
      | Error _ -> (
          match cmdliner_parse s' value with
          | Ok c2 -> Ok (`C2 c2)
          | Error _ -> parse_error ()))
  | Choice3 (s, s', s''), value -> (
      match cmdliner_parse s value with
      | Ok c1 -> Ok (`C1 c1)
      | Error _ -> (
          match cmdliner_parse s' value with
          | Ok c2 -> Ok (`C2 c2)
          | Error _ -> (
              match cmdliner_parse s'' value with
              | Ok c3 -> Ok (`C3 c3)
              | Error _ -> parse_error ())))
  | _ -> parse_error ()

let rec cmdliner_print : type a. a t -> Format.formatter -> a -> unit =
 fun shape fmt value ->
  match (shape, value) with
  | Sbool, b -> Fmt.pf fmt "%a" Fmt.bool b
  | Sstring, s -> Fmt.pf fmt "%s" s
  | Spair (s, s'), (v, v') ->
      Fmt.pf fmt "[%a,%a]" (cmdliner_print s) v (cmdliner_print s') v'
  | Slist s, l ->
      Fmt.pf fmt "[%a]"
        (Fmt.list ~sep:Fmt.(const char ',') (cmdliner_print s))
        l
  | Conv (conv, s), value ->
      let repr = conv.to_repr value in
      cmdliner_print s fmt repr
  | Choice2 (s, _), `C1 v -> cmdliner_print s fmt v
  | Choice2 (_, s), `C2 v -> cmdliner_print s fmt v
  | Choice3 (s, _, _), `C1 v -> cmdliner_print s fmt v
  | Choice3 (_, s, _), `C2 v -> cmdliner_print s fmt v
  | Choice3 (_, _, s), `C3 v -> cmdliner_print s fmt v

let cmdliner_conv shape =
  let parse = cmdliner_parse shape in
  let print = cmdliner_print shape in
  Cmdliner.Arg.conv (parse, print)

let rec equal : type a. a t -> a -> a -> bool =
 fun shape v v' ->
  match (shape, v, v') with
  | Sbool, b, b' -> Bool.equal b b'
  | Sstring, s, s' -> String.equal s s'
  | Spair (sfst, ssnd), (fst, snd), (fst', snd') ->
      equal sfst fst fst' && equal ssnd snd snd'
  | Slist s, l, l' -> List.equal (equal s) l l'
  | Choice2 (s, _), `C1 v, `C1 v' -> equal s v v'
  | Choice2 (_, s), `C2 v, `C2 v' -> equal s v v'
  | Choice2 _, _, _ -> false
  | Choice3 (s, _, _), `C1 v, `C1 v' -> equal s v v'
  | Choice3 (_, s, _), `C2 v, `C2 v' -> equal s v v'
  | Choice3 (_, _, s), `C3 v, `C3 v' -> equal s v v'
  | Choice3 _, _, _ -> false
  | Conv ({ equal = Some eq; _ }, _), v, v' -> eq v v'
  | Conv (conv, s), v, v' ->
      let repr = conv.to_repr v in
      let repr' = conv.to_repr v' in
      equal s repr repr'

let rec pp : type a. a t -> a Fmt.t =
 fun shape fmt v ->
  match (shape, v) with
  | Sbool, b -> Fmt.bool fmt b
  | Sstring, s -> Fmt.pf fmt "%S" s
  | Spair (s, s'), (v, v') -> Fmt.pf fmt "(%a, %a)" (pp s) v (pp s') v'
  | Slist s, l ->
      Fmt.pf fmt "[%a]" (Fmt.list ~sep:Fmt.(const char ';') (pp s)) l
  | Choice2 (s, _), `C1 v -> pp s fmt v
  | Choice2 (_, s), `C2 v -> pp s fmt v
  | Choice3 (s, _, _), `C1 v -> pp s fmt v
  | Choice3 (_, s, _), `C2 v -> pp s fmt v
  | Choice3 (_, _, s), `C3 v -> pp s fmt v
  | Conv ({ pp = Some pp; _ }, _), v -> pp fmt v
  | Conv (conv, s), v ->
      let repr = conv.to_repr v in
      pp s fmt repr
