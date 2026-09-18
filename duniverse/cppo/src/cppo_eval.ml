open Printf

open Cppo_types

module S = Set.Make (String)
module M = Map.Make (String)

let find_opt name env =
  try Some (M.find name env)
  with Not_found -> None

(* An environment entry. *)

(* In a macro definition [EDef (loc, formals, body, env)],

   + [loc] is the location of the macro definition,
   + [formals] is the list of formal parameters,
   + [body] and [env] represent the closed body of the macro definition. *)

type entry =
  | EDef of loc * formals * body * env

(* An environment is a map of (macro) names to environment entries. *)

and env =
  entry M.t

let basic x : formal =
  (x, base)

let ident x =
  `Ident (dummy_loc, x, [])

let dummy_defun formals body env =
  EDef (dummy_loc, List.map basic formals, body, env)

let builtins : (string * (env -> entry)) list = [
  "STRINGIFY",
  dummy_defun
    ["x"]
    (`Stringify (ident "x"))
  ;
  "CONCAT",
  dummy_defun
    ["x";"y"]
    (`Concat (ident "x", ident "y"))
  ;
  "CAPITALIZE",
  dummy_defun
    ["x"]
    (`Capitalize (ident "x"))
  ;
]

let is_reserved s =
  s = "__FILE__" ||
  s = "__LINE__" ||
  List.exists (fun (s', _) -> s = s') builtins

let builtin_env : env =
  List.fold_left (fun env (s, f) -> M.add s (f env) env) M.empty builtins

let line_directive buf pos =
  let len = Buffer.length buf in
  if len > 0 && Buffer.nth buf (len - 1) <> '\n' then
    Buffer.add_char buf '\n';
  bprintf buf "# %i %S\n"
    pos.Lexing.pos_lnum
    pos.Lexing.pos_fname;
  bprintf buf "%s" (String.make (pos.Lexing.pos_cnum - pos.Lexing.pos_bol) ' ')

let rec add_sep sep last = function
    [] -> [ last ]
  | [x] -> [ x; last ]
  | x :: l -> x :: sep :: add_sep sep last l

(* Transform a list of actual macro arguments back into ordinary text,
   after discovering that they are not macro arguments after all. *)
let text loc name (actuals : actuals) : node list =
  match actuals with
  | [] ->
      [`Text (loc, false, name)]
  | _ :: _ ->
      `Text (loc, false, name ^ "(") ::
      add_sep
        (`Text (loc, false, ","))
        (`Text (loc, false, ")"))
        actuals

let trim_and_compact buf s =
  let started = ref false in
  let need_space = ref false in
  for i = 0 to String.length s - 1 do
    match s.[i] with
        ' ' | '\t' | '\n' | '\r' ->
          if !started then
            need_space := true
      | c ->
          if !need_space then
            Buffer.add_char buf ' ';
          (match c with
               '\"' -> Buffer.add_string buf "\\\""
             | '\\' -> Buffer.add_string buf "\\\\"
             | c -> Buffer.add_char buf c);
          started := true;
          need_space := false
  done

let stringify buf s =
  Buffer.add_char buf '\"';
  trim_and_compact buf s;
  Buffer.add_char buf '\"'

let trim_and_compact_string s =
  let buf = Buffer.create (String.length s) in
  trim_and_compact buf s;
  Buffer.contents buf

let trim_compact_and_capitalize_string s =
  let buf = Buffer.create (String.length s) in
  trim_and_compact buf s;
  String.capitalize_ascii (Buffer.contents buf)

let is_ident s =
  let len = String.length s in
  len > 0
  &&
    (match s.[0] with
         'A'..'Z' | 'a'..'z' -> true
       | '_' when len > 1 -> true
       | _ -> false)
  &&
    (try
       for i = 1 to len - 1 do
         match s.[i] with
             'A'..'Z' | 'a'..'z' | '_' | '0'..'9' -> ()
           | _ -> raise Exit
       done;
       true
     with Exit ->
       false)

let concat loc x y =
  let s = trim_and_compact_string x ^ trim_and_compact_string y in
  if not (s = "" || is_ident s) then
    error loc
      (sprintf "CONCAT() does not expand into a valid identifier nor \
                into whitespace:\n%S" s)
  else
    if s = "" then " "
    else " " ^ s ^ " "

let int_expansion_error loc name =
    error loc
      (sprintf "\
Variable %s found in cppo boolean expression must expand
into an int literal, into a tuple of int literals,
or into a variable with the same properties."
         name)

let rec int_expansion loc name (node : node) : string =
  match node with
  | `Text (_loc, _is_space, s) ->
      s
  | `Seq (_loc, nodes) ->
      List.map (int_expansion loc name) nodes
      |> String.concat ""
  | _ ->
      int_expansion_error loc name

(*
   Expand the contents of a variable used in a boolean expression.

   Ideally, we should first completely expand the contents bound
   to the variable, and then parse the result as an int or an int tuple.
   This is a bit complicated to do well, and we don't want to implement
   a full programming language here either.

   Instead we only accept int literals, int tuple literals, and variables that
   themselves expand into one those.

   In particular:
   - We do not support arithmetic operations
   - We do not support tuples containing variables such as (x, y)

   Example of contents that we support:
   - 123
   - (1, 2, 3)
   - x, where x expands into 123.
*)
let rec eval_ident env loc name =
  let body =
    match find_opt name env with
    | Some (EDef (_loc, [], body, _env)) ->
        body
    | Some (EDef _) ->
        error loc (sprintf "%S expects arguments" name)
    | None ->
        error loc (sprintf "Undefined identifier %S" name)
  in
  (try
     match node_is_ident body with
     | Some (loc, name) ->
         (* single identifier that we expand recursively *)
         eval_ident env loc name
     | None ->
         (* int literal or int tuple literal; variables not allowed *)
         let s = int_expansion loc name body in
         (match Cppo_lexer.int_tuple_of_string s with
            Some [i] -> `Int i
          | Some l -> `Tuple (loc, List.map (fun i -> `Int i) l)
          | None ->
              int_expansion_error loc name
         )
   with Cppo_error _ ->
     int_expansion_error loc name
  )

let rec replace_idents env (x : arith_expr) : arith_expr =
  match x with
    | `Ident (loc, name) -> eval_ident env loc name

    | `Int x -> `Int x
    | `Neg x -> `Neg (replace_idents env x)
    | `Add (a, b) -> `Add (replace_idents env a, replace_idents env b)
    | `Sub (a, b) -> `Sub (replace_idents env a, replace_idents env b)
    | `Mul (a, b) -> `Mul (replace_idents env a, replace_idents env b)
    | `Div (loc, a, b) -> `Div (loc, replace_idents env a, replace_idents env b)
    | `Mod (loc, a, b) -> `Mod (loc, replace_idents env a, replace_idents env b)
    | `Lnot a -> `Lnot (replace_idents env a)
    | `Lsl (a, b) -> `Lsl (replace_idents env a, replace_idents env b)
    | `Lsr (a, b) -> `Lsr (replace_idents env a, replace_idents env b)
    | `Asr (a, b) -> `Asr (replace_idents env a, replace_idents env b)
    | `Land (a, b) -> `Land (replace_idents env a, replace_idents env b)
    | `Lor (a, b) -> `Lor (replace_idents env a, replace_idents env b)
    | `Lxor (a, b) -> `Lxor (replace_idents env a, replace_idents env b)
    | `Tuple (loc, l) -> `Tuple (loc, List.map (replace_idents env) l)

let rec eval_int env (x : arith_expr) : int64 =
  match x with
    | `Ident (loc, name) -> eval_int env (eval_ident env loc name)

    | `Int x -> x
    | `Neg x -> Int64.neg (eval_int env x)
    | `Add (a, b) -> Int64.add (eval_int env a) (eval_int env b)
    | `Sub (a, b) -> Int64.sub (eval_int env a) (eval_int env b)
    | `Mul (a, b) -> Int64.mul (eval_int env a) (eval_int env b)
    | `Div (loc, a, b) ->
        (try Int64.div (eval_int env a) (eval_int env b)
         with Division_by_zero ->
           error loc "Division by zero")

    | `Mod (loc, a, b) ->
        (try Int64.rem (eval_int env a) (eval_int env b)
         with Division_by_zero ->
           error loc "Division by zero")

    | `Lnot a -> Int64.lognot (eval_int env a)

    | `Lsl (a, b) ->
        let n = eval_int env a in
        let shift = eval_int env b in
        let shift =
          if shift >= 64L then 64L
          else if shift <= -64L then -64L
          else shift
        in
        Int64.shift_left n (Int64.to_int shift)

    | `Lsr (a, b) ->
        let n = eval_int env a in
        let shift = eval_int env b in
        let shift =
          if shift >= 64L then 64L
          else if shift <= -64L then -64L
          else shift
        in
        Int64.shift_right_logical n (Int64.to_int shift)

    | `Asr (a, b) ->
        let n = eval_int env a in
        let shift = eval_int env b in
        let shift =
          if shift >= 64L then 64L
          else if shift <= -64L then -64L
          else shift
        in
        Int64.shift_right n (Int64.to_int shift)

    | `Land (a, b) -> Int64.logand (eval_int env a) (eval_int env b)
    | `Lor (a, b) -> Int64.logor (eval_int env a) (eval_int env b)
    | `Lxor (a, b) -> Int64.logxor (eval_int env a) (eval_int env b)
    | `Tuple (loc, l) ->
        assert (List.length l <> 1);
        error loc "Operation not supported on tuples"

let rec compare_lists al bl =
  match al, bl with
  | a :: al, b :: bl ->
      let c = Int64.compare a b in
      if c <> 0 then c
      else compare_lists al bl
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1

let compare_tuples env (a : arith_expr) (b : arith_expr) =
  (* We replace the identifiers first to get a better error message
     on such input:

       #define x (1, 2)
       #if x >= (1, 2)

     since variables must represent a single int, not a tuple.
  *)
  let a = replace_idents env a in
  let b = replace_idents env b in
  match a, b with
  | `Tuple (_, al), `Tuple (_, bl) when List.length al = List.length bl ->
      let eval_list l = List.map (eval_int env) l in
      compare_lists (eval_list al) (eval_list bl)

  | `Tuple (_loc1, al), `Tuple (loc2, bl) ->
      error loc2
        (sprintf "Tuple of length %i cannot be compared to a tuple of length %i"
           (List.length bl) (List.length al)
        )

  | `Tuple (loc, _), _
  | _, `Tuple (loc, _) ->
      error loc "Tuple cannot be compared to an int"

  | a, b ->
      Int64.compare (eval_int env a) (eval_int env b)

let rec eval_bool env (x : bool_expr) =
  match x with
      `True -> true
    | `False -> false
    | `Defined s -> M.mem s env
    | `Not x -> not (eval_bool env x)
    | `And (a, b) -> eval_bool env a && eval_bool env b
    | `Or (a, b) -> eval_bool env a || eval_bool env b
    | `Eq (a, b) -> compare_tuples env a b = 0
    | `Lt (a, b) -> compare_tuples env a b < 0
    | `Gt (a, b) -> compare_tuples env a b > 0


type globals = {
  call_loc : Cppo_types.loc;
    (* location used to set the value of
       __FILE__ and __LINE__ global variables;
       also used in the expansion of CONCAT *)

  mutable buf : Buffer.t;
    (* buffer where the output is written *)

  included : S.t;
    (* set of already-included files *)

  require_location : bool ref;
    (* whether a line directive should be printed before outputting the next
       token *)

  show_exact_locations : bool;
    (* whether line directives should be printed even for expanded macro
       bodies *)

  enable_loc : bool ref;
    (* whether line directives should be printed *)

  g_preserve_quotations : bool;
    (* identify and preserve camlp4 quotations *)

  incdirs : string list;
    (* directories for finding included files *)

  current_directory : string;
    (* directory containing the current file *)

  extensions : (string, Cppo_command.command_template) Hashtbl.t;
    (* mapping from extension ID to pipeline command *)
}

(* [preserving_enable_loc g action] saves [g.enable_loc], runs [action()],
   then restores [g.enable_loc]. The result of [action()] is returned. *)
let preserving_enable_loc g action =
  let enable_loc0 = !(g.enable_loc) in
  let result = action() in
  g.enable_loc := enable_loc0;
  result

let parse ~preserve_quotations file lexbuf =
  let lexer_env = Cppo_lexer.init ~preserve_quotations file lexbuf in
  try
    Cppo_parser.main (Cppo_lexer.line lexer_env) lexbuf
  with
      Parsing.Parse_error ->
        error (Cppo_lexer.long_loc lexer_env) "syntax error"
    | Cppo_types.Cppo_error _ as e ->
        raise e
    | e ->
        error (Cppo_lexer.long_loc lexer_env) (Printexc.to_string e)

let plural n =
  if abs n <= 1 then ""
  else "s"


let maybe_print_location g pos =
  if !(g.enable_loc) then
    if !(g.require_location) then (
      line_directive g.buf pos
    )

let expand_ext g loc id data =
  let cmd_tpl =
    try Hashtbl.find g.extensions id
    with Not_found ->
      error loc (sprintf "Undefined extension %s" id)
  in
  let p1, p2 = loc in
  let file = p1.Lexing.pos_fname in
  let first = p1.Lexing.pos_lnum in
  let last = p2.Lexing.pos_lnum in
  let cmd = Cppo_command.subst cmd_tpl file first last in
  Unix.putenv "CPPO_FILE" file;
  Unix.putenv "CPPO_FIRST_LINE" (string_of_int first);
  Unix.putenv "CPPO_LAST_LINE" (string_of_int last);
  let (ic, oc) as p = Unix.open_process cmd in
  output_string oc data;
  close_out oc;
  (try
     while true do
       bprintf g.buf "%s\n" (input_line ic)
     done
   with End_of_file -> ()
  );
  match Unix.close_process p with
      Unix.WEXITED 0 -> ()
    | Unix.WEXITED n ->
        failwith (sprintf "Command %S exited with status %i" cmd n)
    | _ ->
        failwith (sprintf "Command %S failed" cmd)

let check_arity loc name (formals : _ list) (actuals : _ list) =
  let formals = List.length formals
  and actuals = List.length actuals in
  if formals <> actuals then
    sprintf "%S expects %i argument%s but is applied to %i argument%s."
      name formals (plural formals) actuals (plural actuals)
    |> error loc

(* [macro_of_node node] checks that [node] is a single identifier,
   possibly surrounded with whitespace, and returns this identifier
   as well as its location. *)
let macro_of_node (node : node) : loc * macro =
  match node_is_ident node with
  | Some (loc, x) ->
      loc, x
  | None ->
      sprintf "The name of a macro is expected in this position"
      |> error (node_loc node)

(* [fetch loc x env] checks that the macro [x] exists in [env]
   and fetches its definition.  *)
let fetch loc (x : macro) env : entry =
  match find_opt x env with
  | None ->
      sprintf "The macro '%s' is not defined" x
      |> error loc
  | Some def ->
      def

(* [entry_shape def] returns the shape of the macro that is defined
   by the environment entry [def]. *)
let entry_shape (entry : entry) : shape =
  let EDef (_loc, formals, _body, _env) = entry in
  Shape (List.map snd formals)

(* [check_shape loc expected provided] checks that the shapes
   [expected] and [provided] are equal. *)
let check_shape loc expected provided =
  if not (same_shape expected provided) then
    sprintf "A macro of type %s was expected, but\n       \
             a macro of type %s was provided"
      (print_shape expected) (print_shape provided)
    |> error loc

(* [bind_one formal (loc, actual, env) accu] binds one formal parameter
   to one actual argument, extending the environment [accu]. *)
let bind_one (formal : formal) (loc, actual, env) accu =
  let (x : macro), (expected : shape) = formal in
  (* Analyze the shape of this formal parameter. *)
  match expected with
  | Shape [] ->
      (* This formal parameter has the base shape: it is an ordinary
         parameter. It becomes an ordinary (unparameterized) macro:
         the name [x] becomes bound to the closure [actual, env]. *)
      M.add x (EDef (loc, [], actual, env)) accu
  | _ ->
      (* This formal parameter has a shape other than the base shape:
         it is itself a parameterized macro. In that case, we expect
         the actual parameter to be just a name [y]. *)
      let loc, y = macro_of_node actual in
      (* Check that the macro [y] exists, and fetch its definition. *)
      let def = fetch loc y env in
      (* Compute its shape. *)
      let provided = entry_shape def in
      (* Check that the shapes match. *)
      check_shape loc expected provided;
      (* Now bind [x] to the definition of [y]. *)
      (* This is analogous to [let x = y] in OCaml. *)
      M.add x def accu

(* [bind_many formals (loc, actuals, env) accu] binds a tuple of formal
   parameters to a tuple of actual arguments, extending the environment
   [accu]. *)
let bind_many formals (loc, actuals, env) accu =
  List.fold_left2 (fun accu formal actual ->
    bind_one formal (loc, actual, env) accu
  ) accu formals actuals

let rec include_file g loc rel_file env =
  let file =
    if not (Filename.is_relative rel_file) then
      if Sys.file_exists rel_file then
        rel_file
      else
        error loc (sprintf "Included file %S does not exist" rel_file)
    else
      try
        let dir =
          List.find (
            fun dir ->
              let file = Filename.concat dir rel_file in
              Sys.file_exists file
          ) (g.current_directory :: g.incdirs)
        in
        if dir = Filename.current_dir_name then
          rel_file
        else
          Filename.concat dir rel_file
      with Not_found ->
        error loc (sprintf "Cannot find included file %S" rel_file)
  in
  if S.mem file g.included then
    failwith (sprintf "Cyclic inclusion of file %S" file)
  else
    let ic = open_in file in
    let lexbuf = Lexing.from_channel ic in
    let l = parse ~preserve_quotations:g.g_preserve_quotations file lexbuf in
    close_in ic;
    expand_list { g with
                    included = S.add file g.included;
                    current_directory = Filename.dirname file
                } env l

and expand_list ?(top = false) g env l =
  List.fold_left (expand_node ~top g) env l

(* [expand_ident] is the special case of [expand_node] where the node is
   an identifier [`Ident (loc, name, actuals)]. *)
and expand_ident ~top g env0 loc name (actuals : actuals) =

  (* Test whether there exists a definition for the macro [name]. *)
  let def = find_opt name env0 in
  match def with
  | None ->
      (* There is no definition for the macro [name], so this is not
         a macro application after all. Transform it back into text,
         and process it. *)
      expand_list g env0 (text loc name actuals)
  | Some def ->
      expand_macro_application ~top g env0 loc name actuals def

(* [expand_macro_application] is the special case of [expand_ident] where
   it turns out that the identifier [name] is a macro. *)
and expand_macro_application ~top g env0 loc name actuals def =

  let g =
    if top || g.call_loc == dummy_loc then
      { g with call_loc = loc }
    else g
  in

  preserving_enable_loc g @@ fun () ->

  g.require_location := true;

  if not g.show_exact_locations then (
    (* error reports will point more or less to the point
       where the code is included rather than the source location
       of the macro definition *)
    maybe_print_location g (fst loc);
    g.enable_loc := false
  );

  let EDef (_loc, formals, body, env) = def in
  (* Check that this macro is applied to a correct number of arguments. *)
  check_arity loc name formals actuals;
  (* Extend the macro's captured environment [env] with bindings of
     formals to actuals. Each actual captures the environment [env0]
     that exists here, at the macro application site. *)
  let env = bind_many formals (loc, actuals, env0) env in
  (* Process the macro's body in this extended environment. *)
  let (_ : env) = expand_node g env body in

  g.require_location := true;

  (* Continue with our original environment. *)
  env0

and expand_node ?(top = false) g env0 (x : node) =
  match x with

    | `Ident (loc, name, actuals) ->
        expand_ident ~top g env0 loc name actuals

    | `Def (loc, name, formals, body)->
        g.require_location := true;
        if M.mem name env0 then
          error loc (sprintf "%S is already defined" name)
        else
          M.add name (EDef (loc, formals, body, env0)) env0

    | `Scope body ->
        (* A [body] is just a [node]. We expand this node, and drop
           the resulting environment; instead, we return the current
           environment. *)
        let env = expand_node ~top g env0 body in
        ignore env;
        env0

    | `Undef (loc, name) ->
        g.require_location := true;
        if is_reserved name then
          error loc
            (sprintf "%S is a built-in variable that cannot be undefined" name)
        else
          M.remove name env0

    | `Include (loc, file) ->
        g.require_location := true;
        let env = include_file g loc file env0 in
        g.require_location := true;
        env

    | `Ext (loc, id, data) ->
        g.require_location := true;
        expand_ext g loc id data;
        g.require_location := true;
        env0

    | `Cond (_loc, test, if_true, if_false) ->
        let l =
          if eval_bool env0 test then if_true
          else if_false
        in
        g.require_location := true;
        let env = expand_list g env0 l in
        g.require_location := true;
        env

    | `Error (loc, msg) ->
        error loc msg

    | `Warning (loc, msg) ->
        warning loc msg;
        env0

    | `Text (loc, is_space, s) ->
        if not is_space then (
          maybe_print_location g (fst loc);
          g.require_location := false
        );
        Buffer.add_string g.buf s;
        env0

    | `Seq (_loc, l) ->
        expand_list g env0 l

    | `Stringify x ->
        preserving_enable_loc g @@ fun () ->
        g.enable_loc := false;
        let buf0 = g.buf in
        let local_buf = Buffer.create 100 in
        g.buf <- local_buf;
        ignore (expand_node g env0 x);
        stringify buf0 (Buffer.contents local_buf);
        g.buf <- buf0;
        env0

    | `Capitalize (x : node) ->
        preserving_enable_loc g @@ fun () ->
        g.enable_loc := false;
        let buf0 = g.buf in
        let local_buf = Buffer.create 100 in
        g.buf <- local_buf;
        ignore (expand_node g env0 x);
        let xs = Buffer.contents local_buf in
        let s = trim_compact_and_capitalize_string xs in
          (* stringify buf0 (Buffer.contents local_buf); *)
        Buffer.add_string buf0 s ;
        g.buf <- buf0;
        env0

    | `Concat (x, y) ->
        preserving_enable_loc g @@ fun () ->
        g.enable_loc := false;
        let buf0 = g.buf in
        let local_buf = Buffer.create 100 in
        g.buf <- local_buf;
        ignore (expand_node g env0 x);
        let xs = Buffer.contents local_buf in
        Buffer.clear local_buf;
        ignore (expand_node g env0 y);
        let ys = Buffer.contents local_buf in
        let s = concat g.call_loc xs ys in
        Buffer.add_string buf0 s;
        g.buf <- buf0;
        env0

    | `Line (loc, opt_file, n) ->
        (* printing a line directive is not strictly needed *)
        (match opt_file with
             None ->
               maybe_print_location g (fst loc);
               bprintf g.buf "\n# %i\n" n
           | Some file ->
               bprintf g.buf "\n# %i %S\n" n file
        );
        (* printing the location next time is needed because it just changed *)
        g.require_location := true;
        env0

    | `Current_line loc ->
        maybe_print_location g (fst loc);
        g.require_location := true;
        let pos, _ = g.call_loc in
        bprintf g.buf " %i " pos.Lexing.pos_lnum;
        env0

    | `Current_file loc ->
        maybe_print_location g (fst loc);
        g.require_location := true;
        let pos, _ = g.call_loc in
        bprintf g.buf " %S " pos.Lexing.pos_fname;
        env0




let include_inputs
    ~extensions
    ~preserve_quotations
    ~incdirs
    ~show_exact_locations
    ~show_no_locations
    buf env l =

  let enable_loc = not show_no_locations in
  List.fold_left (
    fun env (dir, file, open_, close) ->
      let l = parse ~preserve_quotations file (open_ ()) in
      close ();
      let g = {
        call_loc = dummy_loc;
        buf = buf;
        included = S.empty;
        require_location = ref true;
        show_exact_locations = show_exact_locations;
        enable_loc = ref enable_loc;
        g_preserve_quotations = preserve_quotations;
        incdirs = incdirs;
        current_directory = dir;
        extensions = extensions;
      }
      in
      expand_list ~top:true { g with included = S.add file g.included } env l
  ) env l
