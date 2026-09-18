open Core_bench
let memoize = Fix.Memoize.Int.memoize

(* -------------------------------------------------------------------------- *)

(* Random generation of abstract syntax trees. *)

module Generate = struct

  open AST

  let uneg e =
    EUnOp (UNeg, e)

  let ebinop op (e1, e2) =
    EBinOp (e1, op, e2)

  let pay s =
    assert (s > 0);
    s - 1

  let split s =
    assert (s >= 0);
    let s1 = Random.int (s + 1) in
    let s2 = s - s1 in
    s1, s2

  let rec expr (s : int) : expr =
    if s = 0 then
      EConst 0
    else
      let s = pay s in
      let i = Random.int 5 in
      if i = 4 then
        EUnOp (UNeg, expr s)
      else
        let s1, s2 = split s in
        let op = List.nth [BAdd; BSub; BMul; BDiv] i in
        EBinOp (expr s1, op, expr s2)

  let main (s : int) : main =
    expr s

end

(* -------------------------------------------------------------------------- *)

(* Each benchmark is run at the following tree sizes. *)

let args =
  [10; 100; 1_000; 10_000; 100_000; 1_000_000]

(* -------------------------------------------------------------------------- *)

(* A simple benchmark, for comparison: filling a hash table with [13s]
   entries, and performing a few dummy lookups. *)

let filling =
  let name = "Filling hash table x13" in
  Bench.Test.create_indexed ~name ~args @@ fun s ->
  Core.Staged.stage @@ fun () ->
  let table = Hashtbl.create 43 in
  for i = 0 to 13 * s do
    Hashtbl.add table i i;
    let j = i / 2 in
    ignore (Hashtbl.find table j)
  done

(* -------------------------------------------------------------------------- *)

(* Generating ASTs. *)

let generation =
  let name = "Generating AST" in
  Bench.Test.create_indexed ~name ~args @@ fun s ->
  Core.Staged.stage (fun () -> ignore (Generate.main s))

(* After [Generate.main] has been benchmarked, a memoized version of
   it can be used, so we spend less time preparing data for the next
   benchmarks. *)

let make_ast =
  memoize Generate.main

(* -------------------------------------------------------------------------- *)

(* Converting ASTs to DCSTs. *)

let conversion =
  let name = "Constructing DCST" in
  Bench.Test.create_indexed ~name ~args @@ fun s ->
  let ast = make_ast s in
  Core.Staged.stage (fun () -> ignore (AST2DCST.main ast))

let make_dcst =
  memoize @@ fun s ->
  make_ast s
  |> AST2DCST.main

(* -------------------------------------------------------------------------- *)

(* Resolving DCSTs to obtain CSTs. *)

let settle (m : Parser.DCST.main) : Parser.CST.main =
  Option.get (Parser.Settle.main m)

let resolution =
  let name = "Resolving DCST" in
  Bench.Test.create_indexed ~name ~args @@ fun s ->
  let dcst = make_dcst s in
  Core.Staged.stage (fun () -> ignore (settle dcst))

let make_cst =
  memoize @@ fun s ->
  make_dcst s
  |> settle

(* -------------------------------------------------------------------------- *)

(* Converting CSTs to strings. *)

let stringing =
  let name = "Stringifying CST" in
  Bench.Test.create_indexed ~name ~args @@ fun s ->
  let cst = make_cst s in
  Core.Staged.stage (fun () -> ignore (CST2String.main cst))

let make_string =
  memoize @@ fun s ->
  make_cst s
  |> CST2String.main

(* -------------------------------------------------------------------------- *)

(* Converting CSTs to PPrint documents. *)

let documenting =
  let name = "Documenting CST" in
  Bench.Test.create_indexed ~name ~args @@ fun s ->
  let cst = make_cst s in
  Core.Staged.stage (fun () -> ignore (CST2Document.main cst))

let make_doc =
  memoize @@ fun s ->
  make_cst s
  |> CST2Document.main

let format document : string =
  let b = Buffer.create 1024 in
  Document.ToBuffer.pretty 0.8 80 b document;
  Buffer.contents b

(* -------------------------------------------------------------------------- *)

(* Pretty-printing PPrint documents. *)

let formatting =
  let name = "Formatting document" in
  Bench.Test.create_indexed ~name ~args @@ fun s ->
  let document = make_doc s in
  Core.Staged.stage (fun () -> ignore (format document))

(* -------------------------------------------------------------------------- *)

(* Re-parsing a string. *)

let parse text =
  let lexbuf = Lexing.from_string text in
  Parser.main Lexer.token lexbuf

let parsing =
  let name = "Parsing" in
  Bench.Test.create_indexed ~name ~args @@ fun s ->
  let text = make_string s in
  Core.Staged.stage (fun () -> ignore (parse text))

let make_reparsed_ast =
  memoize @@ fun s ->
  make_string s
  |> parse

(* -------------------------------------------------------------------------- *)

(* Comparing the original AST and the re-parsed AST. *)

let comparing =
  let name = "Comparing" in
  Bench.Test.create_indexed ~name ~args @@ fun s ->
  let original_ast = make_ast s
  and reparsed_ast = make_reparsed_ast s in
  Core.Staged.stage (fun () -> assert (original_ast = reparsed_ast))

(* -------------------------------------------------------------------------- *)

(* Running the benchmarks. *)

let run_all_benchmarks () =
  Command_unix.run (Bench.make_command [
    filling;
    generation;
    conversion;
    resolution;
    stringing;
    documenting;
    formatting;
    parsing;
    comparing;
  ])

(* -------------------------------------------------------------------------- *)

(* Main. *)

let () =
  run_all_benchmarks()
