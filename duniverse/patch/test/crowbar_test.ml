(** USAGE:

    This test works by generating two files (source and target),
    diffing them using the system `diff -u` command,
    applying our Patch code to the source file,
    and checking that we get the target back.

    Counter-examples will give you the source and target file.

    From the root repository, run

       dune exec test/crowbar_test.exe

    to get quicheck-like (blackbox) fuzzing, and

       mkdir -p /tmp/input
       mkdir -p /tmp/output
       echo foo > /tmp/input/test
       dune build test/crowbar_test.exe
       afl-fuzz -i /tmp/input -o /tmp/output dune exec test/crowbar_test.exe @@

    for AFL-full (greybox) fuzzing.

    If you find a counter-example, you can use src/patch_command
    to reproduce the issue. For example, if the quickcheck mode tells you:

    > patch: ....
    > patch: FAIL
    >
    > When given the input:
    >
    >     ["\nx"; "\n"]
    >
    > the test failed:
    >
    >     "" != "\n"
    >

    You can run

      echo -n -e "\nx" > file1
      echo -n -e "\n" > file2
      diff -u file1 file2 > diff
      patch file1 diff -o file2-std-patch
      dune exec src/patch_command.exe -- file1 diff -o file2-our-patch
      diff file2-our-patch file2-std-patch
      rm file1 file2 diff file2-std-patch file2-our-patch

    to check for yourself.
*)


type line = string
type file = line list

let string_of_file = String.concat ""

module Printer = struct
  open Crowbar
  let line : line printer =
    fun ppf line -> pp ppf "%S" line
  let file : file printer =
    fun ppf file -> pp ppf "%S" (String.concat "" file)
end

module Gen = struct
  open Crowbar
  let char : string gen =
    map [range 25] (fun n -> String.make 1 (char_of_int (int_of_char 'a' + n)))
  let line : line gen =
    with_printer Printer.line @@
    map [list char] (fun s -> String.concat "" (s @ ["\n"]))
  let line_no_eol : line gen =
    with_printer Printer.line @@
    map [list char] (fun s -> String.concat "" s)
  let file : file gen =
    with_printer Printer.file @@
    choose [
      list line;
      map [list line; line_no_eol] (fun lines line -> lines @ [line]);
    ]
end

module IO = struct
  let read input =
    let rec loop buf acc input =
      match input_char input with
      | exception End_of_file ->
        if Buffer.length buf = 0 then List.rev acc
        else List.rev (Buffer.contents buf :: acc)
      | '\n' ->
        Buffer.add_char buf '\n';
        let line = Buffer.contents buf in
        Buffer.clear buf;
        loop buf (line :: acc) input
      | c ->
        Buffer.add_char buf c;
        loop buf acc input
    in
    loop (Buffer.create 80) [] input

  let write output file =
    List.iter (output_string output) file;
    ()

  let with_file_out file k =
    let (path, oc) = Filename.open_temp_file "patch_crowbar" "" in
    let clean () =
      close_out oc;
      Sys.remove path in
    write oc file;
    flush oc;
    match k path with
    | exception exn -> clean (); raise exn
    | res -> clean (); res

  let with_tmp k =
    let path = Filename.temp_file "patch_crowbar_diff" "" in
    let clean () = Sys.remove path in
    match k path with
    | exception exn -> clean (); raise exn
    | res -> clean (); res
end

(** getting a system *diff* from two files *)
let get_diffs (file1 : file) (file2 : file) : file =
  IO.with_file_out file1 @@ fun path1 ->
  IO.with_file_out file2 @@ fun path2 ->
  IO.with_tmp @@ fun path_out ->
  Printf.ksprintf (fun cmd -> ignore (Sys.command cmd))
    "diff -u %S %S > %S" path1 path2 path_out;
  let input = open_in path_out in
  let res = IO.read input in
  close_in input;
  res

let check_Patch file1 file2 =
  let text_diff = string_of_file (get_diffs file1 file2) in
  match Patch.parse ~p:0 text_diff with
  | [] -> Crowbar.check_eq (string_of_file file1) (string_of_file file2)
  | _::_::_ -> Crowbar.fail "not a single diff!"
  | [diff] ->
    let data = string_of_file file1 in
    match Patch.patch ~cleanly:true (Some data) diff with
    | None ->
      let exp = string_of_file file2 in
      Crowbar.fail ("input file\n" ^ data ^ "\ndiff\n" ^ text_diff ^ "\nexpected\n" ^ exp)
    | Some output ->
      let exp = string_of_file file2 in
      if String.equal exp output then
        Crowbar.check_eq
          ~pp:Crowbar.pp_string
          output exp
      else
        Crowbar.fail ("input fileFFF\n" ^ data ^ "FFF\ndiffFFF\n" ^ text_diff ^ "FFF\nexpectedFFF\n" ^ exp ^ "FFF")


let () =
  Crowbar.(add_test ~name:"patch" [Gen.file; Gen.file] check_Patch)
