(** For now this command is only used for testing,
    in particular it is not installed to the user.
    (If we wanted to install it, it would need
    a better name.)
*)

let usage =
  "Simplified patch utility for single-file patches;\n
   ./patch.exe -p<num> <input-file> <unified-diff-file> -o <output-file>"

let exit_command_line_error = 1
let exit_open_error = 2
let exit_several_chunks = 3
let exit_patch_failure = 4

let run ~p ~input ~diff =
  match Patch.parse ~p diff with
  | [] -> input
  | _::_::_ ->
    prerr_endline "Error: The diff contains several chunks,\n\
                   which is not supported by this command.";
    exit exit_several_chunks
  | [diff] ->
    begin match Patch.patch ~cleanly:true (Some input) diff with
    | None ->
      Printf.eprintf "Error during patching:\n%!";
      exit exit_patch_failure
    | Some output -> output
    end

module IO = struct
  let read input =
    let rec loop buf input =
      match input_char input with
      | exception End_of_file -> Buffer.contents buf
      | c -> Buffer.add_char buf c; loop buf input
    in
    loop (Buffer.create 80) input

  let write output data =
    String.iter (output_char output) data;
    flush output;
    ()
end

let () =
  if Array.length Sys.argv = 1 then begin
    prerr_endline usage;
    exit 0;
  end;
  let p, input_path, diff_path, output_path = try
      let p =
        let arg = Sys.argv.(1) in
        String.sub arg 2 (String.length arg - 2) |> int_of_string
      in
      let input_path = Sys.argv.(2) in
      let diff_path = Sys.argv.(3) in
      let dash_o = Sys.argv.(4) in
      let output_path = Sys.argv.(5) in
      if dash_o <> "-o" then raise Exit;
      p,
      input_path,
      diff_path,
      output_path
    with _ ->
      prerr_endline "Error parsing the command-line arguments";
      prerr_endline usage;
      prerr_newline ();
      exit exit_command_line_error
  in
  let get_data path =
    match open_in path with
    | exception _ ->
      Printf.eprintf "Error: unable to open file %S for reading\n%!" path;
      exit exit_open_error
    | input ->
      let data = IO.read input in
      close_in input;
      data
  in
  let write_data path ~data =
    match open_out path with
    | exception _ ->
      Printf.eprintf "Error: unable to open file %S for writing\n%!" path;
      exit exit_open_error
    | output ->
      IO.write output data;
      close_out output
  in
  let input_data = get_data input_path in
  let diff_data = get_data diff_path in
  let output_data = run ~p ~input:input_data ~diff:diff_data in
  write_data output_path ~data:output_data
