(* SPDX-License-Identifier: MIT *)

let output_endline ch s =
  Stdlib.output_string ch (s^"\n")

let write file list =
  let list = List.stable_sort String.compare list in
  let ch = Stdlib.open_out file in
  Fun.protect ~finally:(fun () -> close_out ch) begin fun () ->
    output_endline ch "(* This file was generated. See dune file. *)";
    output_endline ch "";
    output_endline ch "let list = [";
    List.iter (fun x -> output_endline ch (Printf.sprintf "  %S;" x)) list;
    output_endline ch "]";
  end

let read file =
  let ch = Stdlib.open_in file in
  Fun.protect
    (fun () -> Ezjsonm.from_channel ch)
    ~finally:(fun () -> close_in ch)

let generate_licenses () =
  let list =
    let value = read "ids/licenses.json" in
    Ezjsonm.find value ["licenses"] |>
    Ezjsonm.get_list Fun.id |>
    List.filter_map begin fun license ->
      let is_deprecated =
        Ezjsonm.find license ["isDeprecatedLicenseId"] |>
        Ezjsonm.get_bool
      in
      if is_deprecated then
        None
      else
        Ezjsonm.find license ["licenseId"] |>
        Ezjsonm.get_string |>
        Option.some
    end
  in
  write "licenseIDs.ml" list

let generate_exceptions () =
  let list =
    let value = read "ids/exceptions.json" in
    Ezjsonm.find value ["exceptions"] |>
    Ezjsonm.get_list Fun.id |>
    List.filter_map begin fun license ->
      let is_deprecated =
        Ezjsonm.find license ["isDeprecatedLicenseId"] |>
        Ezjsonm.get_bool
      in
      if is_deprecated then
        None
      else
        Ezjsonm.find license ["licenseExceptionId"] |>
        Ezjsonm.get_string |>
        Option.some
    end
  in
  write "exceptionIDs.ml" list

let () = begin
  generate_licenses ();
  generate_exceptions ();
end
