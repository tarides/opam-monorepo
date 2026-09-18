let level = Tar.Header.Ustar

let make_extended user_id =
  Tar.Header.Extended.make ~user_id ()

let make_file =
  let gen = ref 0 in
  fun () ->
  let name = "file" ^ string_of_int !gen in
  incr gen;
  let hdr = Tar.Header.make name 0L in
  hdr

let ( let* ) = Result.bind

(* Tests that global and per-file extended headers correctly override
   each other. *)
let use_global_extended_headers _test_ctxt =
  (* Write an archive using global and per-file pax extended headers *)
  begin try Sys.remove "test.tar" with _ -> () end;
  let cout = Unix.openfile "test.tar" [ Unix.O_CREAT ; Unix.O_WRONLY ] 0o644 in
  let g0 = make_extended 1000 in
  let g1 = make_extended 3000 in
  match
    Fun.protect ~finally:(fun () -> Unix.close cout)
      (fun () ->
         let* () = Tar_unix.write_global_extended_header ~level g0 cout in
         let hdr = make_file () in
         let* () = Tar_unix.write_header ~level hdr cout in
         let hdr = make_file () in
         let hdr = { hdr with Tar.Header.extended = Some (make_extended 2000) } in
         let* () = Tar_unix.write_header ~level hdr cout in
         let hdr = make_file () in
         let* () = Tar_unix.write_header ~level hdr cout in
         let hdr = make_file () in
         let* () = Tar_unix.write_global_extended_header ~level g1 cout in
         let* () = Tar_unix.write_header ~level hdr cout in
         Tar_unix.write_end cout)
  with
  | Error `Msg msg -> Alcotest.failf "failed to write something: %s" msg
  | Error `Unix (err, f, a) ->
    Alcotest.failf "failed to write: unix error %s %s %s" (Unix.error_message err) f a
  | Ok () ->
    (* Read the same archive, testing that headers have been squashed. *)
    let header =
      let pp ppf hdr = Fmt.pf ppf "%s" (Tar.Header.Extended.to_detailed_string hdr) in
      Alcotest.testable (fun ppf hdr -> Fmt.pf ppf "%a" Fmt.(option pp) hdr) ( = )
    in
    let f ?global hdr idx =
      let open Tar.Syntax in
      let* _pos = Tar.seek hdr.Tar.Header.file_size in
      match idx with
      | 0 ->
        Alcotest.check header "expected global header" (Some g0) global;
        Alcotest.(check int) "expected user" 1000 hdr.Tar.Header.user_id;
        Tar.return (Ok 1)
      | 1 ->
        Alcotest.check header "expected global header" (Some g0) global;
        Alcotest.(check int) "expected user" 2000 hdr.Tar.Header.user_id;
        Tar.return (Ok 2)
      | 2 ->
        Alcotest.check header "expected global header" (Some g0) global;
        Alcotest.(check int) "expected user" 1000 hdr.Tar.Header.user_id;
        Tar.return (Ok 3)
      | 3 ->
        Alcotest.check header "expected global header" (Some g1) global;
        Alcotest.(check int) "expected user" 3000 hdr.Tar.Header.user_id;
        Tar.return (Ok 4)
      | _ -> Alcotest.fail "too many headers"
    in
    match Tar_unix.fold f "test.tar" 0 with
    | Ok 4 -> ()
    | Ok n -> Alcotest.failf "early abort, expected 4, received %u" n
    | Error e -> Alcotest.failf "failed to read: %a" Tar_unix.pp_error e

let () =
  let suite = "tar - pax global extended headers", [
      Alcotest.test_case "can use pax global extended headers" `Quick use_global_extended_headers;
    ]
  in
  Alcotest.run "global extended headers" [suite]
