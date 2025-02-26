open Spdx_licenses

let fmt = Printf.sprintf

let pp = function
  | Ok x -> fmt "Ok (%s)" (to_string x)
  | Error `ParseError -> "Error `ParseError"
  | Error (`InvalidLicenseID id) -> fmt "Error (`InvalidLicenseID %s)" id
  | Error (`InvalidExceptionID id) -> fmt "Error (`InvalidExceptionID %s)" id

let test_bool name v =
  Alcotest.test_case name `Quick (fun () -> Alcotest.(check bool) name v true)

let test name v x =
  Alcotest.test_case name `Quick
    (fun () -> Alcotest.(check string) name (pp v) x)

let () =
  Alcotest.run "Tests" [
    "tests", [
      test_bool "valid_license_ids is reasonable"
        (List.mem "MIT" valid_license_ids);
      test_bool "valid_exception_ids is reasonable"
        (List.mem "OCaml-LGPL-linking-exception" valid_exception_ids);
      test "parse fails on invalid licenses"
        (parse "TEST")
        "Error (`InvalidLicenseID TEST)";
      test "parse fails on invalid exceptions"
        (parse "MIT WITH TEST")
        "Error (`InvalidExceptionID TEST)";
      test "parse has the right precedence rule (short)"
        (parse "LGPL-2.1-only OR BSD-3-Clause AND MIT")
        "Ok (LGPL-2.1-only OR (BSD-3-Clause AND MIT))";
      test "parse has the right precedence rule (long)"
        (parse "LGPL-2.1-only WITH OCaml-LGPL-linking-exception
                OR BSD-3-Clause AND MIT AND ISC AND BitTorrent-1.1 OR
                BSL-1.0 OR CC-BY-1.0 OR CC-BY-2.5 AND
                MPL-2.0 AND 0BSD WITH OCaml-LGPL-Linking-exception")
        "Ok (LGPL-2.1-only WITH OCaml-LGPL-linking-exception OR \
             (BSD-3-Clause AND MIT AND ISC AND BitTorrent-1.1) OR \
             BSL-1.0 OR CC-BY-1.0 OR \
             (CC-BY-2.5 AND MPL-2.0 AND 0BSD WITH OCaml-LGPL-linking-exception))";
      test "parse has the right precedence rule (long, reversed)"
        (parse "LGPL-2.1-only WITH OCaml-LGPL-linking-exception
                AND BSD-3-Clause OR MIT OR ISC OR BitTorrent-1.1 AND
                BSL-1.0 AND CC-BY-1.0 AND CC-BY-2.5 OR
                MPL-2.0 OR 0BSD WITH OCaml-LGPL-Linking-exception")
        "Ok ((LGPL-2.1-only WITH OCaml-LGPL-linking-exception AND BSD-3-Clause) OR \
             MIT OR ISC OR (BitTorrent-1.1 AND BSL-1.0 AND CC-BY-1.0 AND CC-BY-2.5) OR \
             MPL-2.0 OR 0BSD WITH OCaml-LGPL-linking-exception)";
      test "parse has the right case sensitivity"
        (parse "BsD-2-clAUsE")
        "Ok (BSD-2-Clause)";
    ]
  ]
