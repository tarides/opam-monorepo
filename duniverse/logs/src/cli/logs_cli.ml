(*---------------------------------------------------------------------------
   Copyright (c) 2015 The logs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

let strf = Format.asprintf

let level ?env ?docs () =
  let vopts =
    let doc = "Increase verbosity. Repeatable, but more than twice does
               not bring more."
    in
    Arg.(value & flag_all & info ["v"; "verbose"] ~doc ?docs)
  in
  let verbosity =
    let enum =
      [ "warning", None; (* Hack for the option's absent rendering *)
        "quiet", Some None;
        "error", Some (Some Logs.Error);
        "warning", Some (Some Logs.Warning);
        "info", Some (Some Logs.Info);
        "debug", Some (Some Logs.Debug); ]
    in
    let log_level = Arg.enum enum in
    let enum_alts = Arg.doc_alts_enum List.(tl enum) in
    let doc = strf "Be more or less verbose. $(docv) must be %s. Takes over
                    $(b,-v)." enum_alts
    in
    Arg.(value & opt log_level None &
         info ["verbosity"] ?env ~docv:"LEVEL" ~doc ?docs)
  in
  let quiet =
    let doc = "Be quiet. Takes over $(b,-v) and $(b,--verbosity)." in
    Arg.(value & flag & info ["q"; "quiet"] ~doc ?docs)
  in
  let choose quiet verbosity vopts =
    if quiet then None else match verbosity with
    | Some verbosity -> verbosity
    | None ->
        match List.length vopts with
        | 0 -> Some Logs.Warning
        | 1 -> Some Logs.Info
        | n -> Some Logs.Debug
  in
  Term.(const choose $ quiet $ verbosity $ vopts)
