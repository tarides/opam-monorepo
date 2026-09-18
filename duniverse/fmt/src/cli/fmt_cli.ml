(*---------------------------------------------------------------------------
   Copyright (c) 2015 The fmt programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let strf = Format.asprintf

open Cmdliner

let style_renderer ?env ?docs () =
  let enum = ["auto", None; "always", Some `Ansi_tty; "never", Some `None] in
  let color = Arg.enum enum in
  let enum_alts = Arg.doc_alts_enum enum in
  let doc = strf "Colorize the output. $(docv) must be %s." enum_alts in
  Arg.(value & opt color None & info ["color"] ?env ~doc ~docv:"WHEN" ?docs)
