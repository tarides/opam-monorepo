(* Copyright (c) 2018 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)
open Import

(* These packages are usually used only for legacy building. Since
   opam-monorepo requires everything to build with dune (which is special
   cased) we can ignore these packages if they're in the dependency formula *)
let skip_packages =
  [ "jbuilder"; "dune"; "ocamlbuild"; "ocamlmod"; "oasis"; "ocamlify" ]
  |> List.map ~f:OpamPackage.Name.of_string
  |> OpamPackage.Name.Set.of_list

(* The "ocaml" package is used in various other places so it is kept apart
   in the list of package names *)
let compiler_package_name = OpamPackage.Name.of_string "ocaml"

let compiler_package_names =
  [ compiler_package_name; OpamPackage.Name.of_string "ocaml-compiler" ]
  |> OpamPackage.Name.Set.of_list

let duniverse_opam_repo =
  "git+https://github.com/dune-universe/opam-overlays.git"

let vendor_dir = Fpath.v "duniverse"
let duniverse_log = Fpath.v ".duniverse-log"
let lockfile_ext = ".opam.locked"

(* variable to use for vendoring *)
let vendor_variable = OpamVariable.of_string "vendor"
