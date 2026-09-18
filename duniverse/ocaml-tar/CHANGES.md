## v3.5.0 (2026-05-22)

- Fix path traversal issue in unix, lwt-unix, eio
  reported by Quentin Stiévenart, fixed by @reynir, eio by @avsm
  51ceb0a15982993503c169b9d84456fd50eabe99 CVE-2026-45390 OSEC-2026-08
- Fix integer overflows originating from Int64.to_int (on 32 bit platforms)
  faf7bdf43356a81a0cf8f786e1c199c55512acf7

## v3.4.0 (2026-05-22)

- Improvements to `tar-eio` with an writing support and a refreshed Eio
  interface (@patricoferris @avsm, #176).

## v3.3.0 (2025-03-06)

- Deprecate `Tar.( let* )` in favor of the new `Tar.Syntax` module (@kit-ty-kate, #167)
- tar-mirage: remove mirage-clock dependency in favor of mirage-ptime (@hannesm, #168)

## v3.2.0 (2025-01-20)

- Fix the Tar monad with Tar_gz and allow the `read` operation (@dinosaure, @BChabanne, @reynir, #161)
- Add `x-maintenance-intent` field into opam files (@hannesm, #162)
- Defaults level per entries to the global level (@reynir, #157)
- Update `Tar_eio` with the last version of `tar` (@patricoferris, #159)

## v3.1.2 (2024-09-20)

- Fix a wrong assumption in `Tar_lwt_unix.run` for `Tar.Really_read _` that one
  tar block was always read. This meant that using `Tar.Really_read` with a
  size different from 512 would fail. (Reported by @jonahbeckford, review by @hannesm, @reynir, #153)
- Document better the actual behavior of `Tar_unix.extract` and
  `Tar_lwt_unix.extract` (@reynir, #155)

## v3.1.1 (2024-09-13)

- Expose `Tar_lwt_unix.run` as we do with `Tar_unix.run` and `Tar_eio.run`.
  This was an oversight in v3.0.0. (Reported by @jonahbeckford, @reynir, #150)

## v3.0.0 (2024-08-05)

- Fix `Header.marshal` and the checksum and the length (@reynir, #145)
- Delete a mutable field about the level into the header (@hannesm, #141)
- **BREAKING**: de-functorize the package (@hannesm, @reynir, @dinosaure, #140, #143, #146)

  These PRs attempt to de-functorize `Tar` so that users can implement I/O
  themselves, using `Tar`'s own element serialization/deserialization functions
  to take advantage of read/write methods. This avoids imposing on the user the
  implementation of a module that is too rigid in his/her case (which could have
  performance implications).

  `Tar` offers functions for serializing/deserializing tar-specific elements
  from `string`. It is then up to the user to know how to obtain or write these
  `strings`.

  To this, these PRs add "logics" (see `'a Tar.t`) requiring read and/or write
  implementations and describing how to extract all entries from a tar file or
  how to write a tar file according to a "dispenser" (like `Seq.to_dispenser`)
  of entries.

  These logics do not depend on a particular "scheduler", and these PRs propose
  a derivation of these logics with `tar-unix`, `tar-eio` and `tar-mirage`.
  These latter derivations mean that the API for these packages has only been
  extended, and there are no breaking changes as such.

  These logics also make it easy to offer a compression/decompression layer with
  `decompress`, so you can easily manipulate and/or create a .tar.gz file.

## v2.6.0 (2023-09-07)

- Add eio backend for tar in tar-eio (@patricoferris, review by @talex5, @reynir, #132)
- Also apply backwards compatibility fix when GNU LongName is used. The compatibility fix is unfortunately also applied for unknown-to-ocaml-tar link indicators (reported by @gravicappa in #129, @reynir, #133)
- `tar`: support pax Global Extended Headers. This adds state to tar parsing.
  (#119, #120, @MisterDA)
- Support GNU LongLink and LongName. Prior, `Tar.HeaderWriter` and
  `Tar.HeaderReader` supported both, but `Tar.Header.Link` only had `LongLink`
  and (de)serialized to (from) GNU LongName (#127)
- Compatibility level when reading / parsing is removed. Only GNU
  LongLink/LongName extensions were affected by the compatibility level when
  reading (#127)
- Add module types `Tar.HEADERREADER` and `Tar.HEADERWRITER` describing the
  output of `Tar.HeaderReader` and `Tar.HeaderWriter` respectively (#127)
- Types `Tar.READER.t` and `Tar.WRITER.t` are renamed to `io` (#127)
- Add `write_global` function for writing a global PAX extended header (#127)
- Rework IO-specific modules (tar-unix etc.) harmonizing them (#127)
- Avoid exceptions in tar and use result instead. The exceptions
  `End_of_stream` and `Checksum_mismatch` are removed (#127)
- Remove the `Tar_cstruct` module as it was unused (#127)
- Remove debug printers (#127)
- Finally remove the unused camlp-streams dependency (#127)

## v2.5.1 (2023-06-20)

- Treat headers with link indicator '0' or '\000' (`Normal`) as directories for backward compatibility (reported in #129, fix by @reynir)

## v2.5.0 (2023-06-06)

- File names and link names are used from PAX headers when parsing (reported by @gravicappa, fixed in #128 by @reynir)

## v2.4.0 (2023-03-30)

- Switch to alcotest for tests (@MisterDA, review by @reynir, #121)
- **BREAKING**: fix ustar magic version. Previously, the version "0\000" was
  serialized instead of the correct version "00". This means tar archives may
  not be reproducable with older versions. (@reynir, @hannesm, #117 and #122)
- Remove `ppx_cstruct`dependency (@hannesm, review by @reynir, #117)
- Properly skip Pax GlobalExtendedHeaders (@MisterDA, @reynir, #116 and #118)

## v2.3.0 (2023-02-09)

- `tar-mirage`: implement mirage-kv.6.0.0 (@reynir, @hannesm)

## v2.2.2 (2022-12-12)

- `tar-mirage`: fix writing of data when data+tar header is a multiple of `sector_size` greater than 1 (@reynir, review by @hannesm, #100)

## v2.2.1 (2022-10-28)

- `tar-mirage`: fix writing of data, previously the end_of_archive was set 512 bytes short (#99 @hannesm @reynir)

## v2.2.0 (2022-10-19)

- `tar-mirage` requires and implements `mirage-kv.5.0.0` (@hannesm, #96)
- `tar-mirage` implements `Mirage_kv.RW` (append-only) (@hannesm, @reynir, @dinosaure, review by @MisterDA, #93)
- Update usage of `cstruct` in `tar`: unnecessary memsets removed, use `Cstruct.of_string` (@hannesm, #93)
- Fix `tar-mirage` read buffer allocation error (@reynir, review by @hannesm, #94)
- `tar` and `tar-mirage` do not require `re` anymore, `tar-mirage` doesn't depend on `iopage` and works with solo5 and other improvements (@hannesm, review by @reynir, #90)

## v2.1.0 (2022-08-31)

- `tar-mirage` requires `mirage-block.2.0.0` (@kit-ty-kate, #86)
- Remove `io-page-unix` dependency (@hannesm, #87)
- Add GZip support (@dinosaure, #88)

## v2.0.1 (2022-03-09)

- Set `O_CLOEXEC` on opened files and be sure to close opened files
  (@MisterDA, @talex5, #83)
- OCaml 5.00 support (add a new dependecy `camlp-stream`) (@Sudha247, #84)
- Missing padding in LongLing 'L' case (@dra27, #82)

## v2.0.0 (2021-09-23)

- Bump lower-bound on Cstruct to 6.0.0 (@MisterDA, @djs55, @dinosaure, #74)
- Update to Dune 2.9 and generate opam files (@MisterDA, @djs55, @dinosaure, #74)
- Support only OCaml versions 4.08 and higher. (@MisterDA, @dinosaure, #77)
- Don't print any logging to stdout or stderr (@MisterDA, @djs55, @dinosaure, #74)
- Remove `Tar.Make.Header`, `Tar_cstruct.Header`, `Tar_unix.Header`, and
  `Tar_lwt_unix.Header` to keep only Tar.Header and use it everywhere.
  - `Tar.Make.Header.get_next_header` becomes `Tar.Make.get_next_header`;
  - `Tar_cstruct.Header.get_next_header` becomes `Tar_cstruct.get_next_header`;
  - `Tar_lwt_unix.Header.get_next_header` becomes `Tar_lwt_unix.get_next_header`;
  - `Tar_lwt_unix.Header.of_file` becomes `Tar_lwt_unix.header_of_file`;
  - `Tar_unix.Header.get_next_header` becomes `Tar_unix.get_next_header`;
  - `Tar_unix.Header.of_file` becomes `Tar_unix.header_of_file`;
  - All the `Tar_*.Header.t` values have to be changed to `Tar.Header.t`.
  (@MisterDA, @dinosaure, #77)
- Fix parsing of pax Extended Header File Times with sub-second
  granularity. (@MisterDA, @dinosaure, #77)
- Add `Tar_unix.transform` and `Tar_lwt_unix.transform` to help
  transforming headers of a streamed tar archive between two file
  descriptors. (@MisterDA, @dinosaure, #77)
- Remove `{build}` tag on the `dune` dependency (@CraigFe, @hannesm, #72)
- Adapt `ocaml-tar` to newer MirageOS interfaces (@hannesm, @dinosaure, #73)
- Update gnu.org link (@reynir, @dinosaure, #79)
- `file_mode` defaults to `0o400` (@reynir, @MisterDA, @dinosaure, #78)

## v1.1.0 (2019-04-08)

- Do not depend on mirage-types, use mirage-kv instead (@hannesm)
- Support mirage-kv 2.0.0 (@hannesm)
- Do not suppress "unused value" warning (@emillon)
- Represent link indicator as a char. This transforms comments into actual code
  (@emillon)

## v1.0.1 (2019-02-04)
- fix tar-unix build with modern cstruct.lwt (@avsm)

## v1.0.0 (2019-02-03)
- port build to dune from builder (@avsm)
- upgrade opam metadata to 2.0 (@avsm)
- remove topkg in favour of dune-release (@avsm)
- use modern `ppx_cstruct` instead of `cstruct.ppx` (#65 @avsm @djs55)
- test with OCaml 4.07 as well (@avsm)

## v0.9.0 (2017-11-25)

- preliminary support for Cstruct.t-backed tar processing (#54 by @hcarty)
- fix build with OCaml 4.06.0 (and `-safe-string`)

## v0.8.0 (2017-05-09)

- split into 3 packages: `tar`, `tar-unix`, `tar-mirage`
- use jbuilder for building
- add support for reading @LongLink headers
- mark deprecated functions with @@ocaml.deprecated
- fix some warnings

## v0.7.1 (2017-02-03)

- convert build system to topkg (#43, @hannesm)

## v0.7.0 (2017-01-19)

- Build against MirageOS version 3, and drop support for earlier versions.
- Support only OCaml versions 4.03 and higher.

## v0.6.1 (2016-09-30)

- fix a bug in the key=value interface when the archive isn't a multiple
  of 4KiB in size

## v0.6.0 (2016-09-19)

- support for pax headers
- removed Tar.Archive.fold: please use HeaderReader instead

## v0.5.1 (2016-08-30)

- handle EINTR and short writes properly (@ivg)
- avoid a warning catching `Failure` exceptions from `int_of_string`

## v0.5.0 (2016-04-24)

- now requires cstruct >= 1.9.0 and OCaml 4.02+

## v0.4.2 (2016-04-22)

- test: only run tests if mirage-block-unix is present
- improve the opam file
- travis: simplify the configuration

## v0.4.1 (2015-07-21)

- fix Tar_mirage when using block devices with < 4096 byte
  sectors

## v0.4.0 (2015-07-19)

- add tar.mirage in ocamlfind, containing Tar_mirage which
  exposes a BLOCK device as a KV_RO

## v0.3.0 (2015-04-06)

- add Tar.Make functor which allows easier integration with
  camlzip
- always initialise tar header unused bytes to 0 (previously
  would use uninitialised data)
- modernise Travis CI scripts to use OPAM 1.2 workflow.

## v0.2.1 (2013-11-15)

- Re-add some old deprecated functions

## v0.2.0 (2013-10-13)

- Add 'Tar.Archive.fold' for folding over entries in an archive

## v0.1.1 (2013-10-03)

- Rename ocamlfind package from 'ocaml-tar' to simply 'tar'

## v0.1.0 (2013-10-03)

- Initial release
