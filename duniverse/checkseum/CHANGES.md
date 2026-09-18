### v0.5.3 2026-04-19 Paris (France)

- Fix compilation of checkseum with clang 16.0.6 (@hannesm, #85)
- Add x-maintenance-intent (@hannesm, #86)

### v0.5.2 2023-09-18 Paris (France)

- Fix `adler32` for OCaml implementation (integer overflow) (@dinosaure, #83)

### v0.5.1 2023-04-11 Paris (France)

- Fix regression on `adler32` (the OCaml version) introduced by #76 and spotted
  by @balat (@dinosaure, #80). We advise the user to upgrade as soon as he/she
  can `checkseum`. `checkseum.0.5.0` will be disabled with this release.
- Fix the CI on Windows (@dinosaure, @MisterDA, #79)
  See (mirage/bechamel#41 for more details)

### v0.5.0 2023-02-23 Paris (France)

- Fixup deprecations from `optint.0.3.0` (@tmcgilchrist, #76)
- Fix C stubs on 32-bits machines (@dinosaure, #76)
- Delete the old linking trick about MirageOS 3 (@dinosaure, #77)

### v0.4.0 2022-09-30 Paris (France)

- Support LLVM/clang with `__STDDEF_H` (@jonahbeckford, #71)
- Fix docstring reference (@reynir, #72)
- Add `{of,to}_int32` (@reynir, #73, @dinosaure, #74)

### v0.3.4 2022-06-07 Arles (France)

- Add the support of Esperanto (@dinosaure, #69)
- Do not rely on the OCaml package to execute the `install.ml` script (@vapourismo, #67)

### v0.3.3 2022-04-08 Paris (France)

- Use canonical `_WIN32` macro to detect Windows (@jonahbeckford, #61)
- Fix Windows support with cygpath (@MisterDA, #60)
- Use `ocaml` variable to execute `install.ml` instead of a shebang with `env`
  (@vapourismo, @dinosaure, #62)
- Upgrade to `ocamlformat.0.21.0` (@hannesm, #63)
- Remove `bigarray-compat` package (@hannesm, #63)
- Support only OCaml >= 4.07 (@hannesm, #63)
- Optimize `checkseum.c` and use the advantage of `[@untagged]` and `[@unboxed]`
  `checkseum` requires `optint.0.2.0` now (@dinosaure, #64)
- Update the `README.md` (@dinosaure, #65)
- Use `caml_copy_int32` instead of `copy_int32` (@dinosaure, #66)

### v0.3.2 2021-02-08 Arles (France)

- `freenstanding` support does not need `opam` (@sternenseemann, @dinosaure, #53)
- Fix big-endian support (@dinosaure, #56)
- Use `which` when `command -v` is not available (@dinosaure, @sternenseemann, @mseri, #56)
- Fix `esy` support and on its environment the MirageOS 3 support (@dinosaure, @mseri, #56)
- Upgrade to `ocamlformat.0.19.0` (@dinosaure, #57)

### v0.3.1 2021-23-02 Paris (France)

- Upgrade `checkseum` to `optint.0.0.5` (@dinosaure, #51)

### v0.3.0 2020-11-03 Paris (France)

- Upgrade C artifacts with MirageOS 3.9 (#50, @dinosaure, @hannesm)
- Fix `esy` installation (#49, @dinosaure, @jordwalke, reported by @Faliszek)

### v0.2.1 2020-06-15 Paris (France)

- Move to dune.2.6.0 (#47)

### v0.2.0 2020-06-03 Paris (France)

- fix cross-compilation with `dune -x windows` (#45, @dinosaure, @pirbo)
- add CRC-24 (#43, @dinosaure, @cfcs)
- factorize C stubs (as digestif)
- avoid clash of names when we use `checkseum.c`
  Any functions are prefixed by `checkseum_`
- fix META file (#39 & #41, @hannesm, @dinosaure)
  A test was added to see if runes (static C libraries) are available for
  MirageOS targets (freestanding & xen)
- provide a binary `checkseum` to _digest_ standard input or file
  `checkseum.checkseum` is available to compute check-sum of standard input
  or file. The tool is used only for debugging.
- clean distribution (#38, @dinosaure)
  `checkseum` depends only on `bigarray-compat`, `base-bytes` & `optint`
- `limits.h` is available on any targets (#37, @dinosaure, @pirbo)

### v0.1.1 2019-09-12 Paris (France)

- Compatibility with mirage+dune (#29, @dinosaure)
- Use `bigarray-compat` (#29, @TheLortex)
- Add constraints with < mirage-runtime.4.0.0

  `checkseum` (as some others packages) must be used with MirageOS 4
  where `checkseum.0.9.0` is a compatibility package with Mirage)S 3

- Replace `STDC` macro check by `STDDEF_H_` to be able to compile (#34, @dinosaure)
  checkseum with +32bit compiler variant (#34, @dinosaure)
- Use a much more simpler implementation of CRC32C to be compatible with large set of targets (#34, @dinosaure)
- Avoid fancy operators in OCaml implementation of CRC32 and CRC32C (#34, @dinosaure)
- Require `optint.0.0.3` at least (#34, @dinosaure)

### v0.1.0 2019-05-16 Paris (France)

- Use experimental feature of variants with `dune` (#25, @dinosaure, review @rgrinberg)
  `checkseum` requires at least `dune.1.9.2`
- Add conflict with `< mirage-xen-posix.3.1.0` packages (#21, @hannesm)
- Provide `unsafe_*` functions (@dinosaure)
- Re-organize C implementation as `digestif` (@dinosaure)
- Remove `#include <stdio.h>` in C implementation (@dinosaure)
- Avoid partial application of functions, optimization (#19, @dinosaure)
- Add ocamlformat support (@dinosaure)
- _cross-compilation_ adjustement about MirageOS backends (#18, @hannesm)

### v0.0.3 2018-10-15 Paris (France)

- _Dunify_ project
- Add CRC32 implementation
- Fixed META file (@g2p)
- Update OPAM file

### v0.0.2 2018-08-23 Paris (France)

- Fix windows support (@nojb)

### v0.0.1 2018-07-06 Paris (France)

- First release of `checkseum`
