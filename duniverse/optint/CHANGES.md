### v0.3.0 2022-12-16 Paris (France)

- Add infix operators for bitwise operations (@reynir, #23)
- Add a deprecation about old infix operators
  They will be removed at the next minor release

### v0.2.0 2022-04-08 Paris (France)

- Fix the README.md (@sidkshatriya, #19)
- Fix fuzzers (@dinosaure, #20)
- Add a proof to introspect the type of `Optint.t` (@dinosaure, #21)

### v0.1.0 2021-03-30 Paris (France)

- Annotate integer types with `[@@immediate64]` (@CraigFe, #13)
- Move unwrapped module `Int63` to `Optint.Int63` (@CraigFe, #13)

### v0.0.5 2021-02-22 Paris (France)

- Update the README.md (@CraigFe, #9)
- Add a representation of 63-bit integers (@CraigFe, #9)
- Allow to compile fuzzers on 32-bit architectures (@dinosaure, #9)
- Add encode / decode functions for integers (@CraigFe, #9)
- Fix `optint` about sign and cast on all architectures (@dinosaure, #9)
- **breaking changes**, rename and handle properly sign-bit:
  `{of,to}_int` become `{of,to}_unsigned_int`
  `{of,to}_int32` become `{of,to}_unsigned_int32`
  Previous functions handle sign-bit correctly

### v0.0.4 2020-03-09 Paris (France)

- Fix 32bit backend where we miss to fully apply
  an `invalid_arg`
- Fix 64bit backend where `Native.unsigned_compare`
  and `Nativeint.unsigned_div` exists (OCaml 4.08.0)

### v0.0.3 2010-09-12 Paris (France)

- Avoid partial application of function (#2, @dinosaure)
- Add `[@immediate]` tag (#4, @dinosaure)
- Fix `select.ml` in 32bit (#5, @IndiscriminateCoding)
- Fix typo (#6, @hannesm)
- Add fuzzer (#8, @dinosaure)
- Fix `lsr` and `asr` in 64bit (#8, @cfcs, @dinosaure)
- Optimization on `of_int` function (64bit) (#8, @cfcs, @dinosaure)
- Optimization on `abs` function (64bit) (#8, @cfcs, @dinosaure)
- Fix 32bit architecture, keep bit-sign in the same place (#8, @dinosaure, review @cfcs)

### v0.0.2 2018-10-15 Paris (France)

- _Dunify_ project
- Fix dependencies on `dune` file when we select impl. (@rgrinberg)

### v0.0.1 2018-06-28 Paris (France)

- First version of `optint`
