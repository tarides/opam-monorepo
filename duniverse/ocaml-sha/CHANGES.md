## v1.15.4

- Fix build on OCaml 4.02 and fix SHA equality bug by @djs55 (#61)

## v1.15.3

- Fix build on OpenBSD by @kit-ty-kate (#58)
- More unit tests by @c-cube (#57)

## v1.15.2

- Use modern Bigarray functions by @MisterDA (#55)

## v1.15

- Add `equal` function to compare digests by @MisterDA (#52)
- Improve documentation formatting by @MisterDA (#51)
- Add `of_hex` and `of_bin` functions to unserialise `ShaX.t` values by @MisterDA (#50)

## v1.14

- Handle `safe-string` by @olafhering, reviewed by @nojb (#47)
- Remove `--dev` option from `dune` invocation by @arthurteisseire (#48)

## v1.13

- Update to dune2, fix Windows compilation, make C fns static @nojb (#42)
- Fix opam lint @djs55 (#44)
- Support platforms without `O_CLOEXEC` @dougmenchen (#41 then #45)

## v1.12

- Build with jbuilder (#38 by @nojb)
- Fix a possible GC crash (#36 by @madroach)
- Fix build with MSVC toolchain (#37 by @nojb)
- Build C with `-fPIC` (#25 by @talex5)

## v1.11

- Relicense to ISC (#22)
- Fix Windows support and test with appveyor (#28)
- Fix FreeBSD support (#12)

## v1.10

- Fix build with OCaml 4.06 (and `-safe-string`)
