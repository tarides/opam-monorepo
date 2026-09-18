Checkseum
=========

Chekseum is a library which implements ADLER-32 and CRC32C Cyclic Redundancy
Check. It provides 2 implementation, the first in C and the second in OCaml.
The library is on top of [`optint`][optint] to get the best representation of
the CRC in the OCaml world.

### Linking trick / variant

Then, as [`digestif`][digestif], `checkseum` uses the linking trick. So if you
want to use `checkseum` in a library, you can link with the `checkseum` package
which **does not** provide an implementation. Then, end-user can choose between
the C implementation or the OCaml implementation (both work on Mirage).

So, in `utop`, to be able to fully use `checkseum`, you need to write:
```sh
$ utop -require checkseum.c
```
or
```sh
$ utop -require checkseum.ocaml
```

In a `dune` workspace, the build-system is able to choose silently default
implementation (`checkseum.c`) for your executable if you don't specify one of
them. A _dune_-library is not able to choose an implementatio but still able to
use the _virtual_ library `checkseum`.

The trick permits us to compile `checkseum` with `js_of_ocaml`.

### Compatibility with 32-bits architecture

`checkseum` was made to provide a Adler-32 or a CRC-32 implementation
regardless the architecture - it takes the advantage of [optint][optint] to get
in any situation a 32-bits integer. However, it takes the advantage of the
_immediate_ `int` value for a 64-bits architecture (which let us to use, at
most, 63 bits).

The C implementation provides the cheapest FFI between OCaml and C _via_
annotations such as `[@untagged]` or `[@unboxed]`.

### Example

This is a simple example of how to use `checkseum` with CRC-32:
```sh
$ cat >digest.ml <<EOF
let digest ic =
  let tmp = Bytes.create 0x1000 in
  let rec go crc32 = match input ic tmp 0 0x1000 with
    | 0 -> crc32
    | len ->
      Checkseum.Crc32.digest_bytes tmp 0 len crc32 |> go
    | exception End_of_file -> crc32 in
  go Checkseum.Crc32.default

let () = match Sys.argv with
  | [| _; filename |] when Sys.file_exists filename ->
    let ic = open_in filename in
    let crc32 = digest ic in
    close_in ic ; Format.printf "%a\n%!" Checkseum.Crc32.pp crc32
  | [| _ |] ->
    let crc32 = digest stdin in
    Format.printf "%a\n%!" Checkseum.Crc32.pp crc32
  | _ -> Format.eprintf "%s [<filename>]\n%!" Sys.argv.(0)
EOF
$ cat >dune <<EOF
(executable
 (name digest)
 (libraries checkseum))
EOF
$ dune exec ./digest.exe -- digest.ml
3995955175
```

### Benchmark

With the `dune`'s profile `benchmark` (which requires [bechamel][bechamel]),
the user is able to produce a `bench/main.html` which compares `checkseum.c`
with `tcpip`.

[optint]: https://github.com/mirage/optint
[digestif]: https://github.com/mirage/digestif
[bechamel]: https://github.com/mirage/bechamel
