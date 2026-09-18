# Decompress - Pure OCaml implementation of decompression algorithms

`decompress` is a library which implements:
- [RFC1951](https://tools.ietf.org/html/rfc1951)
- [Zlib](https://zlib.net/)
- [Gzip](https://tools.ietf.org/html/rfc1952)
- [LZO](https://en.wikipedia.org/wiki/Lempel%E2%80%93Ziv%E2%80%93Oberhumer)

## The library

The library is available with:
```
$ opam install decompress
```

It provides three sub-packages:
- `decompress.de` to handle RFC1951 stream
- `decompress.zl` to handle Zlib stream
- `decompress.gz` to handle Gzip stream
- `decompress.lzo` to handle LZO contents

Each sub-package provide 3 sub-modules:
- `Inf` to inflate/decompress a stream
- `Def` to deflate/compress a stream
- `Higher` as a easy entry point to use the stream

## How to use it

### The binary

The distribution provides a simple binary which is able to compress/uncompress
anything:
```sh
$ decompress -fgzip --deflate < my_document.txt > my_document.gzip
$ decompress -fgzip < my_document.gzip > my_document.out
$ diff my_document.txt my_document.out
```

It does the GZip compression, the Zlib one and the DEFLATE one. It can do an
LZO compression too.

### Link issue

`decompress` uses [`checkseum`][checkseum] to compute CRC of streams.
`checkseum` provides 2 implementations:
- a C implementation to be fast
- an OCaml implementation to be usable with `js_of_ocaml` (or, at least,
  require only the _caml runtime_)

When the user wants to make an OCaml executable, it must choose which
implementation of `checkseum` he wants. A compilation of an executable with
`decompress.zl` is:
```
$ ocamlfind opt -linkpkg -package checkseum.c,decompress.zl main.ml
```

Otherwise, the end-user should have a linking error (see
[#47](https://github.com/mirage/decompress/issues/47)).

#### With `dune`

`checkseum` uses a mechanism integrated into `dune` which solves the link
issue. It provides a way to silently choose the default implementation of
`checkseum`: `checkseum.c`.

By this way (and only with `dune`), an executable with `decompress.zl` is:
```
(executable
 (name main)
 (libraries decompress.zl))
```

Of course, the user still is able to choose which implementation he wants:
```
(executable
 (name main)
 (libraries checkseum.ocaml decompress.zl))
```

### The API

`decompress` proposes to the user a full control of:
- the input/output loop
- the allocation

#### Input / Output

The process of the inflation/deflation is non-blocking and it does not require
any _syscalls_ (as an usual MirageOS project). The user can decide how to get
the input and how to store the output.

An usual _loop_ (which can fit into `lwt` or `async`) of `decompress.zl` is:
```ocaml
let rec go decoder = match Zl.Inf.decode decoder with
  | `Await decoder ->
    let len = input itmp 0 (Bstr.length tmp) in
    go (Zl.Inf.src decoder itmp 0 len)
  | `Flush decoder ->
    let len = Bstr.length otmp - Zl.Inf.dst_rem decoder in
    output stdout otmp 0 len ;
    go (Zl.Inf.flush decoder)
  | `Malformed err -> invalid_arg err
  | `End decoder ->
    let len = Bstr.length otmp - Zl.Inf.dst_rem decoder in
    output stdout otmp 0 len in
go decoder
```

#### Allocation

Then, the process does not allocate large objects but it requires at the
initialisation these objects. Such objects can be re-used by another
inflation/deflation process - of course, these processes can not use same
objects at the same time.

```ocaml
val decompress : window:De.window -> in_channel -> out_channel -> unit

let w0 = De.make_windows ~bits:15

(* Safe use of decompress *)
let () =
  decompress ~window:w0 stdin stdout ;
  decompress ~window:w0 (open_in "file.z") (open_out "file")

(* Unsafe use of decompress,
   the second process must use an other pre-allocated window. *)
let () =
  Lwt_main.run @@
    Lwt.join [ (decompress ~window:w0 stdin stdout |> Lwt.return)
             ; (decompress ~window:w0 (open_in "file.z") (open_out "file")
	       |> Lwt.return) ]
```

This ability can be used on:
- the input buffer given to the encoder/decoder with `src`
- the output buffer given to the encoder/decoder
- the window given to the encoder/decoder
- the shared-queue used by the compression algorithm and the encoder

### Example

An example exists into [bin/decompress.ml][decompress.ml] where you can see how
to use `decompress.zl` and `decompress.de`.

### Higher interface

However, `decompress` provides a _higher_ interface close to what `camlzip`
provides to help newcomers to use `decompress`:
```ocaml
val compress :
     refill:(bigstring -> int)
  -> flush:(bigstring -> int -> unit)
  -> unit
val uncompress :
     refill:(bigstring -> int)
  -> flush:(bigstring -> int -> unit)
  -> unit
```

### Benchmark

You can run a simple benchmark which compares `decompress` with `camlzip` and
`bytesrw.zlib` (both use `zlib`). These libraries must be installed and you
can launch the benchmark with:
```shell
$ git clone https://github.com/mirage/decompress
$ cd decompress
$ opam pin add -yn .
$ opam install --deps-only -t decompress
$ dune build @runbench
```

## Build Requirements

 * OCaml >= 4.07.0
 * `dune` to build the project
 * `base-bytes` meta-package
 * `checkseum`
 * `optint`

[checkseum]: https://github.com/mirage/checkseum
[decompress.ml]: ./bin/decompress.ml
