### v1.6.0 2026-07-21 Paris (France)

- Add x-maintenance-intent (@hannesm, #163)
- Fix documentation (@reynir, #161)
- Apply ocamlformat.0.29.0 (@dinosaure, #165)
- Improve our benchmark tool and compare decompress with camlzip and
  bytesrw.zlib (@dinosaure, #167)
- Use warning names instead of numbers (@hannesm, #170)
- Choose a better kind of block when we compress and approximate the cost
  between a Fixed and a Dynamic block (@dinosaure, #166)
- Use a xxHash implementation for our hashtable when we deflate
  (@dinosaure, #171)
- Use a two-level lookup table (as it's done by zlib, see inftrees.c) to
  speed-up the inflation (@dinosaure, #173)

  **breaking change**: De.Lookup.t is updated with a new root field.
  Nobody should use this module.

### v1.5.3 2023-09-19 Paris (France)

- Delete `libdecompress.a` (@dinosaure, #152)

  This artifact is unused and complexify the distribution. We decided to delete
  it. If people want to use `libdecompress.a`, we can provide an other package
  which will implement the `ctypes` ceremony to safely produce a
  `libdecompress.a`.

- Lint dependencies (lower bounds) (@dinosaure, #153)
- Remove unsafe accesses into our `decompress.lzo` implementation
  (@dinosaure, #154)
- Improve and fix our `decompress.lzo` implementation
  (@dinosaure, #155, #158, #159)

### v1.5.2 2023-01-23 Essaouira (Maroc)

- Remove remaining `bigarray-compat` dependencies (@copy, #147)

### v1.5.1 2022-08-30 Paris (France)

- Fix the stream of gzip inflation. If the user wants to know how many bytes
  are available into the output buffer, he/she must be in the DEFLATE internal
  state. Otherwise, we raise an exception. However, such information is not
  available into the API so we decided to say that the full output buffer is
  free when we are into the GZip header state.

  It ensures a real full stream API. (@dinosaure, #144)

### v1.5.0 2022-08-28 Paris (France)

- Update with `ocamlformat.0.20.0` (@dinosaure, #133)
- Add `lzo` into the binary (@dinosaure, #140)
- Be able to deflate/inflate files (@dinosaure, #141)
- Implement the zero-compression into zlib/gzip (@dinosaure, #142)
  **breaking change**: The behavior of flat block changed. The user does not
  specify how many bytes he/she wants to give. He/She can just specify that
  he/she wants a `Flat` block. The `Flat` block will contains what the queue
  has and nothing more: if the queue has 4 elements, we will encode a `Flat`
  block with 4 elements, if the queue has more than 65535 elements, we will
  encore only 65535 elements into the `Flat` block.

  The user is not able to _stream_ a `Flat` block: we can not fill one `Flat`
  block with multiple `Await`. This behavior is specific to the `Flat` block,
  the rest (`Fixed` and `Dynamic` blocks) did not change.

  For higher level API, the `level = 0` informs the _deflator_ to copy as is
  input - no compression was done for `zlib` and `gzip` and we emit only `Flat`
  blocks. By default, the `level = 4` is given so you probably will not notice
  anything but be care that we shifted compression level and `0` becomes a
  level without compression.

### v1.4.3 2022-04-08 Paris (France)

- Replace deprecated function of `fmt` (@dinosaure, #135)
- Remove `bigarray-compat`, `stdlib-shims` and support only
  OCaml >= 4.08.0 (@hannesm, #138)
- Update to `cmdliner.1.1.0` (@hannesm, #138)

### v1.4.2 2021-08-02 Paris (France)

- Fix lower bounds of `cmdliner` (@kit-ty-kate, #130)
- Fix big-endian support (@dinosaure, @talex5, #131)

### v1.4.1 2021-05-11 Paris (France)

- Fix and of file and end of block _op-code_ (@dinosaure, #123)
  **breaking changes**
  Semantically, the module `De` has another behavior about the inflation.
  Previously, the _stream_ inflation was smart enough to recognize the end
  of the stream and the user did not need to really emit:
  `De.Inf.src decoder empty 0 0` to say the end of the stream. Now, such
  call is required to notice to `De.Inf` the end of the stream. By this
  way, we are able to terminate the inflation correctly and we still
  continue to raise an error for unterminated stream
  (see `tests/invalid_distance_code`).

  For `Zl`/`Gz` users, this update does not imply anything when these
  implementations take care about such detail. Only `De` users should update
  their code to really emit the end of the stream with
  `De.Inf.src decoder empty 0 0`. In the PR, the diff show how to upgrade such
  code.
- Upgrade `decompress` to `optint.0.1.0` (@samoht, @dinosaure, #124)
- Fix compilation of benchmarks (@dinosaure, #128)
- Fix out of bounds errors on the _non-stream_ implementation (@clecat,
  @ewanmellor, @dinosaure, #126, #127)
- Optimize `memcpy` used on the _non-stream_ implementation (@clecat,
  @dinosaure, #129)

### v1.4.0 2021-04-22 Paris (France)

- Add a well-know limitation about the encoding on the documentation, the
  output buffer must be upper than 2 bytes in any cases.
  (@dinosaure, #114)
- Improve the documentation
  (@dinosaure, @brendanlong, #115)
- **breaking changes**, the type of the window used to deflate according
  RFC 1951 was updated to `De.Lz77.window`
  (@dinosaure, #116 & #115)
- Fix a bug when we want to add the EOB _op-code_ into the queue. The deflation
  takes care about that. Note that the queue must be larger than 2.
  (@dinosaure, @kluvin, #117)
- Improve the documentation a bit
  (@mseri, @dinosaure, #119)
- Fix the use of `optint` and how we handle large files with `Gz`. Fix an error
  when we encode the `isize` into the deflated stream.
  (@dinosaure, @igarnier, #121 & #120)
- Add a _non-stream_ implementation (@clecat, @dinosaure, #102 & #92)

  The non-blocking stream API has a cost to maintain a _state_ across _syscall_
  such as `read` and `write`. It's useful when we want to plug `decompress`
  behind something like a `socket` and care about memory-consumption but it has
  a big cost when we want to compress/decompress an object saved into one and
  unique buffer.

  The _non-stream_ API gives an opportunity to inflate/deflate one and unique
  buffer without the usual plumbing required by the non-blocking stream API.
  However, we are limited to compute only objects which can fit into a
  `bigarray`.

  About performance, the non-stream API is better than the non-blocking stream
  API. See the PR for more details about performances. On the
  `book2` (from the Calgary corpus) file:
  - `decompress` (stream):
     15 Mb/s (deflation), 76 Mb/s (inflation), ratio: 42.46 %
  - `decompress` (non-stream):
     17 Mb/s (deflation), 105 Mb/s (inflation), ratio: 34.66 %

  Even if we checked the implementation with our tests (we ran `ocaml-git` and
  `irmin` with this path), the implementation is young and we probably miss
  some details/bugs. So we advise the user to compare, at least, the non-stream
  implementation with the non-blocking stream implementation if something is
  wrong.

### v1.3.0 2020-03-03 Paris (France)

- Add a little executable to benchmark inflation into the distribution
  (@dinosaure, #93)
- Add instructions for running benchmark (@gs0510, #94)
- Clarify the description (@XVilka, #96)
- Improve the benchmark and outputs (@dinosaure, @gs0510, #95)
- Avoid allocation of distance table (@Engil, @dinosaure, #97)
- Swapping from arithmetic to logical bitshifts on `d.hold` (@clecat, #99)
- Make the use of all `Higher.compress` arguments (@vect0r-vicall, #103)
- Apply ocamlformat.0.16.0 (@dinosaure, #105, #107)
- Improve Lz77 algorithms (@dinosaure, #108)
  **breaking changes** the deflation expects a new window: `De.Lz77.make_window`
  instead of `De.make_window` (which is twice larger to improve the compression
  algorithm)

  Depending on the level and your corpus, we did not observe performance
  regression on deflation (and #97 improves a lot performances). An higher level
  is slower (but the compression ratio is better). We advise, by default, to use
  the level 6.

  Note that the user is able to make its own compression algorithm according to
  his corpus. An example of such implementation is available on the new
  `decompress.lz` libraries which fills a queue and compress the input.

  **breaking changes** decompress expects a level between 0 and 9 (inclusive)
  (instead of 0 and 3).
- Add tests about level compression (@dinosaure, #109)
- Add level on GZip layer (@dinosaure, #110)
- Provide a `ctypes` reverse binding (@dinosaure, #98)
- Provide a binary `decompress.pipe` which can compress/uncompress with
  deflate, zlib or gzip format.

### v1.2.0 2020-07-07 Paris (France)

- add LZO support (@dinosaure, @cfcs, @XVilka, #82)
- update binaries (@dinosaure, @XVilka, #89)
- fix an exception leak (@dinosaure, @b1gtang, #88)
- update README.md (@dinosaure, @XVilka, #87)
- fix a mis-use of `Zl` API (@dinosaure, #85)
- add `dune` as a dependency of `rfc1951` (@kit-ty-kate)
- real non-blocking state with `Zl` (@dinosaure, #84)

### v1.1.0 2019-03-10 Paris (France)

- add GZip support (@dinosaure, @copy, @hcarty, #79)
- **breaking changes**, `Higher` returns a `result` value instead to raise an
  exception (@dinosaure, @copy, #80)
- protect Zlib decoder on multiple _no-op_ calls of `decode`
- test when we generate an empty zlib flow
- fix a bug on the DEFLATE layer when we must flush bits to avoid integer
  overflow
- really use the internal continuation of the Zlib state
- delete `fmt` depedency
- update fuzzer
- fix default level on `Zl.Higher`

### v1.0.0 2019-08-30 Paris (France)

** breaking changes **

`decompress.1.0.0` is 3 times faster about decompression than before. A huge
[amount of work was done](https://tarides.com/blog/2019-08-26-decompress-the-new-decompress-api.html)
to improve performance and coverage.

The main reason to update the API is to fix a bad design decision regarding
split compression and encoding. User is able to implement a new compression
algorithm and use it.

Release comes with regressions:
- `decompress` only supports `Bigarray` now, not `Bytes`
- GZIP layer does not exist anymore
- state of RFC1951 encoder/decoder is not referentially transparent anymore

Of course, v1.0.0 comes with fixes and improvements:
- `decompress` is able to compress/uncompress
  [Calgary corpus](https://en.wikipedia.org/wiki/Calgary_corpus)
- tests are improved and they include all coverage tests from `zlib`
- compression algorithm has a fuzzer
- encoder has a fuzzer
- performance about decoder is 3 times better than `decompress.v0.9.0` and 3
  times slower than `zlib`

`decompress` is split into 2 main modules:
- `dd` which implements RFC1951
- `zz` which implements ZLIB

API of them are pretty-close to what `decompress.v0.9.0` does with some
advantages on `dd`:
- User can use their own compression algorithm instead of `Dd.L`
- encoder exposes more granular control over what it emits (which block, when,
  where)
- Huffman tree generation is out of `dd`

As a response to #25, `dd` provides a _higher_ level API resembling `camlzip`.

### v0.9.0 2019-07-10 Paris (France)

* Add support of 4.07 and 4.08 in Travis (@XVilka, @dinosaure, #70, #71)
* Use `mmap` (@XVilka, @dinosaure, @hannesm, #68, #69, #71)
* Update documentation (@yurug, @dinosaure, #65, #66)
* Micro-optimization about specialization (@dinosaure, #64)
* Re-organize internals of `decompress` (@dinosaure, #63) 
* GZIP support (@clecat, review by @dinosaure, @cfcs, @hannesm, #60)
 - fix #58 (@dinosaure)

### v0.8.1 2018-10-16 Paris (France)

* _Dunify_ project (@dinosaure)
* *breaking-change* Unbox `Bytes.t` and `Bigstring.t` as I/O buffer (@dinosaure)
* Add foreign tests vectors (@cfcs, @dinosaure)
* Catch invalid distance (@XVilka, @dinosaure)
* Better check on dictionaries (@XVilka, @dinosaure)
* **breaking-change** Add [wbits] argument to check Window size on RFC1951
  (@XVilka, @dinosaure)

### v0.8 2018-07-09 Paris (France)

* Implementation of RFC1951 (task from @cfcs)
* *breaking change* New interface of decompress

  We wrap API in `Zlib_{inflate/deflate}` and add `RFC1951_{inflate/deflate}`.
  
* Move to `jbuilder`/`dune` (task from @samoht)
* Better check on `zlib` header
* Fixed infinite loop (task fron @cfcs)

  See 2e3af68, `decompress` has an infinite loop when the inflated dictionary
  does not provide any bindings (and length of opcode is <= 0). In this case,
  `decompress` expects an empty input and provide an empty output in any case.
  
* Use re.1.7.2 on tests
* Use camlzip.1.07 on tests

### v0.7 2017-10-18 Paris (France)

* Fixed Inflate.write function
* Fixed internal state to stick in a internal final state
* Fixed compilation with js_of_ocaml (use trampoline function to avoid
  stack-overflow)
* Fixed clash of name about state variable in the Inflate module
* Add afl program
* Export Adler-32 modules
* Add -i and -o option in the dpipe binary to inform the size of the
  internal chunk
* Change the value of -mode in the dpipe binary

### v0.6 2017-05-11 Cao Lãnh (Vietnam)

- Fixed bug #29
- Produce far pattern (Lz77 compression)
- Optimize memory consumption of the Inflate module
- Move repository from oklm-wsh to mirage
- Learn topkg release

### v0.5 2017-02-17 Essaouira (Maroc)

- Stabilize the interface (@dbuenzli's interface)
- Merge optimization from @yallop
- Add `sync_flush`, `partial_flush`, `full_flush` (experimental)
- Move the build system to `topkg`
