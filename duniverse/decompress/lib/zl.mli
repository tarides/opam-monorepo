(** {1 ZLIB layer.}

    ZLIB is a standard on top of RFC1951. It uses the {!De} implementation with
    the LZ77 compression algorithm. Module provides non-blocking streaming codec
    to {{:#decode}decode} and {{:#encode}encode} ZLIB encoding. It can
    efficiently work payload by payload without blocking IO. *)

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
(** The type for [bigstring]. *)

type window = De.window
(** The type for sliding windows. *)

val io_buffer_size : int

(** {2:decode ZLIB Decoder.}

    Unlike [de], [zl] provides a referentially transparent {!Inf.decoder}. The
    client must use a {!Inf.decoder} given {b by} {!Inf.decode} instead of a
    [decoder] given {b to} {!Inf.decode}. A common use of [zl] is:

    {[
      let rec go d0 = match Inf.decode d0 with
        | `Await d1 -> ... go d1
        | `Flush d1 -> ... go d1
        | _ -> ... in
    ]} *)

module Inf : sig
  type decoder
  (** The type for decoders. *)

  type src = [ `Channel of in_channel | `String of string | `Manual ]
  (** The type for input sources. With a [`Manual] source the client must
      provide input with {!src}. With [`String] or [`Channel] source the client
      can safely discard [`Await] case (with [assert false]). *)

  type signal =
    [ `Await of decoder
    | `Flush of decoder
    | `End of decoder
    | `Malformed of string ]

  val decoder : src -> o:bigstring -> allocate:(int -> window) -> decoder
  (** [decoder src ~o ~allocate] is a decoder that inputs from [src].

      {b Output buffer.}

      [zl], as [de], uses [o] buffer as internal buffer to store output. We
      recommend to allocate an {!io_buffer_size} buffer as output buffer. Then,
      {!dst_rem}[ decoder] tells you how many unused bytes remain in [o].

      {b Window.}

      ZLIB has a header to specify the window size needed to inflate a given
      input. When [zl] knows that, it calls [allocate] with a number [bits] so
      that [1 lsl bits] is the size of the window. [bits] can not be larger than
      15 nor lower than 8. [allocate] can be [fun bits -> De.make_window ~bits]
      or a previously allocated window. [decoder] will take the {i ownership} on
      it!

      Ownership in our case means that {!decode} will mutate it in-place and
      expect it to remain unchanged between invocations. *)

  val decode : decoder -> signal
  (** [decode d0] is:

      - [`Await d1] if [d0] has a [`Manual] input source and awaits for more
        input. The client must use a {!src} with [d1] to provide it.
      - [`Flush d1] if given output buffer [o] (see {!decoder}) needs to be
        drained before work can be resumed. The client must use {!flush} with
        [d1] to {b completely} flush [o]. Usually [o] will be full and consist
        fully of bytes that need to be copied from the buffer, but sometimes
        only the first part of the buffer is used. In those cases {!dst_rem}
        will give you the amount of free/unused bytes remain in [o]. These
        should {b not} be copied since their contents are not part of the
        output. Instead, the first [bigstring_length o - Inf.dst_rem d1] bytes
        should be copied when flushing [o].
      - [`Malformed err] if given input is malformed. [err] is a human-readable
        error message.
      - [`End d1] if given input notify end of flow. [o] is possibly not empty
        (it can be check with {!dst_rem}). *)

  val reset : decoder -> decoder
  (** [reset d] is a [d] in its original state, as it was initialized by
      {!decoder}. *)

  val src : decoder -> bigstring -> int -> int -> decoder
  (** [src d s j l] provides [d] with [l] bytes to read, starting at [j] in [s].
      This byte range is read by calls to {!decode} with [d] until [`Await] is
      returned. To signal the end of input call the function with [l = 0].

      @raise Invalid_argument
        when [j] and [l] do not correspond to a valid range. *)

  val dst_rem : decoder -> int
  (** [dst_rem d] is how many unused bytes remain in the output buffer of [d].
  *)

  val src_rem : decoder -> int
  (** [src_rem d] is how many unprocessed bytes remain in the input buffer of
      [d]. *)

  val write : decoder -> int
  (** [write d] is how many bytes [d] emitted since it was created. *)

  val flush : decoder -> decoder
  (** [flush d] is a decoder where internal output buffer [o] is {b completely}
      free to store bytes. *)

  module Ns : sig
    (** A non-streamable implementation of the RFC 1950. It considers the input
        to be whole and is therefore able to save some time *)

    type error = [ `Invalid_header | `Invalid_checksum | De.Inf.Ns.error ]
    (** The type for inflation errors. *)

    val pp_error : Format.formatter -> error -> unit
    (** Pretty-printer of {!error}. *)

    val inflate : bigstring -> bigstring -> (int * int, [> error ]) result
    (** [inflate src dst] inflates the content of [src] into [dst].

        In case of success, it returns the bytes read and bytes writen in an
        [Ok] result. In case of failure, it returns the error in a [Error]
        result. *)
  end
end

(** {2:encode ZLIB Encoder.}

    ZLIB encoder is glue between the LZ77 algorithm and the DEFLATE encoder,
    prefixed with a ZLIB header. Any deal with compression algorithm is not
    possible on this layer (see {!De} for more details). As {!Inf}, and unlike
    {!De}, {!Zl} provides a referentially transparent encoder.

    The client must use the {!Def.encoder} given {b by} {!Def.encode} instead a
    [encoder] given {b to} {!Def.encode}. *)

module Def : sig
  type src = [ `Channel of in_channel | `String of string | `Manual ]
  (** The type for input sources. With a [`Manual] source the client must
      provide input with {!src}. With [`String] or [`Channel] source the client
      can safely discard [`Await] case (with [assert false]). *)

  type dst = [ `Channel of out_channel | `Buffer of Buffer.t | `Manual ]
  (** The type for output destinations. With a [`Manual] destination the client
      must provide output storage with {!dst}. With [`Buffer] or [`Channel]
      destination the client can safely discard [`Flush] case (with
      [assert false]). *)

  type encoder
  (** The type for ZLIB encoders. *)

  type ret = [ `Await of encoder | `End of encoder | `Flush of encoder ]

  val encoder :
       ?dynamic:bool
    -> q:De.Queue.t
    -> w:De.Lz77.window
    -> level:int
    -> src
    -> dst
    -> encoder
  (** [encoder ~q ~w ~level src dst] is an encoder that inputs from [src] and
      that outputs to [dst].

      {b Internal queue.}

      [encoder] deals internally with compression algorithm and DEFLATE encoder.
      To pass compression values to DEFLATE encoder, we need a queue [q]. Length
      of [q] has an impact on performance, and small lengths can be a
      bottleneck, leading {!encode} to emit many [`Flush]. We recommend a queue
      as large as output buffer.

      {b Window.}

      ZLIB is able to constrain length of window used to do LZ77 compression.
      However, small window can slow-down LZ77 compression algorithm. Small
      windows are mostly used to enable inflation of output in
      memory-constrained environments, for example when compressed data from
      untrusted sources must be processed.

      {b Level.}

      Zlib implements 10 levels (from 0 to 9). All of them use the dynamic &
      canonic huffman if [dynamic] is [true] (default). Otherwise, we use the
      static huffman. The higher the level, the better the ratio. *)

  val src_rem : encoder -> int
  (** [src_rem e] is how many bytes it remains in given input buffer. *)

  val dst_rem : encoder -> int
  (** [dst_rem e] is how many unused bytes remain in the output buffer of [e].
  *)

  val src : encoder -> bigstring -> int -> int -> encoder
  (** [src e s j l] provides [e] with [l] bytes to read, starting at [j] in [s].
      This byte range is read by calls to {!encode} with [e] until [`Await] is
      returned. To signal the end of input call the function with [l = 0].

      @raise Invalid_argument
        when [j] and [l] do not correspond to a valid range. *)

  val dst : encoder -> bigstring -> int -> int -> encoder
  (** [dst e s j l] provides [e] with [l] bytes available to write, starting at
      [j] in [s]. This byte range is fill by calls to {!encode} with [e] until
      [`Flush] is returned.

      @raise Invalid_argument
        when [j] and [l] do not correspond to a valid range. *)

  val encode : encoder -> ret
  (** [encode e0] is:

      - [`Await e1] if [e0] has a [`Manual] input source and awaits for more
        input. The client must use {!src} with [e1] to provide it.
      - [`Flush e1] if [e0] has a [`Manual] destination and needs more output
        storage. The client must drain the buffer before resuming operation.
      - [`End e1] if [e1] encoded all input. Output buffer is possibly not empty
        (it can be check with {!dst_rem}).

      {b Limitation.}

      The encoder must manipulate an output buffer of, at least, 2 bytes. If
      it's not the case, [encode] does nothing - and it tells you nothing more
      than it did nothing. Depending on what you do, a loop can infinitely call
      [encode] without any updates until the given output still has less than 2
      bytes. *)

  module Ns : sig
    type error = De.Def.Ns.error
    (** The type for deflation errors. *)

    val pp_error : Format.formatter -> error -> unit
    (** Pretty-printer for {!error}. *)

    val compress_bound : int -> int
    (** [compress_bound len] returns a {i clue} about how many bytes we need to
        store the result of the deflation of [len] bytes. It's a pessimistic
        calculation. *)

    val deflate :
      ?level:int -> bigstring -> bigstring -> (int, [> error ]) result
    (** [deflate ~level src dst] deflates the content of [src] into [dst].

        In case of success, it returns the bytes writen in an [Ok] result. In
        case of failure, it returns the error in an [Error] result.
        {!compress_bound} can be used to {i determine} how many bytes the user
        needs to allocate as the destination buffer when he wants to compress
        [N] bytes.

        Here is an example of how to compress any inputs:
        {[
          val input : bigstring

          let len = Zl.Def.Ns.compress_bound (De.bigstring_length input) in
          let dst = De.bigstring_create len in
          Zl.Def.Ns.deflate ~level:4 input dst
        ]} *)
  end
end

module Higher : sig
  val compress :
       ?level:int
    -> ?dynamic:bool
    -> w:De.Lz77.window
    -> q:De.Queue.t
    -> refill:(bigstring -> int)
    -> flush:(bigstring -> int -> unit)
    -> bigstring
    -> bigstring
    -> unit
  (** [compress ?level ?dynamic ~w ~q ~refill ~flush i o] is [Zlib.compress]
      (with [~header:true]) provided by [camlzip] package.

      - [w] is the window used by LZ77 compression algorithm.
      - [q] is shared-queue between compression algorithm and DEFLATE encoder.
      - [i] is input buffer.
      - [o] is output buffer.

      When [compress] wants more input, it calls [refill] with [i]. The client
      returns how many bytes he wrote into [i]. If he returns 0, he signals end
      of input.

      When [compress] has written output buffer, it calls [flush] with [o] and
      how many bytes it wrote. Bytes into [o] must be {b copied} and they will
      be lost at the next call to [flush].

      A simple example of how to use such interface is:
      {[
      let deflate_string ?(level = 4) str =
        let i = De.bigstring_create De.io_buffer_size in
        let o = De.bigstring_create De.io_buffer_size in
        let w = De.Lz77.make_window ~bits:15 in
        let q = De.Queue.create 0x1000 in
        let r = Buffer.create 0x1000 in
        let p = ref 0 in
        let refill buf =
          let len = min (String.length str - !p) De.io_buffer_size in
          Bstr.blit_from_string str ~src_off:!p buf ~dst_off:0 ~len
          ; p := !p + len
          ; len in
        let flush buf len =
          let str = Bstr.sub_string buf ~off:0 ~len in
          Buffer.add_string r str in
        Zl.Higher.compress ~level ~dynamic:true ~w ~q ~refill ~flush i o
        ; Buffer.contents r
      ]}

      As {!De.Higher.compress}, several choices was made in this code and
      [decompress] don't want to be responsible of them. It's why such function
      exists only as example when lengths of buffers (such as [i], [o] or [q])
      changes the speed/compression ratio/memory consumption. *)

  val uncompress :
       allocate:(int -> window)
    -> refill:(bigstring -> int)
    -> flush:(bigstring -> int -> unit)
    -> bigstring
    -> bigstring
    -> (unit, [> `Msg of string ]) result
  (** [uncompress ~allocate ~refill ~flush i o] is [Zlib.uncompress] (with
      [~header:true]) provided by [camlzip] package.

      - [allocate] is the allocator of window used by LZ77 uncompression
        algorithm
      - [i] is input buffer.
      - [o] is output buffer.

      When [uncompress] wants more input, it calls [refill] with [i]. The client
      returns how many bytes he wrote into [i]. If he returns 0, he signals end
      of input.

      When [uncompress] has written output buffer, it calls [flush] with [o] and
      how many bytes it wrote. Bytes into [o] must be {b copied} and they will
      be lost at the next call to [flush].

      A simple example of how to use such interface is:
      {[
      let inflate_string str =
        let i = De.bigstring_create De.io_buffer_size in
        let o = De.bigstring_create De.io_buffer_size in
        let allocate bits = De.make_window ~bits in
        let r = Buffer.create 0x1000 in
        let p = ref 0 in
        let refill buf =
          let len = min (String.length str - !p) De.io_buffer_size in
          Bstr.blit_from_string str ~src_off:!p buf ~dst_off:0 ~len
          ; p := !p + len
          ; len in
        let flush buf len =
          let str = Bstr.sub_string buf ~off:0 ~len in
          Buffer.add_string r str in
        match Zl.Higher.uncompress ~allocate ~refill ~flush i o with
        | Ok () -> Ok (Buffer.contents r)
        | Error _ as err -> err
      ]}

      As you can see, several allocations appear. As long as you want to
      uncompress several contents for example, you can re-use the same
      {i window} instead of an allocation of one per uncompression. Then, the
      throughput is mostly limited by [i] and [o] (even bigger, even faster but
      it requires memories). [decompress] don't want to be responsible about
      these choices, it's why such function exists only as an example. *)
end
