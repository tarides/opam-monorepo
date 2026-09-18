(** {1 GZIP layer.}

    GZIP is a standard on top of RFC1951 according RFC1952. It uses the {!De}
    implementation with the LZ77 compression algorithm. Module provides
    non-blocking streaming codec to {{:#decode}decode} and {{:#encode}encode}
    GZIP encoding. It can efficiently work payload by payload without blocking
    IO. *)

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
(** Type type for [bigstring]. *)

type window = De.window
(** The type for sliding window. *)

val io_buffer_size : int

(** The type for Operating-System. *)
type os =
  | FAT
  | Amiga
  | VMS
  | Unix
  | VM
  | Atari
  | HPFS
  | Macintosh
  | Z
  | CPM
  | TOPS20
  | NTFS
  | QDOS
  | Acorn
  | Unknown

val pp_os : Format.formatter -> os -> unit
(** Pretty-printer of {!os}. *)

val equal_os : os -> os -> bool
(** [equal_os a b] returns [true] if [a] is exactly the same {!os} than [b].
    Otherwise, it returns [false]. *)

(** {2:decode GZIP Decoder.}

    Unlike [de], [gz] provides a referentially transparent {!Inf.decoder}. The
    client must use a {!Inf.decoder} given {b by} {!Inf.decode} instead of a
    decoder given {b to} {!Inf.decode}. A common use of [gz] is:

    {[
      let rec go d0 = match Inf.decode d0 with
        | `Await d1 -> ... go d1
        | `Flush d1 -> ... go d1
        | _ -> .... in
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

  val decoder : src -> o:bigstring -> decoder
  (** [decoder src ~o] is a decoder that inputs from [src].

      {b Output buffer.}

      [gz], as [de], uses [o] buffer as internal buffer to store output. We
      recommend to allocate an {!io_buffer_size} buffer as output buffer. Then,
      {!dst_rem}[ decoder] tells you how many unused bytes remain in [o]. *)

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

  val filename : decoder -> string option
  (** [filename d] returns the {i filename} of the flow if it exists. This can
      be called anytime but should be called when the [`End] case appears (and
      ensure that the GZIP header was computed). *)

  val comment : decoder -> string option
  (** [comment d] returns the {i comment} of the flow if it exists. This can be
      called anytime but should be called when the [`End] case appears (and
      ensure that the GZIP header was computed). *)

  val os : decoder -> os
  (** [os d] returns the {!os} where the flow was compressed. It should be
      called when the [`End] case appears (and ensure that the GZIP header was
      computed). *)

  val extra : key:string -> decoder -> string option
  (** [extra ~key d] returns extra {i field} [key] if it exists. This can be
      called anytime but should be called when the [`End] case appears (and
      ensure that the GZIP header was computed).

      @raise Invalid_argument
        if the length of the given [key] is not equal to 2. *)
end

(** {2:encode GZIP Encoder.}

    GZIP encoder is glue between the LZ77 algorithm and the DEFLATE encoder,
    prefixed with a GZIP header. Any deal with compression algorithm is not
    possible on this layer (see {!De} for more details). As {!Inf}, and unlike
    {!De}, {!Gz} provides a referentially transparent encoder.

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
  (** The type for GZIP encoders. *)

  type ret = [ `Await of encoder | `End of encoder | `Flush of encoder ]

  val encoder :
       src
    -> dst
    -> ?ascii:bool
    -> ?hcrc:bool
    -> ?filename:string
    -> ?comment:string
    -> mtime:int32
    -> os
    -> q:De.Queue.t
    -> w:De.Lz77.window
    -> level:int
    -> encoder
  (** [encoder src dst ~mtime os ~q ~w ~level] is an encoder that inputs from
      [src] and that outputs to [dst].

      {b Internal queue.}

      [encoder] deals internally with compression algorithm and DEFLATE encoder.
      To pass compression values to DEFLATE encoder, we need a queue [q]. Length
      of [q] has an impact on performance, and small lengths can be a
      bottleneck, leading {!encode} to emit many [`Flush]. We recommend a que as
      large as output buffer.

      {b Window.}

      GZIP needs a sliding window to operate the LZ77 compression. The window
      must be a {i 32k} window ({!De.make_window} with [bits = 15]). The
      allocated window can be re-used by an other inflation/deflation process -
      but it {b can not} be re-used concurrently or cooperatively with another
      inflation/deflation process.

      {b Level.}

      Current implementation of GZIP does not handle any compression level.
      However, the client must give a level between 0 and 9, inclusively,
      Otherwise, we raise an [Invalid_argument].

      {b Metadata.}

      Client is able to store some {i metadata} such as:
      - [mtime] time of last modification of the input.
      - [os] {!os} which did the compression.
      - [filename] {i filename} of the input (no limitation about length).
      - [comment] an arbitrary {i payload} (no limitation about length).
      - [ascii] if encoding of contents is ASCII.
      - [hcrc] if the client wants a checksum of the GZIP header. *)

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
      - [`End e1] if [e0] encoded all input. Output buffer is possibly not empty
        (it can be check with {!dst_rem}).

      {b Limitation.}

      The encoder must manipulate an output buffer of, at least, 2 bytes. If
      it's not the case, [encode] does nothing - and it tells you nothing more
      than it did nothing. Depending on what you do, a loop can infinitely call
      [encode] without any updates until the given output still has less than 2
      bytes. *)
end

module Higher : sig
  type 't configuration
  (** Type of the Operating-System configuration. *)

  val configuration :
    ?ascii:bool -> ?hcrc:bool -> os -> ('t -> int32) -> 't configuration
  (** [configuration ?ascii ?hcrc os mtime] makes an Operating-System
      {!configuration} to be able to {!compress} any inputs. *)

  val compress :
       ?level:int
    -> ?filename:string
    -> ?comment:string
    -> w:De.Lz77.window
    -> q:De.Queue.t
    -> refill:(bigstring -> int)
    -> flush:(bigstring -> int -> unit)
    -> 't
    -> 't configuration
    -> bigstring
    -> bigstring
    -> unit
  (** [compress ?level ?filename ?comment ~w ~q ~refill ~flush time cfg i o]
      compresses an input given by [refill] and outputs it via [flush]. It
      requires:
      - a queue [q] which is shared between the compression algorithm and the
        encoder. The length of it can be a bottleneck on the throughput
      - a {i window} to be able to lookup repeated patterns
      - a {i witness} required by the given [cfg]
      - a {!configuration} value
      - [i] is the input buffer
      - [o] is the output buffer

      When [compress] wants more input, it calls [refill] with [i]. The client
      returns how many bytes he wrotes into [i]. If he returns 0, he signals end
      of input.

      When [compress] has written output buffer, it calls [flush] with [o] and
      how many bytes it wrote. Bytes into [o] must be {b copied} and they will
      be lost at the next call to [flush].

      A simple example of how to use such interface (with [unix]) is:
      {[
      let time () = Int32.of_float (Unix.gettimeofday ())

      let deflate_string ?(level = 4) str =
        let i = De.bigstring_create De.io_buffer_size in
        let o = De.bigstring_create De.io_buffer_size in
        let w = De.Lz77.make_window ~bits:15 in
        let q = De.Queue.create 0x1000 in
        let r = Buffer.create 0x1000 in
        let p = ref 0 in
        let cfg = Gz.Higher.configuration Gz.Unix time in
        let refill buf =
          let len = min (String.length str - !p) De.io_buffer_size in
          Bstr.blit_from_string str ~src_off:!p buf ~dst_off:0 ~len
          ; p := !p + len
          ; len in
        let flush buf len =
          let str = Bstr.sub_string buf ~off:0 ~len in
          Buffer.add_string r str in
        Gz.Higher.compress ~w ~q ~level ~refill ~flush () cfg i o
        ; Buffer.contents r
      ]}

      As {!De.Higher.compress} or {!Zl.Higher.compress}, [decompress] don't want
      to take the responsability of such function. It's why this function exists
      only as an example. Especially since a GZip compression requires an Unix
      {i syscall} (see [Unix.gettimeofday]) which does not exists on some
      contexts such as MirageOS.

      The speed and the compression ratio depends on the length of the given
      {i window} and the given {!De.Queue.t}. They can be a serious bottleneck
      on the throughput. Due to all of these choices, we show this function as
      an example - but it should not be copied as is! *)

  type metadata = {
      filename: string option
    ; comment: string option
    ; os: os
    ; extra: key:string -> string option
  }
  (** Type of {i metadata} available into a GZIP flow. *)

  val uncompress :
       refill:(bigstring -> int)
    -> flush:(bigstring -> int -> unit)
    -> bigstring
    -> bigstring
    -> (metadata, [> `Msg of string ]) result
  (** [uncompress ~refill ~flush i o] uncompresses an input given by [refill]
      and outputs it via [flush]. It requires:
      - [i] as the input buffer
      - [o] as the output buffer

      It returns then extracted {i metadata} from the given GZip flow.

      When [uncompress] wants more input, it calls [refill] with [i]. The client
      returns how many bytes he wrote into [i]. If he returns 0, he signals end
      of input.

      When [uncompress] has written output buffer, it calls [flush] with [o] and
      how many bytes it wrote. Bytes into [o] must be {b copied} and tjey will
      be lost at the next call to [flush].

      A simple example of how to use such interface is:
      {[
      let inflate_string str =
        let i = De.bigstring_create De.io_buffer_size in
        let o = De.bigstring_create De.io_buffer_size in
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
        match Gz.Higher.uncompress ~refill ~flush i o with
        | Ok m -> Ok (m, Buffer.contents r)
        | Error _ as err -> err
      ]}

      As {!De.Higher.uncompress} or {!Zl.Higher.uncompress}, [decompress] does
      not want to take the responsability of such implementation when several
      choices was made depending on the context. Indeed, [i] and [o] can be a
      serious bottleneck on the throughput. The choice of [Buffer] can be
      replaced by something else such as a queue or a {i ropes}. All of these
      choices should be made by the client. It's why we provide such function
      only as an example. *)
end
