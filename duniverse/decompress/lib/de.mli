(** {1 RFC1951/DEFLATE codec.}

    RFC1951/DEFLATE is a IETF standard. Module provides non-blocking streaming
    codec to {{:#decode}decode} and {{:#encode}encode} DEFLATE encoding. It can
    efficiently work payload by payload without blocking IO.

    Module provides {{:#compression}LZ77 compression} algorithm but lets the
    client to define his algorithm as long as he/she uses shared queue provided
    in this module.

    {1 Lz77 compression and huffman compression.}

    [Lz77] does a compression such as it searches a repeated {i pattern} and
    emits a [Copy] code (see {!type:Queue.cmd}) which tells us to copy a
    previous pattern. For instance, this is an equivalence between a list of
    {!type:Queue.cmd}s and a [string].

    {[
    let cmds = [`Literal 'a'; `Copy (1, 3)]
    let results = "aaaa"
    ]}

    The goal of [Lz77] is to produce such list from a [string] to help then a
    format such as [DEFLATE] to compress inputs.

    [Def] does an huffman compression. From a list of {!type:Queue.cmd}, it can
    calculate frequencies (see {!type:literals} and {!type:distances}) and
    generate a smaller representation of the given {i alphabet}. Such
    compression is available {i via} the {!const:Def.Fixed} or the
    {!const:Def.Dynamic} block. The {!const:Def.Flat} block does a copy of any
    literals into the output.

    {b NOTE}: It's illegal to emit a [`Copy] {!type:Queue.cmd} and try to
    serialize it with a {!const:Def.Flat} block. {!Queue.eob} is {b required}
    for {!const:Def.Fixed} and {!const:Def.Dynamic} (to delimit the end of the
    block) and ignored by the {!const:Def.Flat} block. *)

(** {2 Prelude.}

    [de] wants to be self-contained. By this constraint, it provides convenience
    values to be used by others (like [zl]). The client should not use these
    functions even if they are available. Others libraries like [Bstr] serve the
    same purpose of a much better way. *)

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
(** The type for [bigstring]. *)

type optint = Optint.t
(** Type type for optimal integer. *)

val bigstring_empty : bigstring
(** An empty {!bigstring}. *)

val bigstring_create : int -> bigstring
(** [bigstring_create len] returns a uninitialized bigstring of length [len]. *)

val bigstring_length : bigstring -> int
(** [bigstring_length t] is the length of the bigstring [t], in bytes. *)

val io_buffer_size : int

(** {2 Window.} *)

type window
(** The type for windows. *)

val make_window : bits:int -> window
(** [make_window] allocates a new buffer which represents a {i window}. It used
    by decoder and LZ77 compressor to keep tracking older inputs and:

    - process a copy from a distance by the decoder.
    - generate a copy from the compression algorithm. *)

val window_bits : window -> int

(** {2:decode DEFLATE Decoder.}

    Decoder of RFC1951 DEFLATE codec. [de] provides a {!Inf.decoder} to decode
    DEFLATE input and inflate it. *)

module Inf : sig
  type src = [ `Channel of in_channel | `String of string | `Manual ]
  (** The type for input sources. With a [`Manual] source the client must
      provide input with {!src}. With [`String] or [`Channel] source the client
      can safely discard [`Await] case (with [assert false]). *)

  type decode = [ `Await | `Flush | `End | `Malformed of string ]

  type decoder
  (** The type for decoders. *)

  val decoder : src -> o:bigstring -> w:window -> decoder
  (** [decoder src ~o ~w] is a decoder that inputs from [src].

      {b Output buffer.}

      [de] uses [o] buffer as internal buffer to store output. We recommend to
      allocate an {!io_buffer_size} buffer as output buffer. Then, {!dst_rem}
      gives you how many bytes it remains in [o].

      {b Window.}

      [de] needs a window to be able to interpret [`Copy] code. Length of window
      is commonly 32k bytes (but the client can use a smaller one with some
      assumptions). *)

  val decode : decoder -> decode
  (** [decode d] is:

      - [`Await] if [d] has a [`Manual] input source and awaits for more input.
        The client must use {!src} to provide it.
      - [`Flush d] if given output buffer [o] (see {!decoder}) is full. The
        client must use {!flush} to {b completely} flush [o]. {!dst_rem} gives
        you how many bytes it remains in [o].
        [Inf.dst_rem d - bigstring_length o] gives you how many bytes are
        available.
      - [`Malformed err] if given input is malformed. [err] is a human-readable
        error.
      - [`End] if given input notify end of flow. [o] is possibly not empty (it
        can be check with {!dst_rem}). *)

  val reset : decoder -> unit
  (** [reset d] is a decoder as is when it was created by {!decoder}. *)

  val src : decoder -> bigstring -> int -> int -> unit
  (** [src d s j l] provides [d] with [l] bytes to read, starting at [j] in [s].
      This byte range is read by calls to {!decode} with [d] until [`Await] is
      returned. To signal the end of input call the function with [l = 0].

      @raise Invalid_argument
        when [j] and [l] do not correspond to a valid range. *)

  val dst_rem : decoder -> int
  (** [dst_rem d] is how many bytes it remains in given output buffer [o]. *)

  val src_rem : decoder -> int
  (** [src_rem d] is how many bytes it remains in given input buffer. *)

  val flush : decoder -> unit
  (** [flush d] provides [d] with new output storage. *)

  val checksum : decoder -> optint
  (** [checkseum d] is ADLER-32 checksum of consumed inputs. *)

  module Ns : sig
    (** A non-streamable implementation of the RFC 1951. It considers the input
        to be whole and is therefore able to save some time *)

    type error =
      [ `Unexpected_end_of_input
      | `Unexpected_end_of_output
      | `Invalid_kind_of_block
      | `Invalid_dictionary
      | `Invalid_complement_of_length
      | `Invalid_distance
      | `Invalid_distance_code ]
    (** The type for inflation errors. *)

    val pp_error : Format.formatter -> error -> unit
    (** Pretty-printer of {!error}. *)

    val inflate : bigstring -> bigstring -> (int * int, [> error ]) result
    (** [inflate src dst w] inflates the content of [src] into [dst].

        In case of sucess, it returns the bytes read and the bytes writen in an
        [Ok] result. In case of failure, it returns the error in an [Error]
        result. We assume that [src] is well formed and [dst] is enough larger
        to store the result of the inflation. The usual worst case is when [dst]
        must be equal (in size) or larger than [src]. Such case appears for
        really small objects and in that case, we returns
        [Error `Unexpected_end_of_output]. *)
  end
end

(** {2 Queue.}

    DEFLATE encoder needs a compressed input which can be transmited by a shared
    queue filled by compression algorithm. {!B} is used between {!N} and a
    compression algorithm like {!L}. It provides a small representation of
    commands (see {!Queue.cmd}) emitted by compression algorithm.

    {!N} encoder interprets {!Queue.cmd} as fast as it can. Shared queue can be
    a bottleneck about the whole compression process. Indeed, it limits encoder
    on how many bytes it can produce. We recommend to make a queue as large as
    output buffer. *)

module Queue : sig
  type cmd [@@immediate]
  (** The type for commands. *)

  type t
  (** The type for queues.

      A command is a small representation of a [`Literal] or a [`Copy] command.
      A [`Copy] command is usually emitted by a compression algorithm to inform
      to copy [length] byte(s) appeared [offset] byte(s) before.

      DEFLATE has some limitations about [`Copy] command. *)

  exception Full
  (** Raised when {!push_exn} is applied to a full queue. *)

  exception Empty
  (** Raised when {!junk_exn} or {!pop_exn} is applied to an empty queue. *)

  val is_empty : t -> bool
  (** Return [true] if the given queue is empty, [false] otherwise. *)

  val is_full : t -> bool
  (** Return [true] if the given queue is full, [false] otherwise. *)

  val length : t -> int
  (** Return the number of elements in the given queue. *)

  val available : t -> int
  (** Free cells available on the given queue. *)

  val push_exn : t -> cmd -> unit
  (** [push_exn q x] adds the element [x] at the end of the queue [q]. It raises
      {!Full} if the given queue [q] is full. *)

  val pop_exn : t -> cmd
  (** [pop_exn q] removes and returns the first element in the given queue [q].
      It raises {!Empty} if the given queue [q] is empty. *)

  val junk_exn : t -> int -> unit
  (** [junk_exn q n] discards [n] elements in the given queue [q]. If [q] does
      not have enough elements, it raises {!Empty} and the given queue is
      unchanged. *)

  val copy : off:int -> len:int -> cmd
  (** [copy ~off ~len] is a {!cmd} for a {i copy} code. *)

  val literal : char -> cmd
  (** [literal chr] is a {!cmd} for a character. *)

  val eob : cmd
  (** [eob] is {i End Of Block} {!cmd}.*)

  val end_with_eob : t -> bool
  (** [end_with_eob t] returns [true] if the last inserted command is {!eob}.
      Otherwise, it returns [false]. *)

  val cmd : [ `Literal of char | `Copy of int * int | `End ] -> cmd
  (** [cmd command] is {!cmd} from a human-readable value. *)

  val blit : t -> bigstring -> int -> int -> unit
  (** [blit q payload off len] {i blits} elements in [payload] to the given
      queue [q] at the end (like a fast iterative {!push_exn} with literal
      elements). If the given queue [q] does not have enough free space to write
      [payload], it raises {!Full} and the given queue is unchanged. *)

  val create : int -> t
  (** [create len] allocates a new queue, initially empty. [len] must be a power
      of two, otherwise it raises [Invalid_argument]. *)

  val reset : t -> unit
  (** Discard all elements from a queue. *)

  val to_list : t -> [ `Literal of char | `Copy of int * int | `End ] list
  val of_list : [ `Literal of char | `Copy of int * int | `End ] list -> t
end

(** {2 Frequencies.}

    DYNAMIC DEFLATE block needs frequencies of code emitted by compression
    algorithm. {!literals} and {!distances} exist to keep frequencies while
    compression process. *)

type literals = private int array
(** The type of frequencies of literals (including lengths). *)

type distances = private int array
(** The type of frequencies of distances. *)

val make_literals : unit -> literals
(** [make_literals] allocates a new {!literals} where frequencies of symbols
    (expect {i End Of Block}) are set to 0. *)

val make_distances : unit -> distances
(** [make_distances] allocates a new {!distances} where frequencies of
    {i distance} symboles are set to 0. *)

val succ_literal : literals -> char -> unit
(** [succ_literals literals chr] increases frequency of [chr]. *)

val succ_length : literals -> int -> unit
(** [succ_length literals l] increases frequency of [l] code. [l] must be upper
    than 2 and lower than 259 according DEFLATE codec. Otherwise, it raises an
    [Invalid_argument]. *)

val succ_distance : distances -> int -> unit
(** [succ_distance distance d] increases frequency of [d] code. [d] must be
    upper than 0 and lower than 32769 according DEFLATE codec. Otherwise, it
    raises an [Invalid_argument]. *)

(** {2:encode DEFLATE Encoder.} *)

module Def : sig
  type dst = [ `Channel of out_channel | `Buffer of Buffer.t | `Manual ]
  (** The type for output destinations. With a [`Manual] destination the client
      must provide output storage with {!dst}. With [`Buffer] or [`Channel]
      destination the client can safely discard [`Flush] case (with
      [assert false]). *)

  type dynamic
  (** The type for DEFLATE DYNAMIC block. *)

  (** The type for DEFLATE header block. *)
  type kind =
    | Flat
        (** A [Flat len] block is a non-compressed block of [len] byte(s). After
            a {i flat} block, output is aligned on bytes. *)
    | Fixed
        (** A [Fixed] block is a compressed block by a precomputed Huffman tree.
            Any symbols can be encoded with this kind of block - {!encode}
            should never return [`Block] with it. *)
    | Dynamic of dynamic
        (** A [Dynamic h] block is a compressed block by an Huffman tree
            represented by [h]. It allows to encode a subset of symbols (or any
            symbols). *)

  type block = {kind: kind; last: bool}
  (** The type for DEFLATE block. *)

  type encode = [ `Await | `Flush | `Block of block ]
  (** The type for user action into {!encoder}:

      - [`Await] is expected until {!encode} returns [`Ok].
      - [`Flush] asks to [encoder] to produce output as long as it can.
      - [`Block block] asks to [encoder] to produce a new block [block] - if
        current block is last block (see {!block}), {!encode} raises an
        [Invalid_argument]. *)

  val dynamic_of_frequencies :
    literals:literals -> distances:distances -> dynamic
  (** [dynamic_of_frequencies ~literals ~distances] is a DEFLATE DYNAMIC block
      header computed from given frequencies. According frequencies,
      [dynamic_of_frequencies] makes a Huffman tree which provides smaller
      representation for symbols which frequency is upper than 0 (others symbols
      are not a part of resulted Huffman tree). At the end, a [dynamic] Huffman
      tree is able to encode a subset of symbols.

      If all frequencies are upper than 0, resulted [dynamic] Huffman tree is
      able to encode any symbols. *)

  val block_of_frequencies :
    last:bool -> literals:literals -> distances:distances -> block
  (** [block_of_frequencies ~last ~literals ~distances] is a {!block} whose
      {!kind} is whichever a {!Fixed} (static Huffman trees) or a {!Dynamic}
      (Huffman trees computed from the frequencies) block. It choose the block
      header in the most compact way.

      A dynamic block always pays for the description of its trees, so
      [block_of_frequencies] falls back to a static block on small or very
      uniform inputs where that overhead is not amortised. *)

  type encoder
  (** The type for DEFLATE encoders. *)

  val encoder : dst -> q:Queue.t -> encoder
  (** [encoder dst ~q] is an encoder that outputs to [dst].

      {b Internal queue.}

      [encoder] needs a side-channel about compressed inputs. To pass
      compression values to [encoder], we use a queue [q]. Length of it can be a
      bottleneck where a small one will let {!encode} to emit too many [`Flush]
      (which is commonly associated to a {i syscall}). We recommend a queue as
      large as output buffer. *)

  val encode : encoder -> encode -> [ `Ok | `Partial | `Block ]
  (** [encode e v] is:

      - [`Partial] iff [e] has a [`Manual] destination and needs more output
        storage. The client must use {!dst} to provide a new buffer and then
        call {!encode} with [`Await] until [`Ok] is returned.
      - [`Ok] when the encoder is ready to encode a new {!encode} action.
      - [`Block] when the encoder reachs a {!Queue.cmd} which can not be encoded
        with the current {!block}. The client must respond with [`Block block]
        where [block] is a new block able to encode current {!Queue.cmd}.

      {b How to signal end of flow?}

      End of flow is characterized by a {!block} where [last = true]. Then, the
      client must emit into the queue [q] {!Queue.eob}.

      {b Limitation.}

      The encoder must manipulate an output buffer of, at least, 2 bytes. If
      it's not the case, [encode] does nothing - and it tells you nothing more
      than it did nothing. Depending on what you do, a loop can infinitely call
      [encode] without any updates until the given output still has less than 2
      bytes. *)

  val dst : encoder -> bigstring -> int -> int -> unit
  (** [dst e s j l] provides [e] with [l] bytes available to write, starting at
      [j] in [s]. This byte range is read by calls to {!encode} with [e] until
      [`Flush] is returned.

      @raise Invalid_argument
        when [j] and [l] do not correspond to a valid range. *)

  val dst_rem : encoder -> int
  (** [dst_rem e] is how many bytes it remains in given output buffer. *)

  val bits_rem : encoder -> int
  (** [bits_rem e] is how many {b bits} it remains in given output buffer. A
      DEFLATE flow is not necessary aligned on bytes. The client can call
      [bits_rem] {b only} when he reachs [`End] case. Otherwise, we raises an
      [Invalid_argument]. *)

  module Ns : sig
    type error = [ `Invalid_compression_level | `Unexpected_end_of_output ]
    (** The type for deflation errors. *)

    val pp_error : Format.formatter -> error -> unit
    (** Pretty-printer for {!error}. *)

    val compress_bound : int -> int
    (** [compress_bound len] returns a {i clue} about how many bytes we need to
        store the result of the deflation of [len] bytes. It's a pessimistic
        calculation. *)

    val deflate :
      ?level:int -> bigstring -> bigstring -> (int, [> error ]) result
    (** [deflate ~level src dst] deflates the content of src into dst.

        In case of sucess, it returns the bytes writen in an [Ok] result. In
        case of failure, it returns the error in an [Error] result.
        {!compress_bound} can be used to {i determine} how many bytes the user
        needs to allocate as the destination buffer when he wants to compress
        [N] bytes.

        Here is an example of how to compress any inputs:
        {[
          val input : bigstring

          let len = De.Def.Ns.compress_bound (De.bigstring_length input) in
          let dst = De.bigstring_create len in
          De.Def.Ns.deflate ~level:4 input dst
        ]} *)
  end
end

(** {2:compression LZ77 compression algorithm.}

    Distribution provides LZ77 compression algorithm which can be used with
    {!N}. However, the client must know others algorithms exist. This algorithm
    is used by [zz] to implement ZLIB layer. *)

module Lz77 : sig
  type src = [ `Channel of in_channel | `String of string | `Manual ]
  (** The type for input sources. With a [`Manual] source the client must
      provide input with {!src}. With [`String] or [`Channel] source the client
      can safely discard [`Await] case (with [assert false]). *)

  type decode = [ `Flush | `Await | `End ]

  type state
  (** The type for states. *)

  val literals : state -> literals
  (** [literals s] is frequencies of lengths and literals emitted by [s] since
      it was created. *)

  val distances : state -> distances
  (** [distances s] is frequencies of distances emitted by [s] since it was
      created. *)

  val checksum : state -> optint
  (** [checksum s] is ADLER-32 checksum of consumed inputs. *)

  val src : state -> bigstring -> int -> int -> unit
  (** [src s i j l] provides [s] with [l] bytes to read, starting at [j] in [i].
      This byte range is read by calls to {!compress} with [s] until [`Await] is
      returned. To signal the end of input call the function with [l = 0].

      @raise Invalid_argument
        when [j] and [l] do not correspond to a valid range. *)

  val src_rem : state -> int
  (** [src_rem s] is how many bytes it remains in given input buffer. *)

  val compress : state -> decode
  (** [compress s] is:

      - [`Await] if [s] has a [`Manual] input source and awits for more input.
        The client must use {!src} to provide it.
      - [`Flush] if [s] filled completely the shared-queue [q] (given in
        {!state}). {!Queue.junk_exn} or {!Queue.pop_exn} can be used to give
        some free cells to {!compress}.
      - [`End] if [s] compressed all input. Given shared-queue [q] is possibly
        not empty. *)

  type window

  val make_window : bits:int -> window

  val state : ?level:int -> q:Queue.t -> w:window -> src -> state
  (** [state src ~w ~q] is an state that inputs from [src] and that outputs to
      [q].

      {b Window.}

      The client can constrain lookup operation by a {i window}. Small window
      enforces {!compress} to emit small distances. However, large window allows
      {!compress} to go furthermore to recognize a pattern which can be
      expensive.

      {b Level.}

      [Lz77] has mainly 2 levels:
      - [0] where we only copy inputs to outpus, we don't do a lookup
      - [n] (to [9]) with a certain configuration of the lookup. The higher the
        level, the longer it may take to find a pattern.

      The [0] can be useful to {b only} {i pack} an input into a format such as
      [DEFLATE] - as an already compressed document such as a video or an image.
      Otherwise, [4] as the level is pretty common. *)

  val no_compression : state -> bool
end

(** {2 Higher API.}

    [de] provides useful but complex API. This sub-module provides an easier way
    to compress/uncompress DEFLATE codec. Even if the client still can give some
    details, we recommend to use {!Inf} and {!Def} if you want a precise control
    about memory consumption. *)

module Higher : sig
  val compress :
       w:Lz77.window
    -> q:Queue.t
    -> refill:(bigstring -> int)
    -> flush:(bigstring -> int -> unit)
    -> bigstring
    -> bigstring
    -> unit
  (** [compress ~w ~q ~refill ~flush i o] is [Zlib.compress] (with
      [~header:false]) provided by [camlzip] package.

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
      let deflate_string str =
        let i = De.bigstring_create De.io_buffer_size in
        let o = De.bigstring_create De.io_buffer_size in
        let w = De.Lz77.make_window ~bits:15 in
        let q = De.Queue.create 0x1000 in
        let r = Buffer.create 0x1000 in
        let p = ref 0 in
        let refill buf =
          (* assert (buf == i); *)
          let len = min (String.length str - !p) De.io_buffer_size in
          Bstr.blit_string str ~src_off:!p buf ~dst_off:0 ~len
          ; p := !p + len
          ; len in
        let flush buf len =
          (* assert (buf == o); *)
          let str = Bstr.sub_string buf ~off:0 ~len in
          Buffer.add_string r str in
        De.Higher.compress ~w ~q ~refill ~flush i o
        ; Buffer.contents r
      ]}

      As you can see, we allocate several things such as input and output
      buffers. Such choice should be decided by the end-user - and it's why we
      don't provide such function. The speed or the compression ratio depends on
      the length of:
      - [q] which is shared between the compression algorithm and the encoder
      - [i] which is the input buffer (and allows a large {i lookup} or not)
      - [o] which is the output buffer (it can be a bottle-neck for the
        throughput)
      - [w] which is the {i lookup-window}
      - [r] which is the data-structure to save the output (it can be a buffer,
        a queue, a [out_channel], etc.)

      As we said, several choices depends on what you want and your context. We
      deliberately choose to be not responsible on these choices. It's why such
      function exists only as an example - and it's not a part of the
      distribution. *)

  val uncompress :
       w:window
    -> refill:(bigstring -> int)
    -> flush:(bigstring -> int -> unit)
    -> bigstring
    -> bigstring
    -> (unit, [> `Msg of string ]) result
  (** [uncompress ~w ~refill ~flush i o] is [Zlib.uncompress] (with
      [~header:false]) provided by [camlzip] package.

      - [w] is the window used by LZ77 uncompression algorithm
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
      let inflate_string str =
        let i = De.bigstring_create De.io_buffer_size in
        let o = De.bigstring_create De.io_buffer_size in
        let w = De.make_window ~bits:15 in
        let r = Buffer.create 0x1000 in
        let p = ref 0 in
        let refill buf =
          let len = min (String.length str - !p) De.io_buffer_size in
          Bstr.blit_from_string str ~src_off:!p buf ~dst_off:0 ~len
          ; p := !p + len
          ; len in
        let flush buf len =
          let str = Bstr.sub_string buf ~off:0 ~len in
          Buffer.add_string r buf in
        match De.Higher.uncompress ~w ~refill ~flush i o with
        | Ok () -> Ok (Buffer.contents r)
        | Error _ as err -> err
      ]}

      As you can see, we allocate several things such as input and output
      buffers. As {!compress}, these details should be decided by the end-user.
      The speed of the decompression depends on the length of:
      - [i] which is the input buffer (it can a bottle-neck for the throughput)
      - [o] which is the output buffer (it can be a bottle-neck for the
        throughput)

      The {i window} depends on how you deflated/compressed the input. Usually,
      we allow a {i window} of 15 bits. *)

  val of_string :
       o:bigstring
    -> w:window
    -> string
    -> flush:(bigstring -> int -> unit)
    -> (unit, [> `Msg of string ]) result

  val to_string :
       ?buffer:int
    -> w:window
    -> q:Queue.t
    -> refill:(bigstring -> int)
    -> bigstring
    -> string
end

(** / **)

val unsafe_set_cursor : Inf.decoder -> int -> unit

module Lookup : sig
  type t = {t: int array; m: int; l: int; root: int}

  val get : t -> int -> int * int
end

module T : sig
  type tree = {lengths: int array; max_code: int; tree: Lookup.t}

  val make :
    length:int -> ?max_length:int -> int array -> bl_count:int array -> tree
end
