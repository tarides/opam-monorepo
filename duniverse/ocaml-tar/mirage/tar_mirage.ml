(*
 * Copyright (C) 2015 Citrix Systems Inc.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** Tar archives as read-only key=value stores for Mirage *)

open Lwt.Infix

module StringMap = Map.Make(String)

module Make_KV_RO (BLOCK : Mirage_block.S) = struct

  type entry =
    | Value of Tar.Header.t * int64
    | Dict of Tar.Header.t * entry StringMap.t

  type t = {
    b: BLOCK.t;
    mutable map: entry;
    (** offset in bytes *)
    mutable end_of_archive: int64;
    info: Mirage_block.info;
    write_lock : Lwt_mutex.t;
  }

  type key = Mirage_kv.Key.t

  type error = [ Mirage_kv.error | `Block of BLOCK.error ]

  let pp_error ppf = function
    | #Mirage_kv.error as e -> Mirage_kv.pp_error ppf e
    | `Block b -> BLOCK.pp_error ppf b

  let read t sector_start buffers =
    Lwt_result.map_error (fun e -> `Block e)
      (BLOCK.read t.b sector_start buffers)

  let get_node t key =
    let rec find e = function
      | [] -> Ok e
      | hd::tl -> match e with
        | Value _ -> Error (`Dictionary_expected key)
        | Dict (_, m) -> match StringMap.find_opt hd m with
          | Some e -> find e tl
          | None -> Error (`Not_found key)
    in
    find t (Mirage_kv.Key.segments key)

  let exists t key =
    let r = match get_node t.map key with
      | Ok (Value _) -> Ok (Some `Value)
      | Ok (Dict _) -> Ok (Some `Dictionary)
      | Error (`Not_found _) -> Ok None
      | Error e -> Error e
    in
    Lwt.return r

  let size t key =
    let r = match get_node t.map key with
      | Ok (Value (e, _)) -> Ok (Optint.Int63.of_int64 e.file_size)
      | Ok (Dict (e, _)) -> Ok (Optint.Int63.of_int64 e.file_size)
      | Error e -> Error e
    in
    Lwt.return r

  let read_data info b offset buffer len =
    assert(len <= 512);
    (* Tar assumes 512 byte sectors, but BLOCK might have 4096 byte sectors for example *)
    let sector_size = info.Mirage_block.sector_size in
    let sector' = Int64.(div offset (of_int sector_size)) in
    let sector_aligned_len =
      if len mod sector_size == 0 then
        len
      else
        len + (sector_size - len mod sector_size)
    in
    let tmp = Cstruct.create sector_aligned_len in
    BLOCK.read b sector' [ tmp ] >>= function
    | Error e ->
      Lwt.return (Error (`Msg
                           (Format.asprintf "Failed to read sector %Ld from block device: %a" sector'
                              BLOCK.pp_error e)))
    | Ok () ->
      (* If the BLOCK sector size is big, then we need to select the 512 bytes we want *)
      let offset_in_cs = Int64.(to_int (sub offset (mul sector' (of_int sector_size)))) in
      Cstruct.blit_to_bytes tmp offset_in_cs buffer 0 len;
      Lwt.return (Ok ())

  let fold info b f init =
    let open Lwt_result.Infix in
    let rec go t offset ?global ?data acc =
      (match data with
       | None ->
         let buf = Bytes.make Tar.Header.length '\000' in
         read_data info b offset buf Tar.Header.length >|= fun () ->
         Int64.(add offset (of_int Tar.Header.length)), Bytes.unsafe_to_string buf
       | Some data ->
         Lwt.return (Ok (offset, data))) >>= fun (offset, data) ->
      match Tar.decode t data with
      | Ok (t, Some `Header hdr, g) ->
        let global = Option.fold ~none:global ~some:(fun g -> Some g) g in
        f offset ?global hdr acc >>= fun acc' ->
        let off' =
          Int64.(add offset (add hdr.Tar.Header.file_size
                               (of_int (Tar.Header.compute_zero_padding_length hdr))))
        in
        go t off' ?global acc'
      | Ok (t, Some `Skip n, g) ->
        let global = Option.fold ~none:global ~some:(fun g -> Some g) g in
        let off' = Int64.add offset n in
        go t off' ?global acc
      | Ok (t, Some `Read n, g) ->
        let global = Option.fold ~none:global ~some:(fun g -> Some g) g in
        if Int64.of_int Sys.max_string_length < n then
          Lwt.return (Error (`Msg "read exceeds string size"))
        else
          let n' = Int64.to_int n in
          let buf = Bytes.make n' '\000' in
          read_data info b offset buf n' >>= fun () ->
          let data = Bytes.unsafe_to_string buf in
          let off' = Int64.add offset n in
          go t off' ?global ~data acc
      | Ok (t, None, g) ->
        let global = Option.fold ~none:global ~some:(fun g -> Some g) g in
        go t offset ?global acc
      | Error `Eof -> Lwt.return (Ok acc)
      | Error `Fatal _ as e -> Lwt.return e
    in
    go (Tar.decode_state ()) 0L init

  (* [read_partial_sector t sector_start ~offset ~length dst]
     reads a single sector and blits [length] bytes from [offset] into [dst]
     with the same offset. *)
  let read_partial_sector t sector_start ~offset ~length dst =
    assert Int64.(add offset length <= of_int t.info.sector_size);
    let length = Int64.to_int length and offset = Int64.to_int offset in
    assert (Cstruct.length dst >= t.info.sector_size);
    if length = 0 then Lwt_result.return () else
    let ( >>>= ) = Lwt_result.bind in
    let src = Cstruct.create t.info.sector_size in
    read t sector_start [ src ] >>>= fun () ->
    Cstruct.blit src offset dst offset length;
    Lwt_result.return ()

  let get_partial t key ~offset ~length =
    match get_node t.map key with
    | Error e -> Lwt.return (Error e)
    | Ok (Dict _) -> Lwt.return (Error (`Value_expected key))
    | Ok (Value (hdr, start_block)) ->
      let open Int64 in
      let offset = Optint.Int63.to_int64 offset in
      let sector_size = of_int t.info.Mirage_block.sector_size in
      (* Compute the unaligned data we need to read *)
      let start_bytes =
        let sec = mul start_block 512L in
        add sec offset
      in
      let length_bytes =
        min (sub hdr.Tar.Header.file_size offset)
          (of_int length)
      in
      if length_bytes < 0L then
        Lwt.return (Ok "")
      else
        let end_bytes = add start_bytes length_bytes in
        (* Compute the starting sector and ending sector (rounding down then up) *)
        let start_sector, start_padding =
          div start_bytes sector_size, rem start_bytes sector_size
        in
        let end_sector = div end_bytes sector_size in
        let n_sectors = succ (sub end_sector start_sector) in
        let buf = Cstruct.create (to_int (mul n_sectors sector_size)) in
        (* XXX: this is to work around limitations in some block implementations *)
        let tmps =
          let sec_size_int = to_int sector_size in
          List.init (to_int n_sectors)
            (fun sec -> Cstruct.sub buf (sec * sec_size_int) sec_size_int)
        in
        read t start_sector tmps >|= function
        | Error _ as e -> e
        | Ok () ->
          let buf =
            Cstruct.sub buf (to_int start_padding) (to_int length_bytes)
          in
          Ok (Cstruct.to_string buf)

  let get t key =
    get_partial t key ~offset:Optint.Int63.zero ~length:max_int

  let list t key =
    let r = match get_node t.map key with
      | Ok (Dict (_, m)) ->
        Ok (StringMap.fold (fun sub value acc ->
            let key = Mirage_kv.Key.add key sub in
            match value with
            | Dict _ -> (key, `Dictionary) :: acc
            | Value _ -> (key, `Value) :: acc)
            m [])
      | Ok (Value _) -> Error (`Dictionary_expected key)
      | Error e -> Error e
    in
    Lwt.return r

  let to_ptime hdr =
    Option.value ~default:Ptime.epoch
      (Ptime.of_float_s (Int64.to_float hdr.Tar.Header.mod_time))

  let last_modified t key =
    let r = match get_node t.map key with
      | Ok (Dict (hdr, _)) -> Ok (to_ptime hdr)
      | Ok (Value (hdr, _)) -> Ok (to_ptime hdr)
      | Error e -> Error e
    in
    Lwt.return r

  let digest t key =
    get t key >|= function
    | Error e -> Error e
    | Ok data -> Ok (Digest.string data)

  (* Compare filenames without a leading / or ./ *)
  let trim_slash x =
    let startswith prefix x =
      let prefix' = String.length prefix in
      let x' = String.length x in
      x' >= prefix' && (String.sub x 0 prefix' = prefix) in
    if startswith "./" x
    then String.sub x 2 (String.length x - 2)
    else if startswith "/" x
    then String.sub x 1 (String.length x - 1)
    else x

  let is_dict filename =
    String.get filename (pred (String.length filename)) = '/'

  let insert map key value =
    let rec go m = function
      | [] -> assert false
      | [hd] -> StringMap.add hd value m
      | hd::tl ->
        let hdr, m' = match StringMap.find_opt hd m with
          | None -> Tar.Header.make hd 0L, StringMap.empty
          | Some (Value _) -> assert false
          | Some (Dict (hdr, m)) -> hdr, m
        in
        let m'' = go m' tl in
        StringMap.add hd (Dict (hdr, m'')) m
    in
    go map (Mirage_kv.Key.segments key)

  let remove map key =
    let rec go m = function
      | [] -> assert false
      | [hd] -> StringMap.remove hd m
      | hd::tl ->
        let hdr, m' = match StringMap.find_opt hd m with
          | None -> Tar.Header.make hd 0L, StringMap.empty
          | Some (Value _) -> assert false
          | Some (Dict (hdr, m)) -> hdr, m
        in
        let m'' = go m' tl in
        if StringMap.is_empty m'' then
          StringMap.remove hd m
        else
          StringMap.add hd (Dict (hdr, m'')) m
    in
    go map (Mirage_kv.Key.segments key)

  let connect b =
    BLOCK.get_info b >>= fun info ->
    let ssize = info.Mirage_block.sector_size in
    if ssize mod 512 <> 0 || ssize < 512 then
      invalid_arg "Sector size needs to be >= 512 and a multiple of 512";
    let f offset ?global:_ hdr (_, map) =
      let filename = trim_slash hdr.Tar.Header.file_name in
      let map =
        if filename = "" then
          map
        else
          let data_tar_offset = Int64.(div offset (of_int Tar.Header.length)) in
          let v_or_d =
            if is_dict filename then
              Dict (hdr, StringMap.empty)
            else
              Value (hdr, data_tar_offset)
          in
          insert map (Mirage_kv.Key.v filename) v_or_d
      in
      let eof = Int64.(add offset
                         (add hdr.Tar.Header.file_size
                            (of_int (Tar.Header.compute_zero_padding_length hdr))))
      in
      Lwt.return (Ok (eof, map))
    in
    fold info b f (0L, StringMap.empty) >>= function
    | Error `Fatal e ->
      Format.kasprintf failwith "Fatal error reading archive: %a" Tar.pp_error e
    | Error `Msg msg ->
      Format.kasprintf failwith "Error reading archive: %s" msg
    | Ok (end_of_archive, map) ->
      let end_of_archive = Int64.(add end_of_archive (of_int (2 * Tar.Header.length))) in
      let map = Dict (Tar.Header.make "/" 0L, map) in
      let write_lock = Lwt_mutex.create () in
      Lwt.return ({ b; map; info; end_of_archive; write_lock })

  let disconnect _ = Lwt.return_unit

end


module Make_KV_RW (BLOCK : Mirage_block.S) = struct

  include Make_KV_RO(BLOCK)

  type write_error = [
    | `Block of BLOCK.error
    | `Block_write of BLOCK.write_error
    | Mirage_kv.write_error
    | `Entry_already_exists
    | `Path_segment_is_a_value
    | `Append_only
    | `Msg of string ]

  let pp_write_error ppf = function
   | `Block e -> Fmt.pf ppf "read error while writing: %a" BLOCK.pp_error e
   | `Block_write e -> BLOCK.pp_write_error ppf e
   | #Mirage_kv.write_error as e -> Mirage_kv.pp_write_error ppf e
   | `Entry_already_exists -> Fmt.string ppf "entry already exists"
   | `Path_segment_is_a_value -> Fmt.string ppf "path segment is a value"
   | `Append_only -> Fmt.string ppf "append only"
   | `Msg msg -> Fmt.pf ppf "writing tar header failed: %s" msg

  let write t sector_start buffers =
    Lwt_result.map_error (fun e -> `Block_write e)
      (BLOCK.write t.b sector_start buffers)

  let free t =
    Int64.(sub (mul (of_int t.info.sector_size) t.info.size_sectors)
             t.end_of_archive)

  let is_safe_to_set t key =
    let rec find e path =
      match e, path with
      | (Value _ | Dict _), [] -> Error `Entry_already_exists
      | Value _, _hd :: _tl -> Error `Path_segment_is_a_value
      | Dict (_, m), hd :: tl ->
        match StringMap.find_opt hd m with
        | Some e -> find e tl
        | None ->
          (* if either (part of) the path or the file doesn't exist we're good *)
          Ok ()
    in
    find t.map (Mirage_kv.Key.segments key)

  let header_of_key ?last_modified key len =
    let mod_time =
      match last_modified with
      | Some mod_time -> Int64.of_float (Ptime.to_float_s mod_time)
      | None ->
        let ptime = Mirage_ptime.now () in
        Int64.of_float (Ptime.to_float_s ptime)
    in
    Tar.Header.make ~mod_time (Mirage_kv.Key.to_string key) (Int64.of_int len)

  (* [space_needed header] is the number of bytes necessary for the data part including padding *)
  let space_needed header =
    let data_size = header.Tar.Header.file_size in
    let padding_size = Tar.Header.compute_zero_padding_length header in
    Int64.(add (of_int padding_size) data_size)

  let update_insert map key hdr offset =
    match map with
    | Value _ ->
      (* if the root is a value we have done something very wrong. This should
         be catched by [is_safe_to_set]. *)
      assert false
    | Dict (root, map) ->
      (* [insert] may raise if [key] is [empty]. However, [is_safe_to_set]
         should catch that since [empty] always exists as a dict (root). *)
      let map = insert map key (Value (hdr, offset)) in
      Dict (root, map)

  let update_remove map key =
    match map with
    | Value _ ->
      (* if the root is a value we have done something very wrong. This should
         be catched by [is_safe_to_set]. *)
      assert false
    | Dict (root, map) ->
      (* [remove] may raise if [key] is [empty]. *)
      let map = remove map key in
      Dict (root, map)

  let write_data info b offset buffer =
    assert (String.length buffer <= Tar.Header.length);
    let sector_size = info.Mirage_block.sector_size in
    let sector = Int64.(div offset (of_int sector_size)) in
    let block = Cstruct.create sector_size in
    BLOCK.read b sector [ block ] >>= function
    | Error e -> Lwt.return (Error (`Block e))
    | Ok () ->
      let start_offset = Int64.to_int offset mod sector_size in
      Cstruct.blit_from_string buffer 0 block start_offset (String.length buffer);
      BLOCK.write b sector [ block ] >>= function
      | Error e -> Lwt.return (Error (`Block_write e))
      | Ok () -> Lwt.return (Ok ())

  let write_header (t : t) header_start_bytes hdr =
    (* it is important we write at level [Ustar] at most as we assume the
       header(s) taking up exactly 512 bytes. With [GNU] level extra blocks
       may be used for long names. *)
    let open Lwt_result.Infix in
    Lwt_result.lift (Tar.encode_header ~level:Tar.Header.Ustar hdr) >>= fun datas ->
    Lwt_list.fold_left_s (fun acc buf ->
        Lwt_result.lift acc >>= fun off' ->
        write_data t.info t.b off' buf >|= fun () ->
        Int64.(add off' (of_int (String.length buf))))
      (Ok header_start_bytes) datas

  let set t key data =
    Lwt_mutex.with_lock t.write_lock (fun () ->
        let data = Cstruct.of_string data in
        let ( >>>= ) = Lwt_result.bind in
        let r =
          let ( let* ) = Result.bind in
          let* () = is_safe_to_set t key in
          let hdr = header_of_key key (Cstruct.length data) in
          let space_needed = space_needed hdr in
          let* () =
            if free t >= Int64.(add space_needed (of_int Tar.Header.length)) then
              Ok ()
            else
              Error `No_space
          in
          Ok (hdr, space_needed)
        in
        Lwt.return r >>>= fun (hdr, space_needed) ->
        let open Int64 in
        let sector_size = of_int t.info.Mirage_block.sector_size in
        let data_start_bytes = sub t.end_of_archive (of_int Tar.Header.length) in
        let header_start_bytes = sub data_start_bytes (of_int Tar.Header.length) in
        let sentinel = mul 2L (of_int Tar.Header.length) in
        let end_bytes = add data_start_bytes (add space_needed sentinel) in
        (* Compute the starting sector and ending sector *)
        let data_start_sector, data_start_sector_offset =
          div data_start_bytes sector_size,
          rem data_start_bytes sector_size
        in
        let end_sector = div (add end_bytes (pred sector_size)) sector_size in
        let last_sector_offset = rem end_bytes sector_size in
        let pad = Tar.Header.compute_zero_padding_length hdr in

        let data =
          let slack =
            if last_sector_offset = 0L then
              0
            else
              to_int (sub sector_size last_sector_offset)
          in
          Cstruct.concat [
            Cstruct.create (to_int data_start_sector_offset);
            data;
            Cstruct.create (pad + to_int sentinel + slack);
          ]
        in
        (* [data] is always at least one sector as the sentinel is always present *)
        let first_sector, remaining_sectors = Cstruct.split data t.info.sector_size in
        let last_sector =
          (* sub on whole [data] as the first sector and last sector might be the same *)
          Cstruct.sub data
            (Cstruct.length data - t.info.sector_size)
            t.info.sector_size
        in
        (* blit in slack at the end if needed *)
        begin if last_sector_offset = 0L then Lwt_result.return () else
          read_partial_sector t (pred end_sector) last_sector
            ~offset:last_sector_offset
            ~length:(sub sector_size last_sector_offset)
        end >>>= fun () ->
        (* to write robustly as we can:
           - we write sectors 2..n,
           - then the header,
           - then we blit the first (data) sector as it may contain the header,
           - finally we write the first (data) sector which contains the first tar data block.
        *)
        let remaining_sectors =
          (* XXX: this is to work around limitations in some block implementations *)
          List.init (Cstruct.length remaining_sectors / to_int sector_size)
            (fun sector ->
               Cstruct.sub remaining_sectors
                 (sector * to_int sector_size)
                 (to_int sector_size))
        in
        write t (succ data_start_sector) remaining_sectors >>>= fun () ->
        (* finally write header and first block *)
        write_header t header_start_bytes hdr >>>= fun _new_offset ->
        (* read in slack at beginning which could include the header *)
        read_partial_sector t data_start_sector first_sector
          ~offset:0L ~length:data_start_sector_offset >>>= fun () ->
        write t data_start_sector [ first_sector ] >>>= fun () ->
        let tar_offset = Int64.div data_start_bytes (of_int Tar.Header.length) in
        t.end_of_archive <- end_bytes;
        t.map <- update_insert t.map key hdr tar_offset;
        Lwt.return (Ok ()))

  let remove t key =
    let ( >>>= ) = Lwt_result.bind in
    Lwt_mutex.with_lock t.write_lock (fun () ->
        match get_node t.map key with
        | Error e -> Lwt.return (Error e)
        | Ok (Dict _) -> Lwt.return (Error (`Value_expected key))
        | Ok (Value (hdr, start_block)) ->
          (* We can only easily remove if the key is the very last entry. *)
          let open Int64 in
          let end_data_bytes =
            add (mul start_block 512L)
              (add hdr.file_size (of_int (Tar.Header.compute_zero_padding_length hdr)))
          in
          if equal end_data_bytes (sub t.end_of_archive 1024L) then begin
            t.map <- update_remove t.map key;
            let start_bytes = mul (pred start_block) 512L in
            let sector_size = of_int t.info.sector_size in
            let start_sector, start_sector_offset =
              div start_bytes sector_size, rem start_bytes sector_size
            in
            let end_bytes = add start_bytes 1024L in
            let end_sector, last_sector_offset =
              div (add end_bytes (pred sector_size)) sector_size, rem end_bytes sector_size
            in
            let buf = Cstruct.create (to_int (mul sector_size (sub end_sector start_sector))) in
            let first_sector = Cstruct.sub buf 0 t.info.sector_size in
            let last_sector =
              Cstruct.sub buf (Cstruct.length buf - t.info.sector_size) t.info.sector_size
            in
            read_partial_sector t start_sector_offset first_sector
              ~offset:0L ~length:start_sector_offset >>>= fun () ->
            begin if last_sector_offset = 0L then Lwt_result.return () else
                read_partial_sector t (pred end_sector) last_sector
                  ~offset:last_sector_offset ~length:(sub sector_size last_sector_offset)
            end >>>= fun () ->
            (* To remove as robustly as possible we first zero the second
               sector (if applicable). *)
            begin if Cstruct.length buf > t.info.sector_size then
                write t (succ start_sector)
                  [Cstruct.sub buf t.info.sector_size t.info.sector_size] >>>= fun () ->
                write t start_sector [Cstruct.sub buf 0 t.info.sector_size]
              else
                write t start_sector [buf]
            end >>>= fun () ->
            t.end_of_archive <- end_bytes;
            Lwt_result.return ()
          end else
            Lwt.return (Error `Append_only))

  let rename t ~source ~dest =
    let ( >>>= ) = Lwt_result.bind in
    Lwt_mutex.with_lock t.write_lock (fun () ->
        Lwt.return (is_safe_to_set t dest) >>>= fun () ->
        Lwt.return begin match get_node t.map source with
          | Ok Value (hdr, data_offset) -> Ok (hdr, data_offset)
          | Ok Dict _ -> Error `Append_only
          | Error _ as e -> e
        end >>>= fun (hdr, data_offset) ->
        let hdr = { hdr with Tar.Header.file_name = Mirage_kv.Key.to_string dest } in
        write_header t Int64.(sub (mul data_offset (of_int Tar.Header.length)) (of_int Tar.Header.length)) hdr >>>= fun _new_off ->
        t.map <- update_insert t.map dest hdr data_offset;
        t.map <- update_remove t.map source;
        Lwt_result.return ())

  let set_partial t key ~offset data =
    let ( >>>= ) = Lwt_result.bind in
    Lwt_mutex.with_lock t.write_lock (fun () ->
        if Optint.Int63.(compare offset zero < 0) then
          invalid_arg "Tar_mirage.set_partial: negative offset";
        Lwt.return begin match get_node t.map key with
          | Ok Value (hdr, data_offset) ->
            Ok (hdr, Int64.(mul data_offset (of_int Tar.Header.length)))
          | Ok Dict _ -> Error `Path_segment_is_a_value
          | Error _ as e -> e
        end >>>= fun (hdr, data_offset) ->
        let offset = Optint.Int63.to_int64 offset in
        let open Int64 in
        let end_bytes = add offset (of_int (String.length data)) in
        begin if end_bytes > hdr.file_size then
            Lwt_result.fail `Append_only
          else
            Lwt_result.return ()
        end >>>= fun () ->
        (* compute the offsets into the archive *)
        let end_bytes = add data_offset end_bytes in
        let start_bytes = add data_offset offset in
        let sector_size = of_int t.info.sector_size in
        let start_sector_offset = rem start_bytes sector_size in
        let end_sector = div (add end_bytes (pred sector_size)) sector_size in
        let last_sector_offset = rem end_bytes sector_size in
        (* allocate a buffer for what we need to write, and blit in data and slack
           at first and last sector. *)
        let data' =
          let len =
            add start_sector_offset
              (add (of_int (String.length data))
                 (if last_sector_offset = 0L then
                    0L
                  else
                    sub sector_size last_sector_offset))
          in
          Cstruct.create (to_int len)
        in
        Cstruct.blit_from_string data 0 data'
          (to_int start_sector_offset) (String.length data);
        read_partial_sector t (div start_bytes sector_size) data'
          ~offset:0L ~length:start_sector_offset >>>= fun () ->
        let last_sector =
          Cstruct.sub data' (Cstruct.length data' - t.info.sector_size)
            t.info.sector_size
        in
        begin if last_sector_offset = 0L then Lwt_result.return () else
          read_partial_sector t (pred end_sector) last_sector
            ~offset:last_sector_offset
            ~length:(sub sector_size last_sector_offset)
        end >>>= fun () ->
        (* XXX: this is to work around limitations in some block implementations *)
        let data' =
          List.init (Cstruct.length data' / t.info.sector_size)
            (fun sector ->
               Cstruct.sub data' (sector * t.info.sector_size) t.info.sector_size)
        in
        write t (div start_bytes sector_size) data')

  let allocate t key ?last_modified size =
    Lwt_mutex.with_lock t.write_lock (fun () ->
        let ( >>>= ) = Lwt_result.bind in
        let r =
          let ( let* ) = Result.bind in
          (* XXX: map `Entry_already_exists to `Append_only ?! *)
          let* () = is_safe_to_set t key in
          let hdr = header_of_key ?last_modified key (Optint.Int63.to_int size) in
          let space_needed = space_needed hdr in
          let* () =
            if free t >= Int64.(add space_needed (of_int Tar.Header.length)) then
              Ok ()
            else
              Error `No_space
          in
          Ok (hdr, space_needed)
        in
        Lwt.return r >>>= fun (hdr, space_needed) ->
        let open Int64 in
        let sector_size = of_int t.info.Mirage_block.sector_size in
        let header_start_bytes =
          sub t.end_of_archive (of_int (2 * Tar.Header.length))
        in
        (* we don't have to zero the block before end_of_archive as it is
           already zero due to the terminating double zero block (sentinel) *)
        let to_zero_start_bytes = t.end_of_archive in
        (* space_needed - 1 block + sentinel = space_needed + 1 block *)
        let end_bytes = add t.end_of_archive
            (add space_needed (of_int Tar.Header.length)) in
        (* Compute the starting sector and ending sector *)
        let to_zero_start_sector, to_zero_start_sector_offset =
          div to_zero_start_bytes sector_size,
          rem to_zero_start_bytes sector_size
        in
        let last_sector_offset = rem end_bytes sector_size in
        let end_sector = div (add end_bytes (pred sector_size)) sector_size in
        (* [num_to_zero_sectors] is at least 1 as we need to write at least one
           zero block of the new sentinel. *)
        let num_to_zero_sectors = to_int (sub end_sector to_zero_start_sector) in
        let zero_sector = Cstruct.create t.info.Mirage_block.sector_size in
        let data = Array.init num_to_zero_sectors (fun _ -> zero_sector) in
        let nonzero_sector c =
          (* we allocate a new buffer if [c] is [zero_sector], otherwise we can
             reuse it in the case first and last sectors are the same. *)
          if c != zero_sector then c else Cstruct.create (Cstruct.length c)
        in
        (* Read slack at start and end sector(s) *)
        let () = data.(0) <- nonzero_sector data.(0) in
        read_partial_sector t to_zero_start_sector data.(0)
          ~offset:0L
          ~length:to_zero_start_sector_offset
        >>>= fun () ->
        let last = nonzero_sector data.(num_to_zero_sectors - 1) in
        let () = data.(num_to_zero_sectors - 1 ) <- last in
        begin if last_sector_offset = 0L then Lwt_result.return () else
          read_partial_sector t (pred end_sector) last
            ~offset:last_sector_offset
            ~length:(sub sector_size last_sector_offset)
        end >>>= fun () ->
        write t to_zero_start_sector (Array.to_list data) >>>= fun () ->
        write_header t header_start_bytes hdr >>>= fun _new_offset ->
        let tar_offset = div (sub t.end_of_archive (of_int Tar.Header.length)) (of_int Tar.Header.length) in
        t.end_of_archive <- end_bytes;
        t.map <- update_insert t.map key hdr tar_offset;
        Lwt.return (Ok ()))
end
