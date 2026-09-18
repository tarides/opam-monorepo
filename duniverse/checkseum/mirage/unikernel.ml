module Make (Console : Mirage_console.S) = struct
  let log console fmt = Format.kasprintf (Console.log console) fmt

  let start console =
    let crc = Checkseum.Crc32.(digest_string "Hello World!" 0 12 default) in
    log console "%a" Checkseum.Crc32.pp crc
end
