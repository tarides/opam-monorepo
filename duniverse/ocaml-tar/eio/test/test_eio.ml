let ( / ) = Eio.Path.( / )
let ( let* ) = Result.map
let tar_eio_error = Alcotest.of_pp Tar_eio.pp_decode_error

let stat =
  let stat_equal (s1 : Eio.File.Stat.t) (s2 : Eio.File.Stat.t) =
    s1.kind = s2.kind
    && Optint.Int63.equal s1.size s2.size
    && Int.equal s1.perm s2.perm
  in
  Alcotest.testable Eio.File.Stat.pp stat_equal

let id =
  let i = ref 0 in
  fun () ->
    incr i;
    !i

let with_tmp_dir cwd fn =
  let tmp = cwd / Fmt.str "tmp-%i" (id ()) in
  Eio.Path.mkdirs ~perm:0o755 tmp;
  fn tmp

let extra_error : [ Tar_gz.error | Tar_eio.decode_error ] Alcotest.testable =
  let pp ppf = function
    | `Gz e -> Fmt.pf ppf "gz %s" e
    | `Eof -> Fmt.pf ppf "eof"
    | #Tar_eio.decode_error as e -> Tar_eio.pp_decode_error ppf e
  in
  Alcotest.of_pp pp

let test_e2e ?(filter_symlinks = false) ?(compress = false) cwd () =
  let filter =
    if filter_symlinks then fun (h : Tar.Header.t) ->
      h.link_indicator <> Tar.Header.Link.Symbolic
    else fun _ -> true
  in
  let test_tar = cwd / "test.tar" in
  with_tmp_dir cwd @@ fun tmp ->
  Eio.Path.save ~create:(`Or_truncate 0o755) (tmp / "hello.txt") "hello";
  Eio.Path.save ~create:(`Or_truncate 0o755) (tmp / "world.txt") "world";
  Eio.Path.symlink ~link_to:"hello.txt" (tmp / "hello-link.txt");
  Eio.Path.mkdirs ~perm:0o600 (tmp / "hello");
  let res =
    Eio.Path.with_open_out ~create:(`Or_truncate 0o755) test_tar @@ fun out ->
    Eio.Switch.run @@ fun sw ->
    let t = Tar_eio.create ~filter ~sw tmp in
    let t =
      if compress then Tar_gz.out_gzipped ~level:4 ~mtime:0l Gz.Unix t else t
    in
    Tar_eio.run t (Tar_eio.flow_of_file out)
  in
  Alcotest.(check (result unit tar_eio_error)) "successful create" (Ok ()) res;
  with_tmp_dir cwd @@ fun extract_dir ->
  let res =
    Eio.Switch.run @@ fun sw ->
    let t = Tar_eio.extract ~sw extract_dir in
    let t = if compress then Tar_gz.in_gzipped t else t in
    Eio.Path.with_open_out ~create:`Never test_tar @@ fun src ->
    Tar_eio.run t (Tar_eio.flow_of_file src)
  in
  Alcotest.(check (result unit extra_error)) "successful extract" (Ok ()) res;
  let hello1, hello2 =
    ( Eio.Path.stat ~follow:false (tmp / "hello.txt"),
      Eio.Path.stat ~follow:false (extract_dir / snd tmp / "hello.txt") )
  in
  Alcotest.check stat "same hello.txt" hello1 hello2;
  let hellod1, hellod2 =
    ( Eio.Path.stat ~follow:false (tmp / "hello"),
      Eio.Path.stat ~follow:false (extract_dir / snd tmp / "hello") )
  in
  Alcotest.check stat "same hello directory" hellod1 hellod2;
  if filter_symlinks then begin
    let symlink =
      try
        Some
          (Eio.Path.stat ~follow:false
             (extract_dir / snd tmp / "hello-link.txt"))
      with Eio.Io (Eio.Fs.E (Not_found _), _) -> None
    in
    Alcotest.(check (option stat)) "no symlink" None symlink
  end
  else begin
    let sym1, sym2 =
      ( Eio.Path.stat ~follow:false (tmp / "hello-link.txt"),
        Eio.Path.stat ~follow:false (extract_dir / snd tmp / "hello-link.txt")
      )
    in
    Alcotest.check stat "same hello-link.txt" sym1 sym2
  end

let simple_tests cwd =
  [
    ("e2e", `Quick, test_e2e cwd);
    ("e2e compression", `Quick, test_e2e ~compress:true cwd);
    ("e2e filter syms", `Quick, test_e2e ~filter_symlinks:true cwd);
  ]

let () =
  Eio_main.run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  Alcotest.run "tar-eio" [ ("simple", simple_tests cwd) ]
