let () =
  let r1 = C.Functions_unlocked.add4 0 in
  let r2 = C.Functions_sequential.add2 r1 in
  Printf.printf "%d\n" r2
