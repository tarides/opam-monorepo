open Configurator.V1

let main_c =
  {c|
#include <stddef.h>

int main() { size_t v = 0 ; return 0; }
  |c}

let () =
  main ~name:"stddef" @@ fun c ->
  let has_stddef = c_test c main_c in
  if has_stddef
  then Format.printf "(-DCHECKSEUM_STDDEF)"
  else Format.printf "(-DCHECKSEUM_NO_STDDEF)"
