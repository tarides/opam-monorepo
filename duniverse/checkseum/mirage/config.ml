open Mirage

let main = foreign "Unikernel.Make" (console @-> job)
let packages = [ package "checkseum" ]
let () = register ~packages "checkseum-test" [ main $ default_console ]
