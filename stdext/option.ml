include Stdlib.Option

module O = struct
  let ( >>= ) = bind
  let ( >>| ) opt f = map f opt
  let ( let* ) = ( >>= )
  let ( let+ ) = ( >>| )
end

let map_default ~f ~default = function None -> default | Some x -> f x
