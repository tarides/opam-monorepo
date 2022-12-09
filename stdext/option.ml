include Stdlib.Option

module O = struct
  let ( >>= ) = bind
  let ( >>| ) opt f = map f opt
  let ( let* ) = ( >>= )
  let ( let+ ) = ( >>| )
end
