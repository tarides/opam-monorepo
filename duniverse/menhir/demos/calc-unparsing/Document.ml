type document = {
  fragile_left:  bool;
  fragile_right: bool;
  content:       PPrint.document;
}

let[@inline] map f d =
  { d with content = f d.content }

let fragile content =
  let fragile_left, fragile_right = true, true in
  { fragile_left; fragile_right; content }

let robust content =
  let fragile_left, fragile_right = false, false in
  { fragile_left; fragile_right; content }

let empty =
  robust PPrint.empty

let (^^) d1 d2 =
  let (^^), space = PPrint.((^^), space) in
  let fragile_left, fragile_right = d1.fragile_left, d2.fragile_right in
  let content =
    if d1.fragile_right || d2.fragile_left then
      (* A space is needed in the middle. *)
      (* A breakable space could be used. *)
      d1.content ^^ space ^^ d2.content
    else
      d1.content ^^ d2.content
  in
  { fragile_left; fragile_right; content }

let[@inline] group d =
  map PPrint.group d

let[@inline] nest i d =
  map (PPrint.nest i) d

let space =
  robust PPrint.space

let[@inline] break i =
  robust (PPrint.break i)

let string s =
  robust (PPrint.string s)

let int i =
  robust (PPrint.OCaml.int i)

module ToChannel = struct
  let pretty r w c d =
    PPrint.ToChannel.pretty r w c d.content
end

module ToBuffer = struct
  let pretty r w b d =
    PPrint.ToBuffer.pretty r w b d.content
end
