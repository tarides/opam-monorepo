(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

module H = Hashtbl.Make(struct
  type t = string
  let equal = (=)
  let hash = Hashtbl.hash
end)

module Int = struct
  type t = int
  let compare = compare
end

module Make () = struct

  type t =
    int

  let equal (x : t) (y : t) = (x = y)
  let compare (x : t) (y : t) = x - y
  let hash (x : t) = x

  (* Set up a hash table, mapping strings to unique integers. *)
  let table = H.create 2048
  (* Set up a vector, mapping integers back to strings. *)
  let text = Vector.create()

  let import (s : string) : int =
    try
      H.find table s
    with Not_found ->
      let i = Vector.length text in
      H.add table s i;
      Vector.push text s;
      i

  let export (i : int) : string =
    Vector.get text i

  module Set = struct

    include Set.Make(Int)

    let import (ss : StringSet.t) : t =
      StringSet.fold (fun s accu ->
        add (import s) accu
      ) ss empty

    let export (ss : t) : StringSet.t =
      fold (fun s accu ->
        StringSet.add (export s) accu
      ) ss StringSet.empty

    let print ss =
      StringSet.print (export ss)

  end

  module Map = struct

    include Map.Make(Int)

    let domain m =
      fold (fun key _ accu -> Set.add key accu) m Set.empty

    let filter p m =
      fold (fun key v m ->
        if p key v then
          add key v m
        else
          m
      ) m empty

    let restrict domain m =
      filter (fun k _ -> Set.mem k domain) m

  end

end
