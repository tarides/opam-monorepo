type t = int [@@immediate]

include Integer_interface.S with type t := t
