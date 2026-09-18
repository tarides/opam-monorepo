open Monolith

let int = le Int.max_int

let int32 =
  let gen_random =
    let open Int32 in
    let bits () = of_int (Gen.bits ()) in
    fun () -> logxor (bits ()) (shift_left (bits ()) 30)
  in
  let pos = easily_constructible gen_random PPrint.OCaml.int32 in
  let neg = deconstructible PPrint.OCaml.int32 in
  ifpol pos neg

let float = deconstructible PPrint.OCaml.float
let string = deconstructible PPrint.string

module type INTEGER = module type of Optint.Int63.Boxed

module Fuzz_integer_equivalence (Reference : INTEGER) (Candidate : INTEGER) =
struct
  module R = Reference
  module C = Candidate

  let encoded_string : (string, string) spec =
    let check_valid r c =
      let exception Incorrect_length of string in
      let exception Different of string * string in
      if not (String.length c = R.encoded_size) then raise (Incorrect_length c);
      if not (String.equal r c) then raise (Different (r, c))
    in
    declare_abstract_type
      ~check:(fun r -> (check_valid r, document (PPrint.string r)))
      ()

  module Wrap = struct
    let pp f x =
      f Format.str_formatter x;
      Format.flush_str_formatter ()

    let encode f x =
      let buf = Bytes.create R.encoded_size in
      f buf ~off:0 x;
      Bytes.unsafe_to_string buf

    let decode f s = f s ~off:0
  end

  let run t fuel =
    let endo = t ^> t in
    let binop = t ^> t ^> t in
    let binop_exn = t ^> t ^!> t in

    declare "zero" t R.zero C.zero;
    declare "one" t R.one C.one;
    declare "minus_one" t R.minus_one C.minus_one;
    declare "max_int" t R.max_int C.max_int;
    declare "min_int" t R.min_int C.min_int;

    declare "succ" endo R.succ C.succ;
    declare "pred" endo R.pred C.pred;
    declare "abs" endo R.abs C.abs;
    declare "neg" endo R.neg C.neg;
    declare "add" binop R.add C.add;
    declare "sub" binop R.sub C.sub;
    declare "mul" binop R.mul C.mul;
    declare "div" binop_exn R.div C.div;
    declare "rem" binop_exn R.rem C.rem;
    declare "logand" binop R.logand C.logand;
    declare "logor" binop R.logor C.logor;
    declare "logxor" binop R.logxor C.logxor;
    declare "lognot" endo R.lognot C.lognot;
    declare "shift_left" (t ^> int ^> t) R.shift_left C.shift_left;
    declare "shift_right" (t ^> int ^> t) R.shift_right C.shift_right;
    declare "shift_right_logical"
      (t ^> int ^> t)
      R.shift_right_logical C.shift_right_logical;

    declare "compare" (t ^> t ^> int) R.compare C.compare;
    declare "equal" (t ^> t ^> bool) R.equal C.equal;

    declare "of_int" (int ^> t) R.of_int C.of_int;
    declare "to_int" (t ^> int) R.to_int C.to_int;
    declare "of_int32" (int32 ^> t) R.of_int32 C.of_int32;
    declare "to_int32" (t ^> int32) R.to_int32 C.to_int32;
    declare "to_float" (t ^> float) R.to_float C.to_float;
    declare "to_string" (t ^> string) R.to_string C.to_string;

    declare "pp" (t ^> string) (Wrap.pp R.pp) (Wrap.pp C.pp);
    declare "encoded_size" int R.encoded_size C.encoded_size;
    declare "encode" (t ^> encoded_string) (Wrap.encode R.encode)
      (Wrap.encode C.encode);
    declare "decode" (encoded_string ^> t) (Wrap.decode R.decode)
      (Wrap.decode C.decode);

    main fuel
end

module Reference = Optint.Int63
module Candidate = Optint.Int63.Boxed
module Int63_equiv = Fuzz_integer_equivalence (Reference) (Candidate)

let () =
  let t : (Reference.t, Candidate.t) spec = declare_abstract_type () in
  Int63_equiv.run t 5
