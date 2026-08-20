type t = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type write = [ `Copy of t | `Passthrough of t ]

let buffer = function `Copy t | `Passthrough t -> t
let create len = Bigarray.Array1.create Bigarray.char Bigarray.c_layout len
let length = Bigarray.Array1.dim
let sub t ~off ~len = Bigarray.Array1.sub t off len

let check name ~size ~off ~len =
  if off < 0 || len < 0 || off > size - len then
    invalid_arg
      (Printf.sprintf "Cohttp.Bigstring.%s: off=%d len=%d out of bounds for %d"
         name off len size)

let blit src ~src_off dst ~dst_off ~len =
  Bigarray.Array1.blit (sub src ~off:src_off ~len) (sub dst ~off:dst_off ~len)

let blit_from_string s ~src_off t ~dst_off ~len =
  check "blit_from_string" ~size:(String.length s) ~off:src_off ~len;
  check "blit_from_string" ~size:(length t) ~off:dst_off ~len;
  for i = 0 to len - 1 do
    Bigarray.Array1.unsafe_set t (dst_off + i)
      (String.unsafe_get s (src_off + i))
  done

let sub_string t ~off ~len =
  check "sub_string" ~size:(length t) ~off ~len;
  String.init len (fun i -> Bigarray.Array1.unsafe_get t (off + i))
