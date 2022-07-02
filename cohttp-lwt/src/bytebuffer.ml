include Cohttp.Private.Bytebuffer

include
  Cohttp.Private.Bytebuffer.Make
    (struct
      include Lwt

      let ( >>| ) x f = Lwt.map f x
    end)
    (struct
      type src = bytes -> pos:int -> len:int -> [ `Ok of int | `Eof ] Lwt.t

      let refill src = src
    end)
