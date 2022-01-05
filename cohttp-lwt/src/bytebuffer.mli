type t
type refill = bytes -> pos:int -> len:int -> [ `Ok of int | `Eof ] Lwt.t

val create : int -> t
val unsafe_buf : t -> Bytes.t
val pos : t -> int
val compact : t -> unit
val length : t -> int
val drop : t -> int -> unit
val refill : t -> refill -> [ `Ok | `Eof ] Lwt.t
val to_string : t -> string
val read_line : t -> refill -> string option Lwt.t
val read : t -> refill -> int -> string Lwt.t
