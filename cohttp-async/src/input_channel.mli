open Async

type t

val create : ?buf_len:int -> Reader.t -> t
val read_line_opt : t -> string option Deferred.t
val read : t -> int -> string Deferred.t
val refill : t -> [ `Eof | `Ok ] Deferred.t
val with_input_buffer : t -> f:(string -> pos:int -> len:int -> 'a * int) -> 'a
val is_closed : t -> bool
val close : t -> unit Deferred.t
val close_finished : t -> unit Deferred.t
val to_reader : Base.Info.t -> t -> Reader.t Deferred.t
