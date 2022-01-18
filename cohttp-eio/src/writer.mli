type t

val create : < Eio.Flow.close ; Eio.Flow.two_way > -> t
val sink : t -> Eio.Flow.sink
val write_string : t -> string -> unit
val wakeup : t -> unit
val run : t -> unit
val close : t -> unit
