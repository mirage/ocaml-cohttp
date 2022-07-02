(* Bytebuffer is split into three regions using two separate indices that are used
   to support read and write operations.
   +--------------------+---------------------------+----------------------------+
   | Consumed Bytes     | Bytes available to read   | Empty space for writing    |
   +--------------------+---------------------------+----------------------------+
   |     0 <=       pos_read         <=          pos_fill              <= capacity

   Consumed Bytes: This is content that's already consumed via a get/read operation.
   This space can be safely reclaimed.

   Bytes available to read: This is the actual content that will be surfaced to users via
   get/read operations on the bytebuffer.

   Empty space for writing: This is space that will be filled by any set/write operations
   on the bytebuffer.
*)
type t

val create : int -> t
val unsafe_buf : t -> Bytes.t
val pos : t -> int
val compact : t -> unit
val length : t -> int
val drop : t -> int -> unit
val to_string : t -> string

module Make (IO : sig
  type 'a t

  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end) (Refill : sig
  type src

  val refill : src -> bytes -> pos:int -> len:int -> [ `Ok of int | `Eof ] IO.t
end) : sig
  val refill : t -> Refill.src -> [ `Ok | `Eof ] IO.t
  val read_line : t -> Refill.src -> string option IO.t
  val read : t -> Refill.src -> int -> string IO.t
end
