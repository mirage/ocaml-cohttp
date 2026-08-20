(** Bytes outside the OCaml heap.

    Spelled as the Bigarray type rather than taken from a library so that cohttp
    gains no dependency: it is the same type as [Bigstringaf.t], [Lwt_bytes.t]
    and Async's [Bigstring.t], so a caller holding any of them passes it
    directly. *)

type t = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type write = [ `Copy of t | `Passthrough of t ]
(** Whether the writer may hold on to a buffer being written.

    [`Copy] means the bytes are taken before the write returns, so the caller
    may reuse or modify the buffer straight after. [`Passthrough] permits the
    writer to send them out of the caller's buffer instead, which spares a copy
    but leaves the bytes borrowed until the write is flushed.

    It is a permission, not a promise: a backend with no way to write from
    foreign memory copies a [`Passthrough] buffer anyway. *)

val buffer : write -> t
(** The buffer either way, for a writer that copies regardless. *)

val create : int -> t
(** Uninitialised. *)

val length : t -> int

val sub : t -> off:int -> len:int -> t
(** A view of [len] bytes from [off], sharing the bytes rather than copying. *)

val blit : t -> src_off:int -> t -> dst_off:int -> len:int -> unit

val blit_from_string :
  string -> src_off:int -> t -> dst_off:int -> len:int -> unit

val sub_string : t -> off:int -> len:int -> string
(** Only for a caller whose result is a string anyway. *)

(** Every function above raises [Invalid_argument] on a range that is not within
    its argument. *)
