type name = string
type value = string
type 'a header = ..

type 'a header +=
  | Content_length : int header
  | Transfer_encoding : [ `chunked | `compress | `deflate | `gzip ] list header
  | Date : Ptime.t header

type (_, _) eq = Eq : ('a, 'a) eq

type header_ext = {
  decode : 'a. 'a header -> string -> 'a lazy_t;
  equal : 'a 'b. 'a header -> 'b header -> ('a, 'b) eq option;
}

exception Unrecognized_header of string
exception Duplicate_header of string

val compare : 'a header -> 'b header -> ('a, 'b) Gmap.Order.t

val extend : 'a header -> header_ext -> unit
(** [extend t header] extends [t] with [header].

    @raise Duplicate_header if [header] already exists *)

val equal : 'a header -> 'b header -> ('a, 'b) eq option
val decode : 'a header -> string -> 'a lazy_t

val name_value : 'a header -> 'a -> name * value
(** [name_value hdr v] is [(name, value)] which represents a string value of
    [hdr] and [v] respectively. *)

module type S = sig
  type t
  type 'a key

  val empty : t
  val add_raw : 'a key -> string -> t -> t
  val get : 'a lazy_t key -> t -> 'a
end

module Make (K : Gmap.KEY) : S
