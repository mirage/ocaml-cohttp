type name = string
type value = string
type 'a header = ..

type 'a header +=
  | Content_length : int header
  | Transfer_encoding : [ `chunked | `compress | `deflate | `gzip ] list header
  | Date : Ptime.t header

module Order : sig
  type (_, _) t = Lt : ('a, 'b) t | Eq : ('a, 'a) t | Gt : ('a, 'b) t
end

type (_, _) eq = Eq : ('a, 'a) eq

type header_ext = {
  decode : 'a. 'a header -> string -> 'a;
  compare : 'a 'b. 'a header -> 'b header -> ('a, 'b) Order.t;
}

exception Unrecognized_header of string
exception Duplicate_header of string

val extend : 'a header -> header_ext -> unit
(** [extend t header] extends [t] with [header].

    @raise Duplicate_header if [header] already exists *)

val compare : 'a header -> 'b header -> ('a, 'b) Order.t
val decode : 'a header -> string -> 'a lazy_t

val name_value : 'a header -> 'a -> name * value
(** [name_value hdr v] is [(name, value)] which represents a string value of
    [hdr] and [v] respectively. *)

module type HEADER = sig
  type 'a t = 'a header

  val compare : 'a t -> 'b t -> ('a, 'b) Order.t
  val decode : 'a t -> string -> 'a lazy_t
end

module type S = sig
  type t
  type 'a key

  val empty : t
  val add_string_val : 'a key -> string -> t -> t
  val add : 'a key -> 'a lazy_t -> t -> t
  val find : 'a key -> t -> 'a
  val find_opt : 'a key -> t -> 'a option
end

module Make (H : HEADER) : S
