type name = string
type value = string
type 'a t = ..

type 'a t +=
  | Content_length : int t
  | Transfer_encoding : [ `chunked | `compress | `deflate | `gzip ] list t
  | Date : Ptime.t t
  | Header : name -> value t

exception Unrecognized_header of string

val compare : 'a t -> 'b t -> ('a, 'b) Gmap.Order.t

val name_value : 'a t -> 'a -> name * value
(** [name_value hdr v] is a tuple of [(name, value)]. *)

val of_name_value : name * value -> 'a t * 'a
