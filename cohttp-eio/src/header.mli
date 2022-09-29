type 'a t = ..
type name = string
type value = string

type 'a t +=
  | Content_length : int t
  | Transfer_encoding : [ `chunked | `compress | `deflate | `gzip ] list t
  | Date : Ptime.t t
  | Header : name -> value t

exception Unrecognized_header of string

val compare : 'a t -> 'b t -> ('a, 'b) Gmap.Order.t

val name_value : 'a t -> 'a -> string * string
(** [name_value hdr v] is a tuple of [(name, value)]. *)
