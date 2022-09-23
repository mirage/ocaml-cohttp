type 'a t = ..
type name = string
type value = string

type 'a t +=
  | Content_length : int t
  | Transfer_encoding : [ `chunked | `compress | `deflate | `gzip ] list t
  | Header : name -> value t

val compare : 'a t -> 'b t -> ('a, 'b) Gmap.Order.t
