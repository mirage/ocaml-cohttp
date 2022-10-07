type name = string
type value = string
type 'a header = ..

type 'a header +=
  | Content_length : int header
  | Transfer_encoding : [ `chunked | `compress | `deflate | `gzip ] list header
  | Date : Ptime.t header

type (_, _) eq = Eq : ('a, 'a) eq

type header_ext = {
  decode : 'a. 'a header -> string -> string -> ('a, string) result;
  equal : 'a 'b. 'a header -> 'b header -> ('a, 'b) eq option;
}

exception Unrecognized_header of string
exception Duplicate_header of string

val extend : 'a header -> header_ext -> unit
(** [extend t header] extends [t] with [header].

    @raise Duplicate_header if [header] already exists *)

val equal : 'a header -> 'b header -> ('a, 'b) eq option

val name_value : 'a header -> 'a -> name * value
(** [name_value hdr v] is [(name, value)] which represents a string value of
    [hdr] and [v] respectively. *)
