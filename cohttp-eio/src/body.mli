(** [Body] is HTTP request or response body. *)

type t =
  | Fixed of string
  | Chunked of chunk_writer
  | Custom of (Eio.Buf_write.t -> unit)
  | Empty

and chunk_writer = {
  body_writer : (chunk -> unit) -> unit;
  trailer_writer : (Http.Header.t -> unit) -> unit;
}

(** [Chunk] encapsulates HTTP/1.1 chunk transfer encoding data structures.
    https://datatracker.ietf.org/doc/html/rfc7230#section-4.1 *)
and chunk = Chunk of chunk_body | Last_chunk of chunk_extension list

and chunk_body = {
  size : int;
  data : string;
  extensions : chunk_extension list;
}

and chunk_extension = { name : string; value : string option }

val pp_chunk_extension : Format.formatter -> chunk_extension list -> unit
val pp_chunk : Format.formatter -> chunk -> unit
val add_content_length : bool -> Http.Header.t -> t -> Http.Header.t

val read_chunked :
  Buf_read.t -> Http.Header.t -> (chunk -> unit) -> Http.Header.t option

val write_body : ?write_chunked_trailers:bool -> Buf_write.t -> t -> unit

(** {1 Writer} *)

(** [writer] is a body that can be written. *)
class virtual writer :
  object
    method virtual write_body : Eio.Buf_write.t -> unit
    method virtual write_header : (name:string -> value:string -> unit) -> unit
  end

(** {2 none} *)

(** [none] is a special type of reader and writer that represents the absence of
    HTTP request or response body. It is a no-op. See {!type:Method.t} and
    {!class:Request.server_request}. *)
class virtual none :
  object
    inherit writer
  end

val none : none
(** [none] is an instance of {!class:none}. *)

(** {2 Content Writer} *)

val content_writer : content:string -> content_type:string -> writer
(** [content_writer ~content ~content_type] is
    [new content_writer ~content ~content_type]. *)

val form_values_writer : (string * string list) list -> writer
(** [form_values_writer key_values] is a {!class:writer} which writes an
    associated list [key_values] as body and adds HTTP header [Content-Length]
    to HTTP request or response. *)

(** {1 Reader} *)

(** [reader] is a body that can be read. {!class:Request.server_request} and
    {!class:Response.client_response} are both [reader] body types. As such both
    of them can be used with functions that accept [#reader] instances. See
    {!val:read_content} and {!val:read_form_values}. *)
class virtual reader :
  object
    method virtual headers : Http.Header.t
    method virtual buf_read : Eio.Buf_read.t
  end

(** {2 Content Readers} *)

val read_content : #reader -> string option
(** [read_content reader] is [Some content], where [content] is of length [n] if
    "Content-Length" header is a valid integer value [n] in [reader]. If
    ["Content-Length"] header is missing or is an invalid value in [reader] then
    [None] is returned. *)

val read_form_values : #reader -> (string * string list) list
(** [read_form_values reader] is [form_values] if [reader] body [Content-Type]
    is ["application/x-www-form-urlencoded"] and [Content-Length] is a valid
    integer value. [form_values] is a list of tuple of form [(name, values)]
    where [name] is the name of the form field and [values] is a list of values
    corresponding to the [name]. *)
