(** [Chunked_body] is HTTP [chunked] Transfer-Encoding encoder and decoders as
    described in https://datatracker.ietf.org/doc/html/rfc7230#section-4.1.3. *)

type t
(** [t] is a HTTP chunk. *)

val make : ?extensions:(string * string option) list -> string -> t
(** [make data] is a chunk [t]. [t] encapsulates data [data]. If
    [String.length data = 0], then the chunk [t] denotes an end of chunked
    transfer-encoding transmission.

    @param extensions
      is a list of extensions associted with [t]. Chunk extension encodes
      additional information about [data] in [t]. An extension is a tuple of
      [(name, value)] where [value] is optional. *)

val data : t -> string option
(** [data t] is [Some data] if a chunk [t] holds data. Otherwise it is [None]. A
    [None] data denotes an end of the chunked transfer encoding. *)

val extensions : t -> (string * string option) list
(** [extensions t] is a list of extensions associated with chunk [t].*)

(** {1 Writer} *)

type write_chunk = (t -> unit) -> unit
(** [write_chunk f] specifies HTTP chunks to be written by a
    {!type:Body.writer}. We specify chunks by applying [f chunk].

    For example, to write a "Hello, world!" in two chunks of "Hello, " and
    "world!", an implementation could be as following :

    {[
      let write_chunk f =
        f (Chunk {data="Hello, "; extensions = []);
        f (Chunk {data="world!"; extension = []);
        f (Last_chunk {extensions = []);
    ]} *)

type write_trailer = (Http.Header.t -> unit) -> unit
(** [write_trailer f] specifies HTTP chunked trailer headers to be written by a
    {!type:Body.writer}. We specify the trailer headers by applying [f headers].

    {[
      let write_trailer f =
        let headers =
          Http.Header.init_with [ ("Expires", "Wed, 21 Oct 2015 07:28:00 GMT") ]
        in
        f headers
    ]} *)

val writer :
  ua_supports_trailer:bool -> write_chunk -> write_trailer -> Body.writer
(** [writer ~ua_supports_trailer write_chunk write_trailer] is
    {!type:Body.writer} for HTTP [chunked] transfer encoding.

    The writer is usually used as a body in HTTP {!type:Request.client} and
    {!type:Response.server_response}.

    @param ua_supports_trailer
      indicates whether an user-agent such as browsers or HTTP clients supports
      receiving chunked trailer headers. This is usually done by adding HTTP
      header "TE" with value "trailers" in requests. See
      {!val:Request.supports_chunked_trailers}. *)

(** {1 Reader} *)

val read_chunked : (t -> unit) -> #Body.reader -> Http.Header.t option
(** [read_chunked f reader] is [Some updated_headers] if "Transfer-Encoding"
    header value is "chunked" in [request]. Each chunk is applied as [f chunk].
    [updated_headers] is the updated headers as specified by the chunked
    encoding algorithm in
    https://datatracker.ietf.org/doc/html/rfc7230#section-4.1.3.

    Returns [None] if [Transfer-Encoding] header in [headers] is not specified
    as "chunked" *)

(** {1 Pretty Printers} *)

val pp : Format.formatter -> t -> unit
