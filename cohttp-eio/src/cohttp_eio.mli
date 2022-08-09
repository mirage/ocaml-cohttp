module Body : sig
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
end

(** [Server] is a HTTP 1.1 server. *)
module Server : sig
  type request = Http.Request.t * Eio.Buf_read.t * Eio.Net.Sockaddr.stream
  (** The request headers, a reader for the socket, and the address of the
      client. To read the request body, use {!read_fixed} or {!read_chunked}. *)

  type response = Http.Response.t * Body.t
  type handler = request -> response

  (** {1 Request} *)

  val read_fixed : Http.Request.t -> Eio.Buf_read.t -> string
  (** [read_fixed request body] reads a string of length [n] if "Content-Length"
      header is a valid integer value [n] in [request].

      @raise Invalid_argument
        if ["Content-Length"] header is missing or is an invalid value in
        [headers] OR if the request http method is not one of [POST], [PUT] or
        [PATCH]. *)

  val read_chunked :
    Http.Request.t -> Eio.Buf_read.t -> (Body.chunk -> unit) -> Http.Header.t
  (** [read_chunked request body chunk_handler] is [updated_headers] if
      "Transfer-Encoding" header value is "chunked" in [headers] and all chunks
      in [reader] are read successfully. [updated_headers] is the updated
      headers as specified by the chunked encoding algorithm in
      https://datatracker.ietf.org/doc/html/rfc7230#section-4.1.3. Otherwise it
      is [Error err] where [err] is the error text.

      @raise Invalid_argument
        if [Transfer-Encoding] header in [headers] is not specified as "chunked" *)

  (** {1 Response} *)

  val text_response : string -> response
  (** [text t s] returns a HTTP/1.1, 200 status response with "Content-Type"
      header set to "text/plain". *)

  val html_response : string -> response
  (** [html t s] returns a HTTP/1.1, 200 status response with header set to
      "Content-Type: text/html". *)

  val not_found_response : response
  (** [not_found t] returns a HTTP/1.1, 404 status response. *)

  val internal_server_error_response : response
  (** [internal_server_error] returns a HTTP/1.1, 500 status response. *)

  val bad_request_response : response
  (* [bad_request t] returns a HTTP/1.1, 400 status response. *)

  (** {1 Run Server} *)

  val run :
    ?socket_backlog:int ->
    ?domains:int ->
    port:int ->
    < domain_mgr : Eio.Domain_manager.t ; net : Eio.Net.t ; .. > ->
    handler ->
    'a

  val connection_handler :
    handler -> #Eio.Net.stream_socket -> Eio.Net.Sockaddr.stream -> unit
  (** [connection_handler request_handler] is a connection handler, suitable for
      passing to {!Eio.Net.accept_fork}. *)

  (** {1 Basic Handlers} *)

  val not_found_handler : handler
end
