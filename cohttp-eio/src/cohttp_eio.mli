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

  (** {1 Request Body} *)

  val read_fixed : Http.Request.t -> Eio.Buf_read.t -> string option
  (** [read_fixed (request, buf_read)] is [Some content], where [content] is of
      length [n] if "Content-Length" header is a valid integer value [n] in
      [request].

      [buf_read] is updated to reflect that [n] bytes was read.

      If ["Content-Length"] header is missing or is an invalid value in
      [request] OR if the request http method is not one of [POST], [PUT] or
      [PATCH], then [None] is returned. *)

  val read_chunked :
    Http.Request.t ->
    Eio.Buf_read.t ->
    (Body.chunk -> unit) ->
    Http.Header.t option
  (** [read_chunked request buf_read chunk_handler] is [Some updated_headers] if
      "Transfer-Encoding" header value is "chunked" in [request] and all chunks
      in [buf_read] are read successfully. [updated_headers] is the updated
      headers as specified by the chunked encoding algorithm in https:
      //datatracker.ietf.org/doc/html/rfc7230#section-4.1.3.

      [buf_read] is updated to reflect the number of bytes read. Returns [None]
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

(** [Client] is a HTTP/1.1 client. *)
module Client : sig
  type response = Http.Response.t * Eio.Buf_read.t

  type host = string * int option
  (** Represents a server host - as ip address or domain name - and an optional
      port value, e.g. www.example.org:8080, www.reddit.com *)

  type resource_path = string
  (** Represents HTTP request resource path, e.g. "/shop/purchase",
      "/shop/items", "/shop/categories/" etc. *)

  type 'a body_disallowed_call =
    ?version:Http.Version.t ->
    ?headers:Http.Header.t ->
    conn:(#Eio.Flow.two_way as 'a) ->
    host ->
    resource_path ->
    response
  (** [body_disallowed_call] denotes HTTP client calls where a request is not
      allowed to have a request body. *)

  type 'a body_allowed_call =
    ?version:Http.Version.t ->
    ?headers:Http.Header.t ->
    ?body:Body.t ->
    conn:(#Eio.Flow.two_way as 'a) ->
    host ->
    resource_path ->
    response
  (** [body_allowed_call] denotes HTTP client calls where a request can
      optionally have a request body. *)

  (** {1 Generic HTTP call} *)

  val call :
    ?meth:Http.Method.t ->
    ?version:Http.Version.t ->
    ?headers:Http.Header.t ->
    ?body:Body.t ->
    conn:#Eio.Flow.two_way ->
    host ->
    resource_path ->
    response

  (** {1 HTTP Calls with Body Disallowed} *)

  val get : 'a body_disallowed_call
  val head : 'a body_disallowed_call
  val delete : 'a body_disallowed_call

  (** {1 HTTP Calls with Body Allowed} *)

  val post : 'a body_allowed_call
  val put : 'a body_allowed_call
  val patch : 'a body_allowed_call

  (** {1 Response Body} *)

  val read_fixed : response -> string
  (** [read_fixed (response,reader)] is [body_content], where [body_content] is
      of length [n] if "Content-Length" header exists and is a valid integer
      value [n] in [response]. Otherwise [body_content] holds all bytes until
      eof. *)

  val read_chunked : response -> (Body.chunk -> unit) -> Http.Header.t option
  (** [read_chunked response chunk_handler] is [Some updated_headers] if
      "Transfer-Encoding" header value is "chunked" in [response] and all chunks
      in [reader] are read successfully. [updated_headers] is the updated
      headers as specified by the chunked encoding algorithm in https:
      //datatracker.ietf.org/doc/html/rfc7230#section-4.1.3.

      [reader] is updated to reflect the number of bytes read.

      Returns [None] if [Transfer-Encoding] header in [headers] is not specified
      as "chunked" *)
end
