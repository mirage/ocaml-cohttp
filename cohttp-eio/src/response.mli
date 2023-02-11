(** [Response] A HTTP Response. *)

(** [t] is a common response abstraction for response types
    {!class:server_response} and {!class:client_response}. *)
class virtual t :
  object
    method virtual version : Http.Version.t
    method virtual headers : Http.Header.t
    method virtual status : Http.Status.t
  end

val version : #t -> Http.Version.t
(** [version t] is HTTP version of response [t]. *)

val headers : #t -> Http.Header.t
(** [headers t] is HTTP headers for response [t]. *)

val status : #t -> Http.Status.t
(** [status t] is HTTP status code for response [t]. *)

(** {1 Client Response} *)

exception Closed

class client_response :
  Http.Version.t
  -> Http.Header.t
  -> Http.Status.t
  -> Eio.Buf_read.t
  -> object
       inherit t
       inherit Body.reader
       method version : Http.Version.t
       method headers : Http.Header.t
       method status : Http.Status.t
       method buf_read : Eio.Buf_read.t
       method body_closed : bool
       method close_body : unit
     end

val parse : Eio.Buf_read.t -> Http.Version.t * Header.t * Http.Status.t
(** [parse buf_read] parses reponse [version,headers,status] from [buf_read]. *)

val close_body : #client_response -> unit
(** [close response] closes the response body of [response]. Once the [response]
    body is closed subsequent calls to read the [response] body will result in
    raising {!exception:Closed}. *)

val body_closed : #client_response -> bool
(** [closed response] is [true] if [response] is closed. [false] otherwise. *)

(** {1 Server Response} *)

class virtual server_response :
  object
    inherit t
    inherit Body.writer
  end

val server_response :
  ?version:Http.Version.t ->
  ?headers:Http.Header.t ->
  ?status:Http.Status.t ->
  Body.writer ->
  server_response
(** [server_response body] is a server response with body [body]. *)

val chunked_response :
  ua_supports_trailer:bool ->
  Chunked_body.write_chunk ->
  Chunked_body.write_trailer ->
  server_response
(** [chunked_response ~ua_supports_trailer write_chunk write_trailer] is a HTTP
    chunked response.

    See {!module:Chunked_body}. *)

val write : #server_response -> #Eio.Time.clock -> Eio.Buf_write.t -> unit
(** [write response clock buf_write] writes server response [response] to
    [buf_write]. [clock] is used to generate "Date" header if required. *)

val text : string -> server_response
(** [text s] returns a HTTP/1.1, 200 status response with "Content-Type" header
    set to "text/plain" and "Content-Length" header set to a suitable value. *)

val html : string -> server_response
(** [html t s] returns a HTTP/1.1, 200 status response with header set to
    "Content-Type: text/html" and "Content-Length" header set to a suitable
    value. *)

val not_found : server_response
(** [not_found] returns a HTTP/1.1, 404 status response. *)

val internal_server_error : server_response
(** [internal_server_error] returns a HTTP/1.1, 500 status response. *)

val bad_request : server_response
(* [bad_request] returns a HTTP/1.1, 400 status response. *)

(** {1 Pretty Printer} *)

val pp : Format.formatter -> #t -> unit
