module type BASE = sig
  module Io : Cohttp.S.IO

  type 'a with_context

  val map_context : 'a with_context -> ('a -> 'b) -> 'b with_context

  val call :
    (?headers:Http.Header.t ->
    ?body:Eio.Flow.source ->
    Http.Method.t ->
    Uri.t ->
    (Http.Response.t * Eio.Flow.source, string) Result.t Io.t)
    with_context
  (** Send an HTTP request with arbitrary method and a body. If the URI has a
      host, we use a TCP connection, otherwaise a UNIX domain socket. *)
end

module type S = sig
  include BASE

  val delete :
    (?headers:Http.Header.t ->
    Uri.t ->
    (Http.Response.t * Eio.Flow.source, string) Result.t Io.t)
    with_context
  (** Send an HTTP DELETE request *)

  val get :
    (?headers:Http.Header.t ->
    Uri.t ->
    (Http.Response.t * Eio.Flow.source, string) Result.t Io.t)
    with_context
  (** Send an HTTP GET request *)

  val head :
    (?headers:Http.Header.t ->
    Uri.t ->
    (Http.Response.t * Eio.Flow.source, string) Result.t Io.t)
    with_context
  (** Send an HTTP HEAD request *)

  val patch :
    (?headers:Http.Header.t ->
    Uri.t ->
    (Http.Response.t * Eio.Flow.source, string) Result.t Io.t)
    with_context
  (** Send an HTTP PATCH request *)

  val post :
    (?headers:Http.Header.t ->
    Uri.t ->
    (Http.Response.t * Eio.Flow.source, string) Result.t Io.t)
    with_context
  (** Send an HTTP POST request *)

  val put :
    (?headers:Http.Header.t ->
    Uri.t ->
    (Http.Response.t * Eio.Flow.source, string) Result.t Io.t)
    with_context
  (** Send an HTTP PUT request *)
end
