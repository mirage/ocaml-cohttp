(*{{{ Copyright (c) 2013 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
  }}}*)

open Core.Std
open Async.Std

module Request : Cohttp.S.Request with type t = Cohttp.Request.t
module Response : Cohttp.S.Response with type t = Cohttp.Response.t

module Body : sig
  type t = [
    | Cohttp.Body.t
    | `Pipe of string Pipe.Reader.t
  ] with sexp_of
  include Cohttp.S.Body with type t := t
  val drain : t -> unit Deferred.t
  val is_empty : t -> bool Deferred.t
  val to_string : t -> string Deferred.t
  val to_string_list : t -> string list Deferred.t
  val to_pipe : t -> string Pipe.Reader.t
  val of_pipe : string Pipe.Reader.t -> t
  val map : t -> f:(string -> string) -> t
  val as_pipe : t -> f:(string Pipe.Reader.t -> string Pipe.Reader.t) -> t
end

module Client : sig

  (** Send an HTTP request with an arbitrary body
      The request is sent as-is. *)
  val request :
    ?interrupt:unit Deferred.t ->
    ?body:Body.t ->
    Request.t ->
    (Response.t * Body.t) Deferred.t

  (** Send an HTTP request with arbitrary method and a body
      Infers the transfer encoding *)
  val call :
    ?interrupt:unit Deferred.t ->
    ?headers:Cohttp.Header.t ->
    ?chunked:bool ->
    ?body:Body.t ->
    Cohttp.Code.meth ->
    Uri.t ->
    (Response.t * Body.t) Deferred.t

  val callv :
    ?interrupt:unit Deferred.t ->
    Uri.t ->
    (Request.t * Body.t) Pipe.Reader.t ->
    (Response.t * Body.t) Pipe.Reader.t Deferred.t

  (** Send an HTTP GET request *)
  val get :
    ?interrupt:unit Deferred.t ->
    ?headers:Cohttp.Header.t ->
    Uri.t ->
    (Response.t * Body.t) Deferred.t

  (** Send an HTTP HEAD request *)
  val head :
    ?interrupt:unit Deferred.t ->
    ?headers:Cohttp.Header.t ->
    Uri.t ->
    Response.t Deferred.t

  (** Send an HTTP DELETE request *)
  val delete :
    ?interrupt:unit Deferred.t ->
    ?headers:Cohttp.Header.t ->
    ?chunked:bool ->
    ?body:Body.t ->
    Uri.t ->
    (Response.t * Body.t) Deferred.t

  (** Send an HTTP POST request.
      [chunked] encoding is off by default as not many servers support it
  *)
  val post :
    ?interrupt:unit Deferred.t ->
    ?headers:Cohttp.Header.t ->
    ?chunked:bool ->
    ?body:Body.t ->
    Uri.t ->
    (Response.t * Body.t) Deferred.t

  (** Send an HTTP PUT request.
      [chunked] encoding is off by default as not many servers support it
  *)
  val put :
    ?interrupt:unit Deferred.t ->
    ?headers:Cohttp.Header.t ->
    ?chunked:bool ->
    ?body:Body.t ->
    Uri.t ->
    (Response.t * Body.t) Deferred.t

  (** Send an HTTP PATCH request.
      [chunked] encoding is off by default as not many servers support it
  *)
  val patch :
    ?interrupt:unit Deferred.t ->
    ?headers:Cohttp.Header.t ->
    ?chunked:bool ->
    ?body:Body.t ->
    Uri.t ->
    (Response.t * Body.t) Deferred.t

  (** Send an HTTP POST request in form format *)
  val post_form:
    ?interrupt:unit Deferred.t ->
    ?headers:Cohttp.Header.t ->
    params:(string * string list) list ->
    Uri.t ->
    (Response.t * Body.t) Deferred.t
end

module Server : sig
  type ('address, 'listening_on) t constraint 'address = [< Socket.Address.t ]
  with sexp_of

  val close          : (_, _) t -> unit Deferred.t
  val close_finished : (_, _) t -> unit Deferred.t
  val is_closed      : (_, _) t -> bool

  type response = Response.t * Body.t with sexp_of

  val respond :
    ?flush:bool ->
    ?headers:Cohttp.Header.t ->
    ?body:Body.t ->
    Cohttp.Code.status_code -> response Deferred.t

  (** Resolve a URI and a docroot into a concrete local filename. *)
  val resolve_local_file : docroot:string -> uri:Uri.t -> string

  (** Respond with a [string] Pipe that provides the response string Pipe.Reader.t.
      @param code Default is HTTP 200 `OK *)
  val respond_with_pipe :
    ?flush:bool ->
    ?headers:Cohttp.Header.t -> ?code:Cohttp.Code.status_code ->
    string Pipe.Reader.t -> response Deferred.t

  (** Respond with a static [string]
      @param code Default is HTTP 200 `OK *)
  val respond_with_string :
    ?flush:bool ->
    ?headers:Cohttp.Header.t -> ?code:Cohttp.Code.status_code ->
    string -> response Deferred.t

  (** Respond with a redirect to an absolute [uri]
      @param uri Absolute URI to redirect the client to *)
  val respond_with_redirect :
    ?headers:Cohttp.Header.t -> Uri.t -> response Deferred.t


  (** Respond with file contents, and [error_string Pipe.Reader.t] if the file isn't found *)
  val respond_with_file :
    ?flush:bool ->
    ?headers:Cohttp.Header.t -> ?error_body:string ->
    string -> response Deferred.t

  (** Build a HTTP server, based on the [Tcp.Server] interface *)
  val create :
    ?max_connections:int ->
    ?max_pending_connections:int ->
    ?buffer_age_limit: Writer.buffer_age_limit ->
    ?on_handler_error:[ `Call of 'address -> exn  -> unit
                      | `Ignore
                      | `Raise ] ->
    ?mode:Conduit_async.server ->
    ('address, 'listening_on) Tcp.Where_to_listen.t
    -> (body:Body.t -> 'address -> Request.t -> response Deferred.t)
    -> ('address, 'listening_on) t Deferred.t
end
