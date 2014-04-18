(*
 * Copyright (c) 2013 Anil Madhavapeddy <anil@recoil.org>
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
*)

open Core.Std
open Async.Std

(** Read in a full body and convert to a [string] *)

module IO : Cohttp.IO.S with type 'a t = 'a Deferred.t

module Request : sig
  include module type of Cohttp.Request with type t = Cohttp.Request.t
  include Cohttp.Request.S with module IO=IO
end

module Response : sig
  include module type of Cohttp.Response with type t = Cohttp.Response.t
  include Cohttp.Response.S with module IO=IO
end

module Body : sig
  type t = [
    | Cohttp.Body.t
    | `Pipe of string Pipe.Reader.t
  ] with sexp_of
  include Cohttp.Body.S with type t := t
  val to_string : t -> string Deferred.t
  val to_pipe : t -> string Pipe.Reader.t
  val of_pipe : string Pipe.Reader.t -> t
end

module Client : sig

  (** Full response consisting of headers, status code, and body *)
  type full_response = (Response.t * Body.t) Deferred.t

  (** Basic http request that returns [response] *)
  type 'response request =
    ?interrupt:unit Deferred.t
    -> ?headers:Cohttp.Header.t
    -> Uri.t
    -> 'response

  (** Request with [body] optionally provided.
      [chunked] encoding is off by default as not many servers support it *)
  type 'response with_body = ?chunked:bool -> ?body:Body.t -> 'response

  (** Send an HTTP GET request *)
  val get : full_response request

  (** Send an HTTP HEAD request *)
  val head : Response.t Deferred.t request

  (** Send an HTTP DELETE request *)
  val delete : full_response request

  (** Send an HTTP POST request.  *)
  val post : full_response request with_body

  (** Send an HTTP PUT request.  *)
  val put : full_response request with_body

  (** Send an HTTP PATCH request.  *)
  val patch : full_response request with_body

  (** Send an HTTP request with arbitrary method and a body *)
  val call : (?interrupt:unit Deferred.t
              -> ?headers:Cohttp.Header.t
              -> Cohttp.Code.meth
              -> Uri.t
              -> full_response) with_body

end

module Server : sig
  type ('address, 'listening_on) t constraint 'address = [< Socket.Address.t ]
  with sexp_of

  val close          : (_, _) t -> unit Deferred.t
  val close_finished : (_, _) t -> unit Deferred.t
  val is_closed      : (_, _) t -> bool

  type response with sexp_of

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
    ?mode:Async_conduit.Server.mode ->
    ('address, 'listening_on) Tcp.Where_to_listen.t
    -> (body:Body.t -> 'address -> Request.t -> response Deferred.t)
    -> ('address, 'listening_on) t Deferred.t
end
