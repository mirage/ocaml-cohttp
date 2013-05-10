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

(** Pipe which gets the potentially large HTTP request/response body *)
type body = string Pipe.Reader.t

(** Read in a full body and convert to a [string] *)
val body_to_string : body -> string Deferred.t

module Client : sig

  (** Send an HTTP GET request *)
  val get :
    ?interrupt:unit Deferred.t ->
    ?headers:Cohttp.Header.t ->
    Uri.t ->
    (Cohttp.Response.r * body) Deferred.t

  (** Send an HTTP POST request.
      [chunked] encoding is off by default as not many servers support it
    *)
  val post :
    ?interrupt:unit Deferred.t ->
    ?headers:Cohttp.Header.t ->
    ?chunked:bool ->
    ?body:body ->
    Uri.t ->
    (Cohttp.Response.r * body) Deferred.t

  (** Send an HTTP request with arbitrary method and body *)
  val call :
    ?interrupt:unit Deferred.t ->
    ?headers:Cohttp.Header.t ->
    ?chunked:bool ->
    ?body:body ->
    Cohttp.Code.meth ->
    Uri.t ->
    (Cohttp.Response.r * body) Deferred.t
end

module Server : sig
  type ('address, 'listening_on) t constraint 'address = [< Socket.Address.t ]

  val close          : (_, _) t -> unit Deferred.t
  val close_finished : (_, _) t -> unit Deferred.t
  val is_closed      : (_, _) t -> bool

  type response

  val respond :
    ?body:string Pipe.Reader.t ->
    ?headers:Cohttp.Header.t ->
    Cohttp.Code.status_code -> response

  (** Resolve a URI and a docroot into a concrete local filename. *)
  val resolve_local_file : docroot:string -> uri:Uri.t -> string

  (** Respond with a [string] Pipe that provides the response body.
      @param code Default is HTTP 200 `OK *)
  val respond_with_pipe : 
    ?headers:Cohttp.Header.t -> ?code:Cohttp.Code.status_code -> string Pipe.Reader.t -> response Deferred.t

  (** Respond with a static [string] body
      @param code Default is HTTP 200 `OK *)
  val respond_with_string : 
    ?headers:Cohttp.Header.t -> ?code:Cohttp.Code.status_code -> string -> response Deferred.t

  (** Respond with file contents, and [error_body] if the file isn't found *)
  val respond_with_file : 
    ?headers:Cohttp.Header.t -> ?error_body:string -> string -> response Deferred.t

  (** Build a HTTP server, based on the [Tcp.Server] interface *)
  val create :
    ?max_connections:int ->
    ?max_pending_connections:int ->
    ?buffer_age_limit: Writer.buffer_age_limit ->
    ?on_handler_error:[ `Call of 'address -> exn  -> unit
                      | `Ignore
                      | `Raise ] ->
    ('address, 'listening_on) Tcp.Where_to_listen.t
    -> (?body:string Pipe.Reader.t -> 'address -> Cohttp.Request.r -> response Deferred.t)
    -> ('address, 'listening_on) t Deferred.t
end
