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

open Cohttp

(** Portable Lwt implementation of HTTP client and server, without
    depending on a particular I/O implementation.  The various [Make]
    functors must be instantiated by an implementation that provides
    a concrete IO monad. *)

(** The [Net] module type defines how to connect to a remote node
    and close the resulting channels to clean up. *)
module type Net = sig
  module IO : S.IO
  val connect_uri : Uri.t -> (IO.conn * IO.ic * IO.oc) Lwt.t
  val connect : ?ssl:bool -> host:string -> service:string -> unit -> (IO.conn * IO.ic * IO.oc) Lwt.t
  val close_in : IO.ic -> unit
  val close_out : IO.oc -> unit
  val close : IO.ic -> IO.oc -> unit
end

(** The [Request] module combines the {! Cohttp.Request } module with
    the IO functions, to have them conveniently in one place. *)
module type Request = sig
  type t = Cohttp.Request.t
  include Cohttp.S.Request with type t := Cohttp.Request.t
  include Cohttp.S.Http_io with type t := Cohttp.Request.t
end

(** Functor to build a concrete {! Request } from an IO implementation *)
module Make_request(IO:S.IO) : Request with module IO = IO

(** The [Response] module combines the {! Cohttp.Request } module with
    the IO functions, to have them conveniently in one place. *)
module type Response = sig
  type t = Cohttp.Response.t
  include Cohttp.S.Response with type t := Cohttp.Response.t
  include Cohttp.S.Http_io with type t := Cohttp.Response.t
end

(** Functor to build a concrete {! Response } from an IO implementation *)
module Make_response(IO:S.IO) : Response with module IO = IO

(** The [Client] module implements non-pipelined single HTTP client
    calls.  Each call will open a separate {! Net } connection.  For
    best results, the {! Cohttp_lwt_body } that is returned should be
    consumed in order to close the file descriptor in a timely
    fashion.  It will still be finalized by a GC hook if it is not used
    up, but this can take some additional time to happen. *)
module type Client = sig
  module IO : S.IO
  module Request : Request
  module Response : Response

  val call :
    ?headers:Cohttp.Header.t ->
    ?body:Cohttp_lwt_body.t ->
    ?chunked:bool ->
    Cohttp.Code.meth ->
    Uri.t -> (Response.t * Cohttp_lwt_body.t) Lwt.t

  val head :
    ?headers:Cohttp.Header.t ->
    Uri.t -> Response.t Lwt.t

  val get :
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Response.t * Cohttp_lwt_body.t) Lwt.t

  val delete :
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Response.t * Cohttp_lwt_body.t) Lwt.t

  val post :
    ?body:Cohttp_lwt_body.t ->
    ?chunked:bool ->
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Response.t * Cohttp_lwt_body.t) Lwt.t

  val put :
    ?body:Cohttp_lwt_body.t ->
    ?chunked:bool ->
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Response.t * Cohttp_lwt_body.t) Lwt.t

  val patch :
    ?body:Cohttp_lwt_body.t ->
    ?chunked:bool ->
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Response.t * Cohttp_lwt_body.t) Lwt.t

  val post_form :
    ?headers:Cohttp.Header.t ->
    params:Cohttp.Header.t ->
    Uri.t -> (Response.t * Cohttp_lwt_body.t) Lwt.t

  val callv :
    ?ssl:bool ->
    string ->
    int ->
    (Request.t * Cohttp_lwt_body.t) Lwt_stream.t ->
    (Response.t * Cohttp_lwt_body.t) Lwt_stream.t Lwt.t
end

(** The [Make_client] functor glues together a {! Cohttp.S.IO } implementation
    with {! Cohttp.Request } and {! Cohttp.Response } to send requests down
    a connection that is established by the  {! Net } module.
    The resulting module satisfies the {! Client } module type. *)
module Make_client
    (IO:Cohttp.S.IO with type 'a t = 'a Lwt.t)
    (Request:Request with module IO = IO)
    (Response:Response with module IO = IO)
    (Net:Net with module IO = IO) :
    Client with module IO=IO and module Request=Request and module Response=Response

(** The [Server] module implements a pipelined HTTP/1.1 server. *)
module type Server = sig
  module IO : S.IO
  module Request : Request
  module Response : Response
  type t = {
    callback :
      IO.conn ->
      Cohttp.Request.t ->
      Cohttp_lwt_body.t ->
      (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t;
    conn_closed:
      IO.conn -> unit -> unit;
  }

  (** Resolve a URI and a docroot into a concrete local filename. *)
  val resolve_local_file : docroot:string -> uri:Uri.t -> string

  val respond :
    ?headers:Cohttp.Header.t ->
    ?flush:bool ->
    status:Cohttp.Code.status_code ->
    body:Cohttp_lwt_body.t -> unit -> (Response.t * Cohttp_lwt_body.t) Lwt.t

  val respond_string :
    ?headers:Cohttp.Header.t ->
    status:Cohttp.Code.status_code ->
    body:string -> unit -> (Response.t * Cohttp_lwt_body.t) Lwt.t

  val respond_error :
    status:Cohttp.Code.status_code ->
    body:string -> unit -> (Response.t * Cohttp_lwt_body.t) Lwt.t

  val respond_redirect :
    ?headers:Cohttp.Header.t ->
    uri:Uri.t -> unit -> (Response.t * Cohttp_lwt_body.t) Lwt.t

  val respond_need_auth :
    ?headers:Cohttp.Header.t ->
    auth:Cohttp.Auth.req -> unit -> (Response.t * Cohttp_lwt_body.t) Lwt.t

  val respond_not_found :
    ?uri:Uri.t -> unit -> (Response.t * Cohttp_lwt_body.t) Lwt.t

  val callback : t -> IO.conn -> IO.ic -> IO.oc -> unit Lwt.t

end

(** The [Make_server] functor glues together a {! Cohttp.S.IO } implementation
    with {! Cohttp.Request } and {! Cohttp.Response } to send requests down
    a connection that is established by the  {! Net } module.
    The resulting module satisfies the {! Server } module type. *)
module Make_server
    (IO:Cohttp.S.IO with type 'a t = 'a Lwt.t)
    (Request:Request with module IO=IO)
    (Response:Response with module IO=IO)
    (Net:Net with module IO = IO) :
    Server with module IO = IO
            and module Request = Request
            and module Response = Response
