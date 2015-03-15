(*
 * Copyright (c) 2013-2014 Anil Madhavapeddy <anil@recoil.org>
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

module type IO = S.IO with type 'a t = 'a Lwt.t
(** The IO module is specialized for the [Lwt] monad. *)

(** The [Net] module type defines how to connect to a remote node
    and close the resulting channels to clean up. *)
module type Net = sig
  module IO : IO
  type ctx with sexp_of
  val default_ctx: ctx
  val connect_uri : ctx:ctx -> Uri.t -> (IO.conn * IO.ic * IO.oc) Lwt.t
  val close_in : IO.ic -> unit
  val close_out : IO.oc -> unit
  val close : IO.ic -> IO.oc -> unit
end

(** The [Request] module combines the {! Cohttp.Request } module with
    the IO functions, to have them conveniently in one place. *)
module type Request = sig
  type t = Cohttp.Request.t with sexp
  include Cohttp.S.Request with type t := Cohttp.Request.t
  include Cohttp.S.Http_io with type t := Cohttp.Request.t
                            and type 'a IO.t = 'a Lwt.t
end

(** Functor to build a concrete {! Request } from an IO implementation *)
module Make_request(IO:IO) : Request with module IO = IO

(** The [Response] module combines the {! Cohttp.Request } module with
    the IO functions, to have them conveniently in one place. *)
module type Response = sig
  type t = Cohttp.Response.t with sexp
  include Cohttp.S.Response with type t := Cohttp.Response.t
  include Cohttp.S.Http_io with type t := Cohttp.Response.t
                            and type 'a IO.t = 'a Lwt.t
end

(** Functor to build a concrete {! Response } from an IO implementation *)
module Make_response(IO:IO) : Response with module IO = IO

(** The [Client] module implements non-pipelined single HTTP client
    calls.  Each call will open a separate {! Net } connection.  For
    best results, the {! Cohttp_lwt_body } that is returned should be
    consumed in order to close the file descriptor in a timely
    fashion.  It will still be finalized by a GC hook if it is not used
    up, but this can take some additional time to happen. *)
module type Client = sig
  module IO : IO
  module Request : Request with module IO = IO
  module Response : Response with module IO = IO

  type ctx with sexp_of
  val default_ctx : ctx
  val close_in : IO.ic -> unit
  val close_out : IO.oc -> unit
  val close : IO.ic -> IO.oc -> unit

  (** [call ?ctx ?headers ?body ?chunked meth uri] will resolve the
    [uri] to a concrete network endpoint using the resolver initialized
    in [ctx].  It will then issue an HTTP request with method [meth],
    adding request headers from [headers] if present.  If a [body]
    is specified then that will be included with the request, using
    chunked encoding if [chunked] is true.  The default is to disable
    chunked encoding for HTTP request bodies for compatibility reasons.

    In most cases you should use the more specific helper calls in the
    interface rather than invoke this function directly.  See {!head},
    {!get} and {!post} for some examples. *)
  val call :
    ?ctx:ctx ->
    ?headers:Cohttp.Header.t ->
    ?body:Cohttp_lwt_body.t ->
    ?chunked:bool ->
    Cohttp.Code.meth ->
    Uri.t -> (Response.t * Cohttp_lwt_body.t) Lwt.t

  val head :
    ?ctx:ctx ->
    ?headers:Cohttp.Header.t ->
    Uri.t -> Response.t Lwt.t

  val get :
    ?ctx:ctx ->
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Response.t * Cohttp_lwt_body.t) Lwt.t

  val delete :
    ?ctx:ctx ->
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Response.t * Cohttp_lwt_body.t) Lwt.t

  val post :
    ?ctx:ctx ->
    ?body:Cohttp_lwt_body.t ->
    ?chunked:bool ->
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Response.t * Cohttp_lwt_body.t) Lwt.t

  val put :
    ?ctx:ctx ->
    ?body:Cohttp_lwt_body.t ->
    ?chunked:bool ->
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Response.t * Cohttp_lwt_body.t) Lwt.t

  val patch :
    ?ctx:ctx ->
    ?body:Cohttp_lwt_body.t ->
    ?chunked:bool ->
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Response.t * Cohttp_lwt_body.t) Lwt.t

  val post_form :
    ?ctx:ctx ->
    ?headers:Cohttp.Header.t ->
    params:(string * string list) list ->
    Uri.t -> (Response.t * Cohttp_lwt_body.t) Lwt.t

  val callv :
    ?ctx:ctx ->
    Uri.t ->
    (Request.t * Cohttp_lwt_body.t) Lwt_stream.t ->
    (Response.t * Cohttp_lwt_body.t) Lwt_stream.t Lwt.t
end

(** The [Make_client] functor glues together a {! Cohttp.S.IO } implementation
    with {! Cohttp.Request } and {! Cohttp.Response } to send requests down
    a connection that is established by the  {! Net } module.
    The resulting module satisfies the {! Client } module type. *)
module Make_client
    (IO:IO)
    (Request:Request with module IO = IO)
    (Response:Response with module IO = IO)
    (Net:Net with module IO = IO) : Client
    with module IO=IO
     and module Request=Request
     and module Response=Response
     and type ctx = Net.ctx

(** The [Server] module implements a pipelined HTTP/1.1 server. *)
module type Server = sig
  module IO : IO
  module Request : Request with module IO = IO
  module Response : Response with module IO = IO

  type ctx with sexp_of
  val default_ctx : ctx
  val close_in : IO.ic -> unit
  val close_out : IO.oc -> unit
  val close : IO.ic -> IO.oc -> unit

  type conn = IO.conn * Cohttp.Connection.t

  type t

  val make : ?conn_closed:(conn -> unit)
    -> callback:(conn -> Cohttp.Request.t -> Cohttp_lwt_body.t
                 -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t)
    -> unit -> t

  (** Resolve a URI and a docroot into a concrete local filename. *)
  val resolve_local_file : docroot:string -> uri:Uri.t -> string

  (** [respond ?headers ?flush ~status ~body] will respond to an HTTP
    request with the given [status] code and response [body].  If
    [flush] is true, then every response chunk will be flushed to
    the network rather than being buffered. [flush] is true by default. 
    The transfer encoding will be detected from the [body] value and
    set to chunked encoding if it cannot be determined immediately.
    You can override the encoding by supplying an appropriate [Content-length]
    or [Transfer-encoding] in the [headers] parameter. *)
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
    ?headers:Header.t ->
    ?status:Cohttp.Code.status_code ->
    body:string -> unit -> (Response.t * Cohttp_lwt_body.t) Lwt.t

  val respond_redirect :
    ?headers:Cohttp.Header.t ->
    uri:Uri.t -> unit -> (Response.t * Cohttp_lwt_body.t) Lwt.t

  val respond_need_auth :
    ?headers:Cohttp.Header.t ->
    auth:Cohttp.Auth.challenge -> unit -> (Response.t * Cohttp_lwt_body.t) Lwt.t

  val respond_not_found :
    ?uri:Uri.t -> unit -> (Response.t * Cohttp_lwt_body.t) Lwt.t

  val callback : t -> IO.conn -> IO.ic -> IO.oc -> unit Lwt.t

end

(** The [Make_server] functor glues together a {! Cohttp.S.IO } implementation
    with {! Cohttp.Request } and {! Cohttp.Response } to send requests down
    a connection that is established by the  {! Net } module.
    The resulting module satisfies the {! Server } module type. *)
module Make_server
    (IO:IO)
    (Request:Request with module IO=IO)
    (Response:Response with module IO=IO)
    (Net:Net with module IO = IO) :
    Server with module IO = IO
            and module Request = Request
            and module Response = Response
            and type ctx = Net.ctx
