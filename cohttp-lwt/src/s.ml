
(** Portable Lwt implementation of HTTP client and server, without
    depending on a particular I/O implementation.  The various [Make]
    functors must be instantiated by an implementation that provides
    a concrete IO monad. *)

module type IO = Cohttp.S.IO with type 'a t = 'a Lwt.t
(** The IO module is specialized for the [Lwt] monad. *)

(** The [Net] module type defines how to connect to a remote node
    and close the resulting channels to clean up. *)
module type Net = sig
  module IO : IO
  type ctx [@@deriving sexp_of]
  val default_ctx: ctx
  val connect_uri : ctx:ctx -> Uri.t -> (IO.conn * IO.ic * IO.oc) Lwt.t
  val close_in : IO.ic -> unit
  val close_out : IO.oc -> unit
  val close : IO.ic -> IO.oc -> unit
end

(** The [Client] module implements non-pipelined single HTTP client
    calls.  Each call will open a separate {! Net } connection.  For
    best results, the {! Body } that is returned should be
    consumed in order to close the file descriptor in a timely
    fashion.  It will still be finalized by a GC hook if it is not used
    up, but this can take some additional time to happen. *)
module type Client = sig

  type ctx [@@deriving sexp_of]
  val default_ctx : ctx

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
    ?body:Body.t ->
    ?chunked:bool ->
    Cohttp.Code.meth ->
    Uri.t -> (Cohttp.Response.t * Body.t) Lwt.t

  val head :
    ?ctx:ctx ->
    ?headers:Cohttp.Header.t ->
    Uri.t -> Cohttp.Response.t Lwt.t

  val get :
    ?ctx:ctx ->
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Cohttp.Response.t * Body.t) Lwt.t

  val delete :
    ?ctx:ctx ->
    ?body:Body.t ->
    ?chunked:bool ->
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Cohttp.Response.t * Body.t) Lwt.t

  val post :
    ?ctx:ctx ->
    ?body:Body.t ->
    ?chunked:bool ->
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Cohttp.Response.t * Body.t) Lwt.t

  val put :
    ?ctx:ctx ->
    ?body:Body.t ->
    ?chunked:bool ->
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Cohttp.Response.t * Body.t) Lwt.t

  val patch :
    ?ctx:ctx ->
    ?body:Body.t ->
    ?chunked:bool ->
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Cohttp.Response.t * Body.t) Lwt.t

  val post_form :
    ?ctx:ctx ->
    ?headers:Cohttp.Header.t ->
    params:(string * string list) list ->
    Uri.t -> (Cohttp.Response.t * Body.t) Lwt.t

  val callv :
    ?ctx:ctx ->
    Uri.t ->
    (Cohttp.Request.t * Body.t) Lwt_stream.t ->
    (Cohttp.Response.t * Body.t) Lwt_stream.t Lwt.t
end

(** The [Server] module implements a pipelined HTTP/1.1 server. *)
module type Server = sig
  module IO : IO

  type conn = IO.conn * Cohttp.Connection.t

  type t

  val make : ?conn_closed:(conn -> unit)
    -> callback:(conn -> Cohttp.Request.t -> Body.t
                 -> (Cohttp.Response.t * Body.t) Lwt.t)
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
    ?on_exn:(exn -> unit) ->
    ?flush:bool ->
    status:Cohttp.Code.status_code ->
    body:Body.t -> unit -> (Cohttp.Response.t * Body.t) Lwt.t

  val respond_string :
    ?flush:bool ->
    ?headers:Cohttp.Header.t ->
    status:Cohttp.Code.status_code ->
    body:string -> unit -> (Cohttp.Response.t * Body.t) Lwt.t

  val respond_error :
    ?headers:Cohttp.Header.t ->
    ?status:Cohttp.Code.status_code ->
    body:string -> unit -> (Cohttp.Response.t * Body.t) Lwt.t

  val respond_redirect :
    ?headers:Cohttp.Header.t ->
    uri:Uri.t -> unit -> (Cohttp.Response.t * Body.t) Lwt.t

  val respond_need_auth :
    ?headers:Cohttp.Header.t ->
    auth:Cohttp.Auth.challenge -> unit -> (Cohttp.Response.t * Body.t) Lwt.t

  val respond_not_found :
    ?uri:Uri.t -> unit -> (Cohttp.Response.t * Body.t) Lwt.t

  val callback : t -> IO.conn -> IO.ic -> IO.oc -> unit Lwt.t

end
