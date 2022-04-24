(** Portable Lwt implementation of HTTP client and server, without depending on
    a particular I/O implementation. The various [Make] functors must be
    instantiated by an implementation that provides a concrete IO monad. *)

(** The IO module is specialized for the [Lwt] monad. *)
module type IO = sig
  include Cohttp.S.IO with type 'a t = 'a Lwt.t

  type error

  val catch : (unit -> 'a t) -> ('a, error) result t
  (** [catch f] is [f () >|= Result.ok], unless [f] fails with an IO error, in
      which case it returns the error. *)

  val pp_error : Format.formatter -> error -> unit
end

(** The [Net] module type defines how to connect to a remote node and close the
    resulting channels to clean up. *)
module type Net = sig
  module IO : IO

  type endp

  (** Conduit context. Contains configuration of resolver, local source
      address, TLS / SSL library, certificates, keys.

      Depending on [ctx], the library is able to send HTTP requests
      unencrypted or encrypted one with a secured protocol (such as
      TLS). Depending on how conduit is configured, [ctx] might initiate
      a secured connection with TLS (using [ocaml-tls]) or SSL (using
      [ocaml-ssl]), on [*:443] or on the specified port by the user. If
      neitehr [ocaml-tls] or [ocaml-ssl] are installed on the system,
      [cohttp]/[conduit] tries the usual ([*:80]) or the specified port
      by the user in a non-secured way. *)
  type ctx [@@deriving sexp_of]

  val default_ctx : ctx
  val resolve : ctx:ctx -> Uri.t -> endp IO.t
  val connect_uri : ctx:ctx -> Uri.t -> (IO.conn * IO.ic * IO.oc) IO.t
  val connect_endp : ctx:ctx -> endp -> (IO.conn * IO.ic * IO.oc) IO.t
  val close_in : IO.ic -> unit
  val close_out : IO.oc -> unit
  val close : IO.ic -> IO.oc -> unit
end

(** This is compatible with [Mirage_time.S].
    It may be satisfied by mirage-time-unix [Time] or [Mirage_time]. *)
module type Sleep = sig
  type 'a promise
  val sleep_ns : int64 -> unit promise
end

(** [request ?body request]
    Function type used to process requests.

    @return [(response, response_body)]
    [response_body] is not buffered, but stays on the wire until
    consumed. It must therefore be consumed in a timely manner.
    Otherwise the connection would stay open and a file descriptor leak
    may be caused. Following responses would get blocked.
    Functions in the {!Body} module can be used to consume [response_body].
    Use {!Body.drain_body} if you don't consume the body by other means.

    Leaks are detected by the GC and logged as debug messages, these can
    be enabled activating the debug logging. For example, this can be
    done as follows in [cohttp-lwt-unix]

    {[
      Cohttp_lwt_unix.Debug.activate_debug ();
      Logs.set_level (Some Logs.Warning)
    ]}

    @raise {!exception Connection.Retry} on recoverable errors like the remote endpoint closing
    the connection gracefully. Even non-idempotent requests are
    guaranteed to not have been processed by the remote endpoint and
    should be retried. But beware that a [`Stream] [body] may have been
    consumed. *)
type requester = ?body:Body.t -> Cohttp.Request.t -> (Cohttp.Response.t * Body.t)

(** The [Connection] module handles a single, possibly pipelined, http
    connection. *)
module type Connection = sig
  module Net : Net

  exception Retry
  type t

  (** [create ?finalise ?persistent ?ctx endp] connects to [endp]. The
      connection handle may be used immediately, although the connection
      may not yet be established.
      @param finalise called when the connection is closed, but before
      still waiting requests are failed.
      @param persistent if [false], a [Connection: close] header is sent and
      the connection closed as soon as possible. If [true], it is
      assumed the remote end does support pipelining and multiple
      requests may be sent even before receiving any reply. By default
      we wait for the first response to decide whether connection
      keep-alive and pipelining is suppored.
      @param ctx See [Net.ctx]
      @param endp The remote address, port and protocol to connect to.
  *)
  val create :
    ?finalise:(t -> unit Net.IO.t) ->
    ?persistent:bool ->
    ?ctx:Net.ctx ->
    Net.endp ->
    t

  (** Same as [create], but returns d promise which gets fulfilled when
      the connection is established or rejected when connecting fails. *)
  val connect :
    ?finalise:(t -> unit Net.IO.t) ->
    ?persistent:bool ->
    ?ctx:Net.ctx ->
    Net.endp ->
    t Net.IO.t

  (** Send {e EOF}.
      On {e TCP} connections send a {e FIN} packet.
      On {e TLS} connections send a {e close notify}.
      No new requests can be sent afterwards, but responses may still
      be received. *)
  val shutdown : t -> unit

  (** Immediately close connection. All outstanding requests will fail,
      but non-idempotent requests that already went out on the wire may
      have produced side-effects. *)
  val close : t -> unit

  (** If [is_closed connection] is [false] the [connection] still
      accepts new requests. *)
  val is_closed : t -> bool

  (** Number of unfulfilled requests. This includes requests already
      sent out and requests still waitung to be sent. *)
  val length : t -> int

  (** Request notification on change of [length] and on closing. *)
  val notify : t -> unit Net.IO.t

  (** Queue a request. Please see {!type:requester}. *)
  val request : t ->
    ?body:Body.t -> Cohttp.Request.t -> (Cohttp.Response.t * Body.t) Net.IO.t
end

module type Connection_cache = sig
  module IO : IO

  type t

  (** Process a request. Please see {!type:requester}. *)
  val request : t ->
    ?body:Body.t -> Cohttp.Request.t -> (Cohttp.Response.t * Body.t) IO.t
end

(** The [Client] module is a collection of convenience functions for
    constructing and processing requests. *)
module type Client = sig
  type ctx

  (** Provide a function used to process requests.
      Please see {!type:requester}.
      The provided function is only used when no [ctx] argument is
      passed to the convenience functions below. *)
  val set_cache :
    (?body:Body.t -> Cohttp.Request.t -> (Cohttp.Response.t * Body.t) Lwt.t) ->
    unit

  (** [request ?ctx ?body request] processes a request.
      Please see {!type:requester}.
      @param ctx If provided, this is
      {!val:Connection_cache.Make_no_cache.create}. *)
  val request :
    ?ctx:ctx ->
    ?body:Body.t -> Cohttp.Request.t -> (Cohttp.Response.t * Body.t) Lwt.t

  (** [call ?ctx ?headers ?body ?chunked meth uri]
      constructs a {!module:Request} using provided [headers],
      [chunked], [meth] and [uri]. Then passes this {e request} and the
      [body] to {!val:request}.

      @return [(response, response_body)] Consume [response_body] in a
      timely fashion. Please see {!val:requester} about how and why.

      @param chunked use chunked encoding if [true]. The default is
      [false] for compatibility reasons.
      @param ctx If provided, {!val:request} is not used, but
      {!val:Connection_cache.Make_no_cache.create} is used to resolve
      uri and create a dedicated connection with [ctx].

      In most cases you should use the more specific helper calls in the
      interface rather than invoke this function directly. See {!head}, {!get}
      and {!post} for some examples.
  *)
  val call :
    ?ctx:ctx ->
    ?headers:Http.Header.t ->
    ?body:Body.t ->
    ?chunked:bool ->
    Http.Method.t ->
    Uri.t ->
    (Http.Response.t * Body.t) Lwt.t

  val head :
    ?ctx:ctx -> ?headers:Http.Header.t -> Uri.t -> Http.Response.t Lwt.t

  val get :
    ?ctx:ctx ->
    ?headers:Http.Header.t ->
    Uri.t ->
    (Http.Response.t * Body.t) Lwt.t

  val delete :
    ?ctx:ctx ->
    ?body:Body.t ->
    ?chunked:bool ->
    ?headers:Http.Header.t ->
    Uri.t ->
    (Http.Response.t * Body.t) Lwt.t

  val post :
    ?ctx:ctx ->
    ?body:Body.t ->
    ?chunked:bool ->
    ?headers:Http.Header.t ->
    Uri.t ->
    (Http.Response.t * Body.t) Lwt.t

  val put :
    ?ctx:ctx ->
    ?body:Body.t ->
    ?chunked:bool ->
    ?headers:Http.Header.t ->
    Uri.t ->
    (Http.Response.t * Body.t) Lwt.t

  val patch :
    ?ctx:ctx ->
    ?body:Body.t ->
    ?chunked:bool ->
    ?headers:Http.Header.t ->
    Uri.t ->
    (Http.Response.t * Body.t) Lwt.t

  val post_form :
    ?ctx:ctx ->
    ?headers:Http.Header.t ->
    params:(string * string list) list ->
    Uri.t ->
    (Http.Response.t * Body.t) Lwt.t

  (** @deprecated use a [!Connection] instead. *)
  val callv :
    ?ctx:ctx ->
    Uri.t ->
    (Http.Request.t * Body.t) Lwt_stream.t ->
    (Http.Response.t * Body.t) Lwt_stream.t Lwt.t
end

(** The [Server] module implements a pipelined HTTP/1.1 server. *)
module type Server = sig
  module IO : IO

  type conn = IO.conn * Cohttp.Connection.t [@@warning "-3"]

  type response_action =
    [ `Expert of Http.Response.t * (IO.ic -> IO.oc -> unit Lwt.t)
    | `Response of Http.Response.t * Body.t ]
  (** A request handler can respond in two ways:

      - Using [`Response], with a {!Response.t} and a {!Body.t}.
      - Using [`Expert], with a {!Response.t} and an IO function that is
        expected to write the response body. The IO function has access to the
        underlying {!IO.ic} and {!IO.oc}, which allows writing a response body
        more efficiently, stream a response or to switch protocols entirely
        (e.g. websockets). Processing of pipelined requests continue after the
        {!unit Lwt.t} is resolved. The connection can be closed by closing the
        {!IO.ic}. *)

  type t

  val make_response_action :
    ?conn_closed:(conn -> unit) ->
    callback:(conn -> Http.Request.t -> Body.t -> response_action Lwt.t) ->
    unit ->
    t

  val make_expert :
    ?conn_closed:(conn -> unit) ->
    callback:
      (conn ->
      Http.Request.t ->
      Body.t ->
      (Http.Response.t * (IO.ic -> IO.oc -> unit Lwt.t)) Lwt.t) ->
    unit ->
    t

  val make :
    ?conn_closed:(conn -> unit) ->
    callback:
      (conn -> Http.Request.t -> Body.t -> (Http.Response.t * Body.t) Lwt.t) ->
    unit ->
    t

  val resolve_local_file : docroot:string -> uri:Uri.t -> string
    [@@deprecated "Please use Cohttp.Path.resolve_local_file. "]
  (** Resolve a URI and a docroot into a concrete local filename. *)

  val respond :
    ?headers:Http.Header.t ->
    ?flush:bool ->
    status:Http.Status.t ->
    body:Body.t ->
    unit ->
    (Http.Response.t * Body.t) Lwt.t
  (** [respond ?headers ?flush ~status ~body] will respond to an HTTP request
      with the given [status] code and response [body]. If [flush] is true, then
      every response chunk will be flushed to the network rather than being
      buffered. [flush] is true by default. The transfer encoding will be
      detected from the [body] value and set to chunked encoding if it cannot be
      determined immediately. You can override the encoding by supplying an
      appropriate [Content-length] or [Transfer-encoding] in the [headers]
      parameter. *)

  val respond_string :
    ?flush:bool ->
    ?headers:Http.Header.t ->
    status:Http.Status.t ->
    body:string ->
    unit ->
    (Http.Response.t * Body.t) Lwt.t

  val respond_error :
    ?headers:Http.Header.t ->
    ?status:Http.Status.t ->
    body:string ->
    unit ->
    (Http.Response.t * Body.t) Lwt.t

  val respond_redirect :
    ?headers:Http.Header.t ->
    uri:Uri.t ->
    unit ->
    (Http.Response.t * Body.t) Lwt.t

  val respond_need_auth :
    ?headers:Http.Header.t ->
    auth:Cohttp.Auth.challenge ->
    unit ->
    (Http.Response.t * Body.t) Lwt.t

  val respond_not_found : ?uri:Uri.t -> unit -> (Http.Response.t * Body.t) Lwt.t
  val callback : t -> IO.conn -> IO.ic -> IO.oc -> unit Lwt.t
end
