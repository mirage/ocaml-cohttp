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

  type ctx [@@deriving sexp_of]
  (** Conduit context. Contains configuration of resolver, local source address,
      TLS / SSL library, certificates, keys.

      Depending on [ctx], the library is able to send HTTP requests unencrypted
      or encrypted one with a secured protocol (such as TLS). Depending on how
      conduit is configured, [ctx] might initiate a secured connection with TLS
      (using [ocaml-tls]) or SSL (using [ocaml-ssl]), on [*:443] or on the
      specified port by the user. If neitehr [ocaml-tls] or [ocaml-ssl] are
      installed on the system, [cohttp]/[conduit] tries the usual ([*:80]) or
      the specified port by the user in a non-secured way. *)

  val default_ctx : ctx

  val resolve : ctx:ctx -> Uri.t -> endp IO.t
  (** [resolve ~ctx uri] resolves [uri] into an endpoint description. This is
      [Resolver_lwt.resolve_uri ~uri ctx.resolver]. *)

  val connect_uri : ctx:ctx -> Uri.t -> (IO.conn * IO.ic * IO.oc) IO.t
  (** [connect_uri ~ctx uri] starts a {i flow} on the given [uri]. The choice of
      the protocol (with or without encryption) is done by the {i scheme} of the
      given [uri]:

      - If the scheme is [https], we will {b extend} [ctx] to be able to start a
        TLS connection with a default TLS configuration (no authentication) on
        the default or user-specified port.
      - If the scheme is [http], we will {b extend} [ctx] to be able to start a
        simple TCP/IP connection on the default or user-specified port.

      These extensions have the highest priority ([Conduit] will try to initiate
      a communication with them first). By {i extension}, we mean that the user
      is able to fill its own [ctx] and we don't overlap resolution functions
      from the given [ctx].

      This is [resolve ~ctx uri >>= connect_endp ~ctx]. *)

  val connect_endp : ctx:ctx -> endp -> (IO.conn * IO.ic * IO.oc) IO.t
  (** [connect_endp ~ctx endp] starts a {i flow} to the given [endp]. [endp]
      describes address and protocol of the endpoint to connect to. *)

  val close_in : IO.ic -> unit
  val close_out : IO.oc -> unit
  val close : IO.ic -> IO.oc -> unit
end

(** This is compatible with [Mirage_time.S]. It may be satisfied by
    mirage-time-unix [Time] or [Mirage_time]. *)
module type Sleep = sig
  val sleep_ns : int64 -> unit Lwt.t
end

type call =
  ?headers:Http.Header.t ->
  ?body:Body.t ->
  Http.Method.t ->
  Uri.t ->
  (Cohttp.Response.t * Body.t) Lwt.t
(** [call ?headers ?body method uri]
    Function type used to handle http requests

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

    @raise {!exception Connection.Retry} on recoverable errors like the remote
    endpoint closing
    the connection gracefully. Even non-idempotent requests are
    guaranteed to not have been processed by the remote endpoint and
    should be retried. But beware that a [`Stream] [body] may have been
    consumed. *)

(** The [Connection] module handles a single, possibly pipelined, http
    connection. *)
module type Connection = sig
  module Net : Net

  exception Retry

  type t

  val create :
    ?finalise:(t -> unit Net.IO.t) ->
    ?persistent:bool ->
    ?ctx:Net.ctx ->
    Net.endp ->
    t
  (** [create ?finalise ?persistent ?ctx endp] connects to [endp]. The
      connection handle may be used immediately, although the connection may not
      yet be established.

      @param finalise
        called when the connection is closed, but before still waiting requests
        are failed.
      @param persistent
        if [false], a [Connection: close] header is sent and the connection
        closed as soon as possible. If [true], it is assumed the remote end does
        support pipelining and multiple requests may be sent even before
        receiving any reply. By default we wait for the first response to decide
        whether connection keep-alive and pipelining is suppored. Chunked
        encoding can only be used when pipelining is supported. Therefore better
        avoid using chunked encoding on the very first request.
      @param ctx See [Net.ctx]
      @param endp The remote address, port and protocol to connect to. *)

  val connect :
    ?finalise:(t -> unit Net.IO.t) ->
    ?persistent:bool ->
    ?ctx:Net.ctx ->
    Net.endp ->
    t Net.IO.t
  (** Same as [create], but returns d promise which gets fulfilled when the
      connection is established or rejected when connecting fails. *)

  val shutdown : t -> unit
  (** Send {e EOF}. On {e TCP} connections send a {e FIN} packet. On {e TLS}
      connections send a {e close notify}. No new requests can be sent
      afterwards, but responses may still be received. *)

  val close : t -> unit
  (** Immediately close connection. All outstanding requests will fail, but
      non-idempotent requests that already went out on the wire may have
      produced side-effects. *)

  val is_closed : t -> bool
  (** If [is_closed connection] is [false] the [connection] still accepts new
      requests. *)

  val length : t -> int
  (** Number of unfulfilled requests. This includes requests already sent out
      and requests still waitung to be sent. *)

  val notify : t -> unit Net.IO.t
  (** Request notification on change of [length] and on closing. *)

  val call : t -> call
  (** Queue a request. Please see {!type:requester}. *)
end

(** A [Connection_cache] handles http requests. It not necessarily caches
    connections. *)
module type Connection_cache = sig
  type t

  val call : t -> call
  (** Process a request. Please see {!type:call}. *)
end

(** The [Client] module is a collection of convenience functions for
    constructing and processing requests. *)
module type Client = sig
  type ctx

  (** @param ctx
        If provided, no connection cache is used, but
        {!val:Connection_cache.Make_no_cache.create} is used to resolve uri and
        create a dedicated connection with [ctx].

        In most cases you should use the more specific helper calls in the
        interface rather than invoke this function directly. See {!head}, {!get}
        and {!post} for some examples. *)
  include
    Cohttp.Client.S
      with type 'a io = 'a Lwt.t
       and type body = Body.t
       and type 'a with_context = ?ctx:ctx -> 'a

  val set_cache : call -> unit
  (** Provide a function used to process requests. Please see {!type:call}. The
      provided function is only used when no [ctx] argument is passed to the
      convenience functions below. *)

  val post_form :
    ?ctx:ctx ->
    ?headers:Http.Header.t ->
    params:(string * string list) list ->
    Uri.t ->
    (Http.Response.t * Body.t) Lwt.t

  val callv :
    ?ctx:ctx ->
    Uri.t ->
    (Http.Request.t * Body.t) Lwt_stream.t ->
    (Http.Response.t * Body.t) Lwt_stream.t Lwt.t
  (** @deprecated use {!module Cohttp_lwt.Connection} instead. *)
end

(** The [Server] module implements a pipelined HTTP/1.1 server. *)
module type Server = sig
  include Cohttp.Server.S with type body = Body.t and type 'a IO.t = 'a Lwt.t

  val resolve_local_file : docroot:string -> uri:Uri.t -> string
    [@@deprecated "Please use Cohttp.Path.resolve_local_file. "]
  (** Resolve a URI and a docroot into a concrete local filename. *)

  val respond_error :
    ?headers:Http.Header.t ->
    ?status:Http.Status.t ->
    body:string ->
    unit ->
    (Http.Response.t * body) IO.t

  val respond_redirect :
    ?headers:Http.Header.t -> uri:Uri.t -> unit -> (Http.Response.t * body) IO.t

  val respond_need_auth :
    ?headers:Http.Header.t ->
    auth:Cohttp.Auth.challenge ->
    unit ->
    (Http.Response.t * body) IO.t

  val respond_not_found : ?uri:Uri.t -> unit -> (Http.Response.t * body) IO.t
end
