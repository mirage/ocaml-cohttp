type response = Response.t * Body.t [@@deriving sexp_of]

type 'r respond_t =
  ?flush:bool ->
  ?headers:Cohttp.Header.t ->
  ?body:Body.t ->
  Cohttp.Code.status_code -> 'r Async_kernel.Deferred.t

(** A request handler can respond in two ways:
    - Using [`Response], with a {!Response.t} and a {!Body.t}.
    - Using [`Expert], with a {!Response.t} and an IO function that is expected
      to write the response body. The IO function has access to the underlying
      {!Async_unix.Reader.t} and {!Async_unix.Writer.t}, which allows writing a
      response body more efficiently, stream a response or to switch protocols
      entirely (e.g. websockets). Processing of pipelined requests continue
      after the {!unit Async_kernel.Deferred.t} is resolved. The connection can
      be closed by closing the {!Async_unix.Reader.t}. *)
type response_action =
  [ `Expert of Cohttp.Response.t
               * (Async_unix.Reader.t
                  -> Async_unix.Writer.t
                  -> unit Async_kernel.Deferred.t)
  | `Response of response ]

val respond : response respond_t

(** Resolve a URI and a docroot into a concrete local filename. *)
val resolve_local_file : docroot:string -> uri:Uri.t -> string

(** Respond with a [string] Pipe that provides the response string
    Pipe.Reader.t.
    @param code Default is HTTP 200 `OK *)
val respond_with_pipe :
  ?flush:bool ->
  ?headers:Cohttp.Header.t -> ?code:Cohttp.Code.status_code ->
  string Async_kernel.Pipe.Reader.t -> response Async_kernel.Deferred.t

val respond_string :
  ?flush:bool ->
  ?headers:Cohttp.Header.t ->
  ?status:Cohttp.Code.status_code ->
  string -> response Async_kernel.Deferred.t

(** Respond with a redirect to an absolute [uri]
    @param uri Absolute URI to redirect the client to *)
val respond_with_redirect :
  ?headers:Cohttp.Header.t -> Uri.t -> response Async_kernel.Deferred.t


(** Respond with file contents, and [error_string Pipe.Async_unix.Reader.t] if the file isn't found *)
val respond_with_file :
  ?flush:bool ->
  ?headers:Cohttp.Header.t -> ?error_body:string ->
  string -> response Async_kernel.Deferred.t

(** Build a HTTP server and expose the [IO.ic] and [IO.oc]s, based on the
    [Tcp.Server] interface. *)
val create_expert :
  ?timeout:int ->
  ?backlog:int ->
  on_handler_error:[ `Call of Conduit_async.flow -> exn  -> unit
                   | `Ignore
                   | `Raise ] ->
  protocol:(_, 'flow) Conduit_async.protocol ->
  service:('cfg, 't, 'flow) Conduit_async.Service.service ->
  'cfg
  -> (body:Body.t -> Conduit_async.flow -> Request.t -> response_action Async_kernel.Deferred.t)
  -> unit Async.Condition.t * (unit -> unit Async.Deferred.t)


(** Build a HTTP server, based on the [Tcp.Server] interface *)
val create :
  ?timeout:int ->
  ?backlog:int ->
  on_handler_error:[ `Call of Conduit_async.flow -> exn  -> unit
                   | `Ignore
                   | `Raise ] ->
  protocol:(_, 'flow) Conduit_async.protocol ->
  service:('cfg, 't, 'flow) Conduit_async.Service.service ->
  'cfg
  -> (body:Body.t -> Conduit_async.flow -> Request.t -> response Async_kernel.Deferred.t)
  -> unit Async.Condition.t * (unit -> unit Async.Deferred.t)
