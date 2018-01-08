open! Core
open! Async

type ('address, 'listening_on) t constraint 'address = [< Socket.Address.t ]
  [@@deriving sexp_of]

val close          : (_, _) t -> unit Deferred.t
val close_finished : (_, _) t -> unit Deferred.t
val is_closed      : (_, _) t -> bool

val listening_on   : (_, 'listening_on) t -> 'listening_on

type response = Response.t * Body.t [@@deriving sexp_of]

type 'r respond_t =
  ?flush:bool ->
  ?headers:Cohttp.Header.t ->
  ?body:Body.t ->
  Cohttp.Code.status_code -> 'r Deferred.t

type response_action =
  (* The connection is not closed in the [`Expert] case until the [unit
     Deferred.t] becomes determined. *)
  [ `Expert of Cohttp.Response.t * (Reader.t -> Writer.t -> unit Deferred.t)
  | `Response of response ]

val respond : response respond_t

(** Resolve a URI and a docroot into a concrete local filename. *)
val resolve_local_file : docroot:string -> uri:Uri.t -> string

(** Respond with a [string] Pipe that provides the response string Pipe.Reader.t.
    @param code Default is HTTP 200 `OK *)
val respond_with_pipe :
  ?flush:bool ->
  ?headers:Cohttp.Header.t -> ?code:Cohttp.Code.status_code ->
  string Pipe.Reader.t -> response Deferred.t

val respond_string :
  ?flush:bool ->
  ?headers:Cohttp.Header.t ->
  ?status:Cohttp.Code.status_code ->
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

type mode = Conduit_async.server

(** Build a HTTP server and expose the [IO.ic] and [IO.oc]s, based on the
    [Tcp.Server] interface. *)
val create_expert :
  ?max_connections:int ->
  ?backlog:int ->
  ?buffer_age_limit:Writer.buffer_age_limit ->
  ?mode:mode ->
  on_handler_error:[ `Call of 'address -> exn  -> unit
                   | `Ignore
                   | `Raise ] ->
  ('address, 'listening_on) Tcp.Where_to_listen.t
  -> (body:Body.t -> 'address -> Request.t -> response_action Deferred.t)
  -> ('address, 'listening_on) t Deferred.t


(** Build a HTTP server, based on the [Tcp.Server] interface *)
val create :
  ?max_connections:int ->
  ?backlog:int ->
  ?buffer_age_limit: Writer.buffer_age_limit ->
  ?mode:Conduit_async.server ->
  on_handler_error:[ `Call of 'address -> exn  -> unit
                   | `Ignore
                   | `Raise ] ->
  ('address, 'listening_on) Tcp.Where_to_listen.t
  -> (body:Body.t -> 'address -> Request.t -> response Deferred.t)
  -> ('address, 'listening_on) t Deferred.t
