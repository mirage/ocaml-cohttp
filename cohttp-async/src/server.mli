open! Core
open! Async

type ('address, 'listening_on) t constraint 'address = [< Socket.Address.t ]
  [@@deriving sexp_of]

val close          : (_, _) t -> unit Deferred.t
val close_finished : (_, _) t -> unit Deferred.t
val is_closed      : (_, _) t -> bool

type response = Response.t * Body.t [@@deriving sexp_of]

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
  string -> response Deferred.t [@@deprecated "please use respond_string instead"]

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

(** Build a HTTP server, based on the [Tcp.Server] interface *)
val create :
  ?max_connections:int ->
  ?buffer_age_limit: Writer.buffer_age_limit ->
  ?on_handler_error:[ `Call of 'address -> exn  -> unit
                    | `Ignore
                    | `Raise ] ->
  ?mode:Conduit_async.server ->
  ('address, 'listening_on) Tcp.Where_to_listen.t
  -> (body:Body.t -> 'address -> Request.t -> response Deferred.t)
  -> ('address, 'listening_on) t Deferred.t
