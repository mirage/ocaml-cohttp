(** The [Server] module implements the full UNIX HTTP server interface,
    including the UNIX-specific functions defined in {!S}. *)

include Cohttp_lwt.S.Server with module IO = Io

val resolve_file : docroot:string -> uri:Uri.t -> string

val respond_file :
  ?headers:Cohttp.Header.t ->
  fname:string -> unit -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t

val create :
  ?timeout:int ->
  ?stop:unit Lwt.t ->
  ?on_exn:(exn -> unit) ->
  ?ctx:Net.ctx ->
  ?mode:Conduit_lwt_unix.server -> t -> unit Lwt.t
