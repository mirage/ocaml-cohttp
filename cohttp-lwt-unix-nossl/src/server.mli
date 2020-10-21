(** The [Server] module implements the full UNIX HTTP server interface,
    including the UNIX-specific functions defined in {!S}. *)

include Cohttp_lwt.S.Server with module IO = Io

val resolve_file : docroot:string -> uri:Uri.t -> string

val respond_file :
  ?headers:Cohttp.Header.t ->
  fname:string -> unit -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t


(** [create ?timeout ?backlog ?stop ?on_exn cfg protocol service t] is a new
   HTTP server.

    The user can decide to start a simple HTTP server (without encryption)
   or one with TLS encryption. It depends on what the user gives as [cfg],
   [protocol] and [service]. Using [conduit-lwt-tls], the end-user is able
   to make an encrypted HTTP server with:

    {[
      let run =
        create cfg Conduit_lwt_tls.TCP.protocol Conduit_lwt_tls.TCP.service
    ]}

    A simple HTTP server (with [conduit-lwt]) is:

    {[
      let run =
        create cfg Conduit_lwt.TCP.protocol Conduit_lwt.TCP.service
    ]}

    [cfg] depends on the given [service] - and let the user to define which
   port the server use, and, in the case of {!Conduit_lwt_tls.TCP.service},
   which TLS certificate it uses. See [Conduit] for more information about
   {i protocol} and {i service}.

    When provided, the [stop] thread will terminate the server if it
   ever becomes determined.

    When provided, [backlog] will limit the number of open
   connections.

    Every connection will be served in a new lightweight thread that
   is invoked via the callback defined in [t]. If the callback raises
   an exception, it is passed to [on_exn] (by default, to a function
   that logs the exception using the {!Logs} library). *)
val create :
  ?timeout:int ->
  ?backlog:int ->
  ?stop:unit Lwt.t ->
  ?on_exn:(exn -> unit) ->
  'cfg ->
  (_, 'flow) Conduit_lwt.protocol ->
  ('cfg, 't, 'flow) Conduit_lwt.Service.service -> t -> (unit -> unit Lwt.t)
