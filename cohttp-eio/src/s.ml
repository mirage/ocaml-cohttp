type connection = Eio.Flow.two_way_ty Eio.Std.r
type t = sw:Eio.Switch.t -> Uri.t -> connection

type cache_call =
  (sw:Eio.Switch.t -> Uri.t -> connection) ->
  sw:Eio.Switch.t ->
  ?headers:Http.Header.t ->
  ?body:Body.t ->
  ?chunked:bool ->
  ?absolute_form:bool ->
  Http.Method.t ->
  Uri.t ->
  Cohttp.Response.t * Body.t
(** [call ?headers ?body method uri] Function type used to handle http requests

    @return
      [(response, response_body)] [response_body] is not buffered, but stays on
      the wire until consumed. It must therefore be consumed in a timely manner.
      Otherwise the connection would stay open and a file descriptor leak may be
      caused. Following responses would get blocked. Functions in the {!Body}
      module can be used to consume [response_body]. Use {!Body.drain_body} if
      you don't consume the body by other means.

    Leaks are detected by the GC and logged as debug messages, these can be
    enabled activating the debug logging. For example, this can be done as
    follows in [cohttp-lwt-unix]

    {[
      Cohttp_lwt_unix.Debug.activate_debug ();
      Logs.set_level (Some Logs.Warning)
    ]}

    @raise {!Connection.Retry}
      on recoverable errors like the remote endpoint closing the connection
      gracefully. Even non-idempotent requests are guaranteed to not have been
      processed by the remote endpoint and should be retried. But beware that a
      [`Stream] [body] may have been consumed. *)

(** A [Connection_cache] handles http requests. It not necessarily caches
    connections. *)
module type Connection_cache = sig
  type t

  val call : t -> cache_call
  (** Process a request. Please see {!type:call}. *)
end
