(** [Server] is a HTTP 1.1 server. *)

type handler = Request.server_request -> Response.server_response

val run :
  ?backlog:int ->
  ?domains:int ->
  port:int ->
  on_error:(exn -> unit) ->
  #Eio.Domain_manager.t ->
  #Eio.Net.t ->
  #Eio.Time.clock ->
  handler ->
  'c
(** [run ~port on_error domain_mgr net clock handler] runs a HTTP/1.1 server
    executing [handler] and listening on [port].

    @param backlog
      is the number of pending connections for tcp server socket. The default is
      [128].
    @param domains
      is the number of OCaml 5.0 domains the server will use. The default is
      [1]. You may use {!val:Domain.recommended_domain_count} to configure a
      multicore capable server. *)

val connection_handler :
  handler ->
  #Eio.Time.clock ->
  #Eio.Net.stream_socket ->
  Eio.Net.Sockaddr.stream ->
  unit
(** [connection_handler request_handler client_addr conn] is a connection
    handler, suitable for passing to {!Eio.Net.accept_fork}. *)

(** {1 Basic Handlers} *)

val not_found_handler : handler
(** [not_found_handler] return HTTP 404 response. *)
