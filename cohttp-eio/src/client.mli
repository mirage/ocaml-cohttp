open Eio.Std

type t

include
  Cohttp.Generic.Client.S
    with type 'a with_context = t -> sw:Switch.t -> 'a
     and type 'a io = 'a
     and type body = Body.t

val make :
  https:
    (Uri.t ->
    [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Std.r ->
    [> Eio.Resource.close_ty ] Eio.Flow.two_way)
    option ->
  _ Eio.Net.t ->
  t
(** [make ~https net] is a convenience wrapper around {!make_generic} that uses
    [net] to make connections.

    - URIs of the form "http://host:port/..." connect to the given TCP host and
      port.
    - URIs of the form "https://host:port/..." connect to the given TCP host and
      port, and are then wrapped by [https] (or rejected if that is [None]).
    - URIs of the form "httpunix://unix-path/http-path" connect to the given
      Unix path. *)

val make_generic :
  (sw:Switch.t -> Uri.t -> [> Eio.Resource.close_ty ] Eio.Flow.two_way) -> t
(** [make_generic connect] is an HTTP client that uses [connect] to get the
    connection to use for a given URI. *)

val set_proxies :
  ?no_proxy_patterns:string ->
  ?default_proxy:Uri.t ->
  ?scheme_proxies:(string * Uri.t) list ->
  ?proxy_headers:Http.Header.t ->
  unit ->
  unit
(** [set_proxies ~default_proxy ()] configures the proxy to use for any
    subsequent clients created via {!val:make}.

    See {!val:Cohttp.Proxy.Forward.make_servers} for the meaning of the
    parameters. *)
