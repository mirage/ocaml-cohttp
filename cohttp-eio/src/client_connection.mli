(** Internal API over socket connections for the client *)

type proxies = (Uri.t, Uri.t) Cohttp.Proxy.Forward.servers

type proxy = (Uri.t, Http.Header.t option * Uri.t) Cohttp.Proxy.Forward.t
(** A proxy to a [Direct uri], or a [Tunnel (proxy_headers, uri)]*)

type t = [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Std.r
(** Connections are represented as Eio flows, which usually are over system
    sockets *)

type call = t -> Http.Response.t * Eio.Flow.source_ty Eio.Resource.t
(** A call on a connection *)

val call :
  sw:Eio.Switch.t ->
  ?headers:Http.Header.t ->
  ?body:[> Eio.Flow.source_ty ] Eio.Resource.t ->
  ?chunked:bool ->
  Cohttp.Code.meth ->
  Uri.t ->
  call
(** Execute a call on a connection *)

type address_info
(** TODO doc*)

val address_info :
  [> [> `Generic ] Eio.Net.ty ] Eio.Std.r ->
  proxy option ->
  Uri.t ->
  address_info
(** TODO doc*)

val to_address : address_info -> Eio.Net.Sockaddr.stream
(** TODO doc*)

val make :
  sw:Eio.Switch.t ->
  https:
    (Uri.t ->
    [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Std.r ->
    [> Eio.Resource.close_ty ] Eio.Flow.two_way)
    option ->
  _ Eio.Net.t ->
  address_info ->
  t
(** Create a new connection *)
