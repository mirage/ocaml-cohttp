open Eio.Std
module Proxy = Cohttp.Proxy.Forward
module Connection = Client_connection

type t = sw:Switch.t -> Uri.t -> Connection.t

let proxies : (Http.Header.t option * Connection.proxies) option Atomic.t =
  Atomic.make None

let set_proxies ?no_proxy_patterns ?default_proxy ?(scheme_proxies = [])
    ?proxy_headers () =
  let servers =
    Proxy.make_servers ~no_proxy_patterns ~default_proxy ~scheme_proxies
      ~direct:Fun.id ~tunnel:Fun.id
  in
  Atomic.set proxies (Some (proxy_headers, servers))

let get_proxy uri : Connection.proxy option =
  match Atomic.get proxies with
  | None -> None
  | Some (headers, proxies) -> (
      match Proxy.get proxies uri with
      | None -> None
      | Some (Proxy.Direct _) as proxy -> proxy
      | Some (Proxy.Tunnel p) -> Some (Proxy.Tunnel (headers, p)))

include
  Cohttp.Generic.Client.Make
    (struct
      type 'a io = 'a
      type body = Body.t
      type 'a with_context = t -> sw:Eio.Switch.t -> 'a

      let map_context v f t ~sw = f (v t ~sw)

      let call (t : t) ~sw ?headers ?body ?(chunked = false) meth uri =
        let conn = t ~sw uri in
        Connection.call ~sw ?headers ?body ~chunked meth uri conn
    end)
    (Io.IO)

let make_generic fn = (fn :> t)

let make ~https net : t =
 fun ~sw uri ->
  let proxy = get_proxy uri in
  let addr_info = Connection.address_info net proxy uri in
  Connection.make ~sw ~https net addr_info
