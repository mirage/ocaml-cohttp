open Eio.Std
open Utils
module Proxy = Cohttp.Proxy.Forward

type proxies = (Uri.t, Uri.t) Proxy.servers
type proxy = (Uri.t, Http.Header.t option * Uri.t) Cohttp.Proxy.Forward.t
type t = [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] r
type call = t -> Http.Response.t * Eio.Flow.source_ty Eio.Resource.t

let call ~sw ?headers ?body ?(chunked = false) meth uri socket =
  let body_length =
    if chunked then None
    else
      match body with
      | None -> Some 0L
      | Some (Eio.Resource.T (body, ops)) ->
          let module X = (val Eio.Resource.get ops Eio.Flow.Pi.Source) in
          List.find_map
            (function
              | Body.String m -> Some (String.length (m body) |> Int64.of_int)
              | _ -> None)
            X.read_methods
  in
  let request =
    Cohttp.Request.make_for_client ?headers
      ~chunked:(Option.is_none body_length)
      ?body_length meth uri
  in
  Eio.Buf_write.with_flow socket @@ fun output ->
  let () =
    Eio.Fiber.fork ~sw @@ fun () ->
    Io.Request.write ~flush:false
      (fun writer ->
        match body with
        | None -> ()
        | Some body -> flow_to_writer body writer Io.Request.write_body)
      request output
  in
  let input = Eio.Buf_read.of_flow ~max_size:max_int socket in
  match Io.Response.read input with
  | `Eof -> failwith "connection closed by peer"
  | `Invalid reason -> failwith reason
  | `Ok response -> (
      match Cohttp.Response.has_body response with
      | `No -> (response, Eio.Flow.string_source "")
      | `Yes | `Unknown ->
          let body =
            let reader = Io.Response.make_body_reader response input in
            flow_of_reader (fun () -> Io.Response.read_body_chunk reader)
          in
          (response, body))

let unix_address uri =
  match Uri.host uri with
  | Some path -> `Unix path
  | None -> Fmt.failwith "no host specified (in %a)" Uri.pp uri

let tcp_address ~net uri =
  let service =
    match Uri.port uri with
    | Some port -> Int.to_string port
    | _ -> Uri.scheme uri |> Option.value ~default:"http"
  in
  match
    Eio.Net.getaddrinfo_stream ~service net
      (Uri.host_with_default ~default:"localhost" uri)
  with
  | ip :: _ -> ip
  | [] -> failwith "failed to resolve hostname"

type sockaddr =
  | Plain of Eio.Net.Sockaddr.stream
  | Https of Eio.Net.Sockaddr.stream

let to_sockaddr = function Plain addr | Https addr -> addr

let address_of_uri net uri : sockaddr =
  match Uri.scheme uri with
  | Some "httpunix" ->
      (* FIXME: while there is no standard, http+unix seems more widespread *)
      Plain (unix_address uri)
  | Some "http" -> Plain (tcp_address ~net uri)
  | Some "https" -> Https (tcp_address ~net uri)
  | x ->
      Fmt.failwith "Unknown scheme %a"
        Fmt.(option ~none:(any "None") Dump.string)
        x

type address_info =
  | Direct of sockaddr * Uri.t
  | Tunnelled of {
      proxy_headers : Http.Header.t option;
      proxy_uri : Uri.t;
      proxy_address : sockaddr;
      remote_uri : Uri.t;
      remote_address : sockaddr;
    }

let address_info net proxy uri : address_info =
  match proxy with
  | None -> Direct (address_of_uri net uri, uri)
  | Some (Proxy.Direct proxy_uri) ->
      Direct (address_of_uri net proxy_uri, proxy_uri)
  | Some (Proxy.Tunnel (proxy_headers, proxy_uri)) ->
      Tunnelled
        {
          proxy_headers;
          proxy_uri;
          proxy_address = address_of_uri net proxy_uri;
          remote_uri = uri;
          remote_address = address_of_uri net uri;
        }

let to_address = function
  | Direct (addr, _) -> to_sockaddr addr
  | Tunnelled t -> to_sockaddr t.remote_address

(* Create a tunnel to the proxy at [proxy_uri] *)
let make_tunnel ~sw ~headers proxy_uri socket =
  let resp, _ = call ~sw ?headers `CONNECT proxy_uri socket in
  match Http.Response.status resp with
  | #Http.Status.success -> Ok ()
  | _ -> Error (Http.Response.status resp)

(* Apply the https wrapper, if provided, or else fail with an error *)
let apply_https https uri conn =
  match https with
  | None -> Fmt.failwith "HTTPS not enabled (for %a)" Uri.pp uri
  | Some wrap -> (wrap uri conn :> t)

let make ~sw ~https net address_info : t =
  match address_info with
  | Direct (Plain conn, _) -> (Eio.Net.connect ~sw net conn :> t)
  | Direct (Https conn, uri) ->
      apply_https https uri (Eio.Net.connect ~sw net conn :> t)
  | Tunnelled
      { proxy_headers; proxy_uri; proxy_address; remote_uri; remote_address }
    -> (
      let conn =
        match proxy_address with
        | Plain addr -> (Eio.Net.connect ~sw net addr :> t)
        | Https addr ->
            apply_https https proxy_uri (Eio.Net.connect ~sw net addr :> t)
      in
      match make_tunnel ~sw ~headers:proxy_headers remote_uri conn with
      | Ok () -> (
          match remote_address with
          | Plain _ ->
              failwith "impossible: we do not tunnel for plain connections"
          | Https _ -> apply_https https remote_uri conn)
      | Error status ->
          Fmt.failwith
            "Proxy could not form tunnel to proxy %a for host %a; status %a"
            Uri.pp proxy_uri Uri.pp remote_uri Http.Status.pp status)
