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

(* Create a socket for the uri, and signal whether it requires https *)
let scheme_conn_of_uri ~sw net uri =
  match Uri.scheme uri with
  | Some "httpunix" ->
      (* FIXME: while there is no standard, http+unix seems more widespread *)
      `Plain (Eio.Net.connect ~sw net (unix_address uri) :> t)
  | Some "http" -> `Plain (Eio.Net.connect ~sw net (tcp_address ~net uri) :> t)
  | Some "https" -> `Https (Eio.Net.connect ~sw net (tcp_address ~net uri) :> t)
  | x ->
      Fmt.failwith "Unknown scheme %a"
        Fmt.(option ~none:(any "None") Dump.string)
        x

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

let make ~sw ~https net proxy uri : t =
  let scheme_conn =
    match proxy with
    | None -> scheme_conn_of_uri ~sw net uri
    | Some (Proxy.Direct proxy_uri) -> scheme_conn_of_uri ~sw net proxy_uri
    | Some (Proxy.Tunnel (proxy_headers, proxy_uri)) -> (
        let conn =
          match scheme_conn_of_uri ~sw net proxy_uri with
          | `Plain socket -> socket
          | `Https socket -> apply_https https uri socket
        in
        match make_tunnel ~sw ~headers:proxy_headers uri conn with
        | Ok () ->
            (* we know its an https connection, because we have selected a tunnelling proxy *)
            `Https conn
        | Error status ->
            Fmt.failwith
              "Proxy could not form tunnel to %a for host %a; status %a" Uri.pp
              proxy_uri Uri.pp uri Http.Status.pp status)
  in
  match scheme_conn with
  | `Plain conn -> conn
  | `Https conn -> apply_https https uri conn
