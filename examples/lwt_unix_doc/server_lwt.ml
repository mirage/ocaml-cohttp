open Lwt
open Cohttp
open Cohttp_lwt_unix

let server =
  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.to_string in
    let meth = req |> Request.meth |> Code.string_of_method in
    let headers = req |> Request.headers |> Header.to_string in
    ( body |> Cohttp_lwt.Body.to_string >|= fun body ->
      Printf.sprintf "Uri: %s\nMethod: %s\nHeaders\nHeaders: %s\nBody: %s" uri
        meth headers body )
    >>= fun body -> Server.respond_string ~status:`OK ~body ()
  in
  let tcp_config =
    {
      Conduit_lwt.TCP.sockaddr = Unix.ADDR_INET (Unix.inet_addr_loopback, 8000);
      capacity = 40;
    }
  in
  Server.create tcp_config Conduit_lwt.TCP.protocol Conduit_lwt.TCP.service
    (Server.make ~callback ())

let _ = Lwt_main.run (server ())
