open Cohttp_eio

let length = 2053
let text = String.make length 'a'
let headers = Cohttp.Header.of_list [ ("content-length", Int.to_string length) ]

let server_callback _conn _req _body =
  Server.respond_string ~headers ~status:`OK ~body:text ()

let () =
  let port = ref 8080 in
  Arg.parse
    [ ("-p", Arg.Set_int port, " Listening port number(8080 by default)") ]
    ignore "An HTTP/1.1 server";
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let socket =
    Eio.Net.listen env#net ~sw ~backlog:11_000 ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, !port))
  and server = Cohttp_eio.Server.make ~callback:server_callback () in
  Cohttp_eio.Server.run socket server ~on_error:raise
