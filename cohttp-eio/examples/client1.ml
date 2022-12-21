open Cohttp_eio

let () =
  let host, port = ("www.example.org", 80) in
  Eio_main.run @@ fun env ->
  Eio.Net.with_tcp_connect ~host ~service:(string_of_int port) env#net
    (fun conn ->
      let host = (host, Some port) in
      let res = Client.get ~conn host "/" in
      print_string @@ Client.read_fixed res)
