open Eio
open Cohttp_eio

let () =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let hostname, port = ("www.example.org", 80) in
  let he = Unix.gethostbyname hostname in
  let addr = `Tcp (Eio_unix.Ipaddr.of_unix he.h_addr_list.(0), port) in
  let conn = Net.connect ~sw env#net addr in
  let host = (hostname, Some port) in
  let res = Client.get ~conn host "/" in
  print_string @@ Client.read_fixed res
