open Eio
open Cohttp_eio

let conn env sw resource_path =
  let hostname, port = ("www.example.org", 80) in
  let he = Unix.gethostbyname hostname in
  let addr = `Tcp (Eio_unix.Ipaddr.of_unix he.h_addr_list.(0), port) in
  let flow = Net.connect ~sw env#net addr in
  let host = (hostname, Some port) in
  (resource_path, host, flow)

let () =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let res = Client.get (conn env sw) "/" in
  match Client.read_fixed res with Some b -> print_string b | None -> ()
