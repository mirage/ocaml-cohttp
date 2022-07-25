open Eio
open Cohttp_eio

let connect_info url =
  let uri  = Uri.of_string url in
  let host = Uri.host uri |> Option.get in
  let port = Uri.port uri |> Option.value ~default:80 in
  let path = Uri.of_string (Uri.path uri) in
  let addr =
    let he = Unix.gethostbyname host in
    he.h_addr_list.(0)
  in
  (Eio_unix.Ipaddr.of_unix addr, port, path)

let () =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let (addr, port, path) = connect_info "http://www.reddit.com/" in
  let res =
    Client.get
      ~headers:(Http.Header.of_list [ ("Accept", "application/json") ])
      env sw
      (`Tcp (addr, port))
      path
  in
  match Client.read_fixed res with Some b -> print_string b | None -> ()
