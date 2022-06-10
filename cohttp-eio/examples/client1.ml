(* Contributed by @patricoferris *)

open Eio
open Cohttp_eio

let () =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let res =
    Client.get
      ~headers:(Http.Header.of_list [ ("Accept", "application/json") ])
      env sw
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 8080))
      (Uri.of_string "/")
  in
  match Client.read_fixed res with Some b -> print_string b | None -> ()
