open Eio
open Cohttp_eio

let () =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  (* Increment/decrement this value to see success/failure. *)
  let timeout_s = 0.01 in
  Eio.Time.with_timeout env#clock timeout_s (fun () ->
      let hostname, port = ("www.example.org", 80) in
      let he = Unix.gethostbyname hostname in
      let addr = `Tcp (Eio_unix.Ipaddr.of_unix he.h_addr_list.(0), port) in
      let conn = Net.connect ~sw env#net addr in
      let host = (hostname, Some port) in
      let res = Client.get ~conn host "/" in
      Client.read_fixed res |> Result.ok)
  |> function
  | Ok s -> print_string s
  | Error `Timeout -> print_string "Connection timed out"
