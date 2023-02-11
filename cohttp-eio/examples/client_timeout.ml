open Eio
open Cohttp_eio

let () =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  (* Increment/decrement this value to see success/failure. *)
  let timeout_s = 0.01 in
  Eio.Time.with_timeout env#clock timeout_s (fun () ->
      let host, port = ("www.example.org", 80) in
      let he = Unix.gethostbyname host in
      let addr = `Tcp (Eio_unix.Ipaddr.of_unix he.h_addr_list.(0), port) in
      let conn = Net.connect ~sw env#net addr in
      let req = Request.get "www.example.org" in
      let res = Client.call ~conn req in
      Option.to_result ~none:`No_body (Body.read_content res))
  |> function
  | Ok s -> print_string s
  | Error `No_body -> ()
  | Error `Timeout -> print_string "Connection timed out"
