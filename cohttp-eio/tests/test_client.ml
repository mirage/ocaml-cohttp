open Eio
open Cohttp_eio

let get () =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let res =
    Client.get
      ~headers:(Http.Header.of_list [ ("Accept", "application/json") ])
      env sw
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 8080))
      (Uri.of_string "/get")
  in
  match Client.read_fixed res with Some s -> print_string s | None -> ()

let post () =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let content = "hello world!" in
  let content_length = String.length content |> string_of_int in
  let res =
    Client.post
      ~headers:
        (Http.Header.of_list
           [
             ("Accept", "application/json"); ("Content-Length", content_length);
           ])
      ~body:(Body.Fixed content) env sw
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 8080))
      (Uri.of_string "/post")
  in
  match Client.read_fixed res with Some s -> print_string s | None -> ()

let () =
  match Sys.argv.(1) with
  | "get" -> get ()
  | "post" -> post ()
  | _ -> print_string "Usage: test-client [get|post]"
