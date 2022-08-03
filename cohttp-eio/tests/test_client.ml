module Net = Eio.Net
module Stdenv = Eio.Stdenv
module Switch = Eio.Switch
open Cohttp_eio

let conn env sw port () =
  let addr = `Tcp (Net.Ipaddr.V4.loopback, port) in
  let flow = (Net.connect ~sw (Stdenv.net env) addr :> Eio.Flow.two_way) in
  let host = ("localhost", Some port) in
  (host, flow)

let get env sw port =
  let res =
    Client.get
      ~headers:(Http.Header.of_list [ ("Accept", "application/json") ])
      (conn env sw port) "/get"
  in
  match Client.read_fixed res with Some s -> print_string s | None -> ()

let post env sw port =
  let content = "hello world!" in
  let content_length = String.length content |> string_of_int in
  let res =
    Client.post
      ~headers:
        (Http.Header.of_list
           [
             ("Accept", "application/json"); ("Content-Length", content_length);
           ])
      ~body:(Body.Fixed content) (conn env sw port) "/post"
  in
  match Client.read_fixed res with Some s -> print_string s | None -> ()

let () =
  let port = ref 8080 in
  let t = ref "invalid_uri" in
  Arg.parse
    [
      ("-p", Arg.Set_int port, " Server listening port number(8080 default)");
      ( "-t",
        Arg.Set_string t,
        "Specify test case to execute,('invalid_uri' default)" );
    ]
    ignore "An HTTP/1.1 server";

  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  match !t with
  | "get" -> get env sw !port
  | "post" -> post env sw !port
  | _ -> print_string "Usage: test-client [get|post]"
