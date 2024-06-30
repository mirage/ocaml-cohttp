module Server = Cohttp_lwt_unix.Server
module Body = Cohttp_lwt.Body
open Lwt.Syntax
open Lwt.Infix

let server =
  List.map Cohttp_lwt_unix_test.const
    [
      (let body : Body.t = Body.of_string "hello curl" in
       Server.respond ~status:`OK ~body ());
    ]
  |> Cohttp_lwt_unix_test.response_sequence

let check_error = function Ok _ -> failwith "expected error" | Error _ -> ()

let without_error = function
  | Ok s -> s
  | Error e -> failwith (Cohttp_curl_lwt.Error.message e)

let test =
  Cohttp_lwt_unix_test.test_server_s ~port:25_190 server (fun uri ->
      [
        ( "simple request",
          fun () ->
            let uri = Uri.to_string uri in
            let input = Cohttp_curl_lwt.Source.empty in
            let output = Cohttp_curl_lwt.Sink.string in
            let ctx = Cohttp_curl_lwt.Context.create () in
            let req = Cohttp_curl_lwt.Request.create `GET ~uri ~input ~output in
            let resp = Cohttp_curl_lwt.submit ctx req in
            let+ body = Cohttp_curl_lwt.Response.body resp >|= without_error in
            Alcotest.check Alcotest.string "test 1" body "hello curl" );
        ( "failing request",
          fun () ->
            let uri = "0.0.0.0:45_120" in
            let input = Cohttp_curl_lwt.Source.empty in
            let output = Cohttp_curl_lwt.Sink.string in
            let ctx = Cohttp_curl_lwt.Context.create () in
            let req = Cohttp_curl_lwt.Request.create `GET ~uri ~input ~output in
            let resp = Cohttp_curl_lwt.submit ctx req in
            let* http_resp = Cohttp_curl_lwt.Response.response resp in
            check_error http_resp;
            let+ body = Cohttp_curl_lwt.Response.body resp in
            check_error body );
      ])

let _ = test |> Cohttp_lwt_unix_test.run_async_tests |> Lwt_main.run
