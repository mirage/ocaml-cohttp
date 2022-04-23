module Server = Cohttp_async.Server
module Body = Cohttp_async.Body
module Deferred = Async_kernel.Deferred

let ( let+ ) x f = Deferred.map x ~f
let ( let* ) x f = Deferred.bind x ~f

let server =
  List.map Cohttp_async_test.const
    [
      (let body : Body.t = Body.of_string "hello curl" in
       Server.respond `OK ~body);
    ]
  |> Cohttp_async_test.response_sequence

let test =
  Cohttp_async_test.test_server_s ~port:25_290 server (fun uri ->
      [
        ( "simple request",
          fun () ->
            let uri = Uri.to_string uri in
            let input = Cohttp_curl_async.Source.empty in
            let output = Cohttp_curl_async.Sink.string in
            let ctx = Cohttp_curl_async.Context.create () in
            let req =
              Cohttp_curl_async.Request.create `GET ~uri ~input ~output
            in
            let resp = Cohttp_curl_async.submit ctx req in
            let+ body = Cohttp_curl_async.Response.body resp in
            Alcotest.check Alcotest.string "test 1" body "hello curl" );
      ])

let _ =
  let run =
    let* _ = Cohttp_async_test.run_async_tests test in
    Async_unix.Shutdown.exit 0
  in
  Deferred.don't_wait_for run;
  Core.never_returns (Async_unix.Scheduler.go ())
