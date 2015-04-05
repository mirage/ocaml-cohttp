open Lwt
open OUnit
open Cohttp_lwt_unix
open Cohttp_lwt_unix_test

let message = "Hello sanity!"

let server _req _body = Server.respond_string ~status:`OK ~body:message ()

let ts =
  Cohttp_lwt_unix_test.test_server server begin fun uri ->
    let t () =
      Client.get uri >>= fun (_, body) ->
      body |> Cohttp_lwt_body.to_string >>= fun body ->
      return (assert_equal body message) in
    ["sanity test", t]
  end


let _ = run_async_tests ts
