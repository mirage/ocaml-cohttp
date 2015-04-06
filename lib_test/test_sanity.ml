open Lwt
open OUnit
open Cohttp_lwt_unix
open Cohttp_lwt_unix_test

module Body = Cohttp_lwt_body

let message = "Hello sanity!"

let chunk_body = ["one"; ""; " "; "bar"; ""]

let server =
  response_sequence [
    Server.respond_string ~status:`OK ~body:message ();
    Server.respond ~status:`OK ~body:(Body.of_string_list chunk_body) ()
  ]

let ts =
  Cohttp_lwt_unix_test.test_server server begin fun uri ->
    let t () =
      Client.get uri >>= fun (_, body) ->
      body |> Body.to_string >|= fun body ->
      assert_equal body message in
    let empty_chunk () =
      Client.get uri >>= fun (_, body) ->
      body |> Body.to_string >|= fun body ->
      assert_equal body (String.concat "" chunk_body) in
    [ "sanity test", t
    ; "empty chunk test", empty_chunk]
  end


let _ = run_async_tests ts
