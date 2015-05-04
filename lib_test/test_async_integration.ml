open Core.Std
open Async.Std
open OUnit
open Cohttp
open Cohttp_async
open Cohttp_async_test

let chunk_body = ["one"; ""; " "; "bar"; ""]

let server =
  response_sequence [
    (* empty_chunk *)
    Server.respond `OK ~body:(Body.of_string_list chunk_body)
  ]

let ts =
  test_server_s server begin fun uri ->
    let empty_chunk () =
      let headers = Header.init_with "connection" "close" in
      Client.get ~headers uri >>= fun (_, body) ->
      body |> Body.to_string >>| fun body ->
      assert_equal body (String.concat ~sep:"" chunk_body) in
    [ "empty chunk test", empty_chunk
    ]
  end

let () =
  ts |> run_async_tests >>= (fun _ -> Shutdown.exit 0) |> don't_wait_for;
  never_returns (Scheduler.go ())
