open Core.Std
open Async.Std
open OUnit
open Cohttp
open Cohttp_async
open Cohttp_async_test

let chunk_body = ["one"; ""; " "; "bar"; ""]

let large_string = String.make (Int.pow 2 16) 'A'

let response_bodies = [ "Testing"
                      ; "Foo bar" ]

let ok s = Server.respond `OK ~body:(Body.of_string s)

let server =
  [ (* empty_chunk *)
    const @@ Server.respond `OK ~body:(Body.of_string_list chunk_body);
    (* large response *)
    const @@ Server.respond_with_string large_string;
    (* large request *)
    (fun _ body ->
       body |> Body.to_string >>| String.length >>= fun len ->
       Server.respond_with_string (Int.to_string len))
  ] @ (* pipelined_chunk *)
  (response_bodies |> List.map ~f:(Fn.compose const ok))
  |> response_sequence


let ts =
  test_server_s server begin fun uri ->
    let headers = Header.init_with "connection" "close" in
    let empty_chunk () =
      Client.get ~headers uri >>= fun (_, body) ->
      body |> Body.to_string >>| fun body ->
      assert_equal body (String.concat ~sep:"" chunk_body) in
    let large_response () =
      Client.get ~headers uri >>= fun (_, body) ->
      body |> Body.to_string >>| fun body ->
      assert_equal body large_string in
    let large_request () =
      Client.post ~headers ~body:(Body.of_string large_string) uri
      >>= fun (_, body) ->
      body |> Body.to_string >>| fun s ->
      assert_equal (String.length large_string) (Int.of_string s) in
    let pipelined_chunk () =
      let printer x = x in
      let reqs = [
        Request.make ~meth:`POST uri, (Body.of_string "foo");
        Request.make ~meth:`POST uri, (Body.of_string "bar");
      ] in
      let body_q = response_bodies |> Queue.of_list in
      reqs
      |> Pipe.of_list
      |> Client.callv uri >>= fun responses -> responses
                                               |> Pipe.to_list
      >>= fun resps -> resps
                       |> Deferred.List.iter ~f:(fun (resp, body) ->
                         let expected_body = body_q |> Queue.dequeue_exn in
                         body |> Body.to_string >>| fun body ->
                         assert_equal ~printer expected_body body
                       )
    in
    [ "empty chunk test", empty_chunk
    ; "large response", large_response
    ; "large request", large_request
    ; "pipelined chunk test", pipelined_chunk
    ]
  end

let () =
  ts |> run_async_tests >>= (fun _ -> Shutdown.exit 0) |> don't_wait_for;
  never_returns (Scheduler.go ())
