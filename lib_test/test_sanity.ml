open Lwt
open OUnit
open Cohttp
open Cohttp_lwt_unix
open Cohttp_lwt_unix_test

module Body = Cohttp_lwt_body

let message = "Hello sanity!"

let chunk_body = ["one"; ""; " "; "bar"; ""]

let server =
  [ (* t *)
    Server.respond_string ~status:`OK ~body:message ();
    (* empty_chunk *)
    Server.respond ~status:`OK ~body:(Body.of_string_list chunk_body) ();
    (* pipelined_chunk *)
    Server.respond ~status:`OK ~body:(Body.of_string "") ();
    Server.respond ~status:`OK ~body:(Body.of_string_list chunk_body) ();
    Server.respond ~status:`OK ~body:(Body.of_string "") ();
    (* not modified *)
    Server.respond ~status:`Not_modified ~body:Body.empty ()
  ]
  |> List.map const
  |> response_sequence

let ts =
  Cohttp_lwt_unix_test.test_server_s server begin fun uri ->
    let t () =
      Client.get uri >>= fun (_, body) ->
      body |> Body.to_string >|= fun body ->
      assert_equal body message in
    let empty_chunk () =
      Client.get uri >>= fun (_, body) ->
      body |> Body.to_string >|= fun body ->
      assert_equal body (String.concat "" chunk_body) in
    let pipelined_chunk () =
      let printer x = x in
      let body = String.concat "" chunk_body in
      let reqs = [
        Request.make ~meth:`HEAD uri, `Empty;
        Request.make ~meth:`GET  uri, `Empty;
        Request.make ~meth:`HEAD uri, `Empty;
      ] in
      let counter = ref 0 in
      Client.callv uri (Lwt_stream.of_list reqs) >>= fun resps ->
      Lwt_stream.iter_s (fun (r,rbody) ->
        rbody |> Body.to_string >|= fun rbody ->
        begin match !counter with
          | 0 | 2 -> assert_equal ~printer ""   rbody
          | _     -> assert_equal ~printer body rbody
        end;
        incr counter
      ) resps >>= fun () ->
      assert_equal ~printer:string_of_int 3 !counter;
      return_unit in
    let not_modified_has_no_body () =
      Client.get uri >>= fun (resp, body) ->
      assert_equal (Response.status resp) `Not_modified;
      let headers = Response.headers resp in
      assert_equal ~printer:Transfer.string_of_encoding
        Transfer.Unknown (Header.get_transfer_encoding headers);
      body |> Body.is_empty >|= fun is_empty ->
      assert_bool "No body returned when not modified" is_empty in
    [ "sanity test", t
    ; "empty chunk test", empty_chunk
    ; "pipelined chunk test", pipelined_chunk
    ; "no body when response is not modified", not_modified_has_no_body
    ]
  end


let _ = ts |> run_async_tests |> Lwt_main.run
