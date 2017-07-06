open Lwt
open OUnit
open Cohttp
open Cohttp_lwt_unix
open Cohttp_lwt_unix_test

module Body = Cohttp_lwt.Body

let message = "Hello sanity!"

let chunk_body = ["one"; ""; " "; "bar"; ""]

let leak_repeat = 1024

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
    Server.respond ~status:`Not_modified ~body:Body.empty ();
    (* pipelined_interleave *)
    Server.respond_string ~status:`OK ~body:"one" ();
    Server.respond_string ~status:`OK ~body:"two" ();
    Server.respond_string ~status:`OK ~body:"three" ();
    (* Massive chunked *)
    Server.respond ~status:`OK ~body:begin
      let count = ref 0 in
      let chunk = String.make 64 '0' in
      `Stream (Lwt_stream.from_direct (fun () ->
        if !count < 1000
        then (incr count; Some chunk)
        else None
      ))
    end()
  ]
  |> List.map const
  |> (fun tests ->
    tests @ [
      (fun _ body -> (* Returns 500 on bad file *)
         Body.to_string body >>= fun fname ->
         Server.respond_file ~fname ())] @
    (Array.init (leak_repeat * 2) (fun _ _ _ ->
         (* no leaks *)
         Server.respond_string ~status:`OK ~body:"no leak" ()) |> Array.to_list))
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
    let pipelined_interleave () =
      let r n =
        let uri = Uri.with_query' uri ["test", (string_of_int n)] in
        (Request.make uri, Body.empty) in
      let (reqs, push) = Lwt_stream.create () in
      push (Some (r 1));
      push (Some (r 2));
      Client.callv uri reqs >>= fun resps ->
      let resps = Lwt_stream.map_s (fun (_, b) -> Body.to_string b) resps in
      Lwt_stream.fold (fun b i ->
        Lwt_log.ign_info_f "Request %i\n" i;
        begin match i with
        | 0 -> assert_equal b "one"
        | 1 ->
          assert_equal b "two";
          Lwt_log.ign_info "Sending extra request";
          push (Some (r 3))
        | 2 ->
          assert_equal b "three";
          push None;
        | x -> assert_failure ("Test failed with " ^ string_of_int x)
        end;
        succ i
      ) resps 0 >|= fun l ->
      assert_equal l 3
    in
    let massive_chunked () =
      Client.get uri >>= fun (resp, body) ->
      Body.to_string body >|= fun body ->
      assert_equal ~printer:string_of_int (1000 * 64) (String.length body) in
    let unreadable_file_500 () =
      let fname = "unreadable500" in
      Lwt.finalize (fun () ->
        Lwt_io.open_file ~flags:[Lwt_unix.O_CREAT] ~perm:0o006
          ~mode:Lwt_io.Output fname >>= fun oc ->
        Lwt_io.write_line oc "never read" >>= fun () ->
        Lwt_io.close oc >>= fun () ->
        Client.post uri ~body:(Body.of_string fname)
        >>= begin fun (resp, body) ->
          assert_equal ~printer:Code.string_of_status
            (Response.status resp) `Internal_server_error;
          Body.to_string body
        end >|= fun body ->
        assert_equal ~printer:(fun x -> "'" ^ x ^ "'")
          body "Error: Internal Server Error"
      ) (fun () -> Lwt_unix.unlink fname)
    in
    let test_no_leak () =
      let stream = Array.init leak_repeat (fun _ -> uri) |> Lwt_stream.of_array in
      Lwt_stream.fold_s (fun uri () ->
          Client.head uri >>= fun resp_head ->
          assert_equal (Response.status resp_head) `OK;
          Client.get uri >>= fun (resp_get, body) ->
          assert_equal (Response.status resp_get) `OK;
          Body.drain_body body) stream ()
    in
    [ "sanity test", t
    ; "empty chunk test", empty_chunk
    ; "pipelined chunk test", pipelined_chunk
    ; "no body when response is not modified", not_modified_has_no_body
    ; "pipelined with interleaving requests", pipelined_interleave
    ; "massive chunked", massive_chunked
    ; "unreadable file returns 500", unreadable_file_500
    ; "no leaks on requests", test_no_leak
    ]
  end


let _ = ts |> run_async_tests |> Lwt_main.run
