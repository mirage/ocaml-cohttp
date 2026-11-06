let () =
  Logs.set_level ~all:true @@ Some Logs.Debug;
  Logs.set_reporter (Logs_fmt.reporter ())

(* Every 8 bytes name their own offset, so a byte that arrives in the
   wrong place says where it came from. *)
let big_body =
  String.concat "" (List.init 400 (fun i -> Printf.sprintf "%07d|" i))

let handler _conn request body =
  match Http.Request.resource request with
  | "/" -> Cohttp_eio.Server.respond_string ~status:`OK ~body:"root" ()
  | "/stream" ->
      let body = Eio_mock.Flow.make "streaming body" in
      let () =
        Eio_mock.Flow.on_read body
          [ `Return "Hello"; `Yield_then (`Return "World") ]
      in
      Cohttp_eio.Server.respond ~status:`OK ~body ()
  | "/post" -> Cohttp_eio.Server.respond ~status:`OK ~body ()
  | "/big" ->
      Cohttp_eio.Server.respond ~status:`OK
        ~body:(Eio.Flow.string_source big_body)
        ()
  | _ -> Cohttp_eio.Server.respond_string ~status:`Not_found ~body:"" ()

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let () =
    let socket =
      Eio.Net.listen env#net ~sw ~backlog:128 ~reuse_addr:true ~reuse_port:true
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, 4242))
    and server = Cohttp_eio.Server.make ~callback:handler () in
    Eio.Fiber.fork_daemon ~sw @@ fun () ->
    let () = Cohttp_eio.Server.run socket server ~on_error:raise in
    `Stop_daemon
  in
  let test_case name f =
    let f () =
      let socket =
        Eio.Net.connect ~sw env#net (`Tcp (Eio.Net.Ipaddr.V4.loopback, 4242))
      in
      f socket
    in
    Alcotest.test_case name `Quick f
  in
  let root socket =
    let () =
      Eio.Flow.write socket
        [ Cstruct.of_string "GET / HTTP/1.1\r\nconnection: close\r\n\r\n" ]
    in
    Alcotest.(check ~here:[%here] string)
      "response"
      "HTTP/1.1 200 OK\r\nconnection: close\r\ncontent-length: 4\r\n\r\nroot"
      Eio.Buf_read.(of_flow ~max_size:max_int socket |> take_all)
  and missing socket =
    let () =
      Eio.Flow.write socket
        [
          Cstruct.of_string "GET /missing HTTP/1.1\r\nconnection: close\r\n\r\n";
        ]
    in
    Alcotest.(check ~here:[%here] string)
      "response"
      "HTTP/1.1 404 Not Found\r\nconnection: close\r\ncontent-length: 0\r\n\r\n"
      Eio.Buf_read.(of_flow ~max_size:max_int socket |> take_all)
  and streaming_response socket =
    let () =
      Eio.Flow.write socket
        [
          Cstruct.of_string "GET /stream HTTP/1.1\r\nconnection: close\r\n\r\n";
        ]
    in
    Alcotest.(check ~here:[%here] string)
      "response"
      "HTTP/1.1 200 OK\r\n\
       connection: close\r\n\
       transfer-encoding: chunked\r\n\
       \r\n\
       5\r\n\
       Hello\r\n\
       5\r\n\
       World\r\n\
       0\r\n\
       \r\n"
      Eio.Buf_read.(of_flow ~max_size:max_int socket |> take_all)
  and request_body socket =
    let () =
      Eio.Flow.write socket
        [
          Cstruct.of_string
            "POST /post HTTP/1.1\r\n\
             connection: close\r\n\
             content-length:12\r\n\
             \r\n\
             hello world!";
        ]
    in
    Alcotest.(check ~here:[%here] string)
      "response"
      "HTTP/1.1 200 OK\r\n\
       connection: close\r\n\
       transfer-encoding: chunked\r\n\
       \r\n\
       c\r\n\
       hello world!\r\n\
       0\r\n\
       \r\n"
      Eio.Buf_read.(of_flow ~max_size:max_int socket |> take_all)
  and get_body_framing socket =
    let embedded = "GET /missing HTTP/1.1\r\n\r\n" in
    let request =
      Printf.sprintf
        "GET /post HTTP/1.1\r\n\
         content-length: %d\r\n\
         \r\n\
         %sGET / HTTP/1.1\r\n\
         connection: close\r\n\
         \r\n"
        (String.length embedded) embedded
    in
    Eio.Flow.write socket [ Cstruct.of_string request ];
    let first_response =
      Printf.sprintf
        "HTTP/1.1 200 OK\r\n\
         connection: keep-alive\r\n\
         transfer-encoding: chunked\r\n\
         \r\n\
         %x\r\n\
         %s\r\n\
         0\r\n\
         \r\n"
        (String.length embedded) embedded
    in
    let second_response =
      "HTTP/1.1 200 OK\r\nconnection: close\r\ncontent-length: 4\r\n\r\nroot"
    in
    Alcotest.(check ~here:[%here] string)
      "two responses"
      (first_response ^ second_response)
      Eio.Buf_read.(of_flow ~max_size:max_int socket |> take_all)
  (* The body flow hands one chunk over in as many [single_read] calls as
     the reader's buffer needs. The second and later deliveries must continue
     from where the previous one stopped, not from the start of the chunk.
     The server writes [big_body] as one 3200-byte chunk (it fits the
     writer's buffer), so a 100-byte reader takes it in 32 deliveries. *)
  and partial_body_reads socket =
    let client = Cohttp_eio.Client.make_generic (fun ~sw:_ _uri -> socket) in
    let _response, body =
      Cohttp_eio.Client.get ~sw client
        (Uri.of_string "http://localhost:4242/big")
    in
    let out = Buffer.create (String.length big_body) in
    let cs = Cstruct.create 100 in
    let rec loop () =
      match Eio.Flow.single_read body cs with
      | n ->
          Buffer.add_string out (Cstruct.to_string ~len:n cs);
          loop ()
      | exception End_of_file -> ()
    in
    let () = loop () in
    Alcotest.(check ~here:[%here] string)
      "body read 100 bytes at a time" big_body (Buffer.contents out)
  in
  Alcotest.run "cohttp-eio"
    [
      ( "cohttp-eio server",
        [
          test_case "root" root;
          test_case "missing" missing;
          test_case "streaming response" streaming_response;
          test_case "request body" request_body;
          test_case "GET body framing" get_body_framing;
          test_case "partial body reads" partial_body_reads;
        ] );
    ]
