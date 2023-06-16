let () =
  Logs.set_level ~all:true @@ Some Logs.Debug;
  Logs.set_reporter (Logs_fmt.reporter ())

let handler _conn request body =
  match Http.Request.resource request with
  | "/" -> (Http.Response.make (), Cohttp_eio.Body.of_string "root")
  | "/stream" ->
      let body = Eio_mock.Flow.make "streaming body" in
      let () =
        Eio_mock.Flow.on_read body
          [ `Return "Hello"; `Yield_then (`Return "World") ]
      in
      (Http.Response.make (), (body :> Eio.Flow.source))
  | "/post" -> (Http.Response.make (), body)
  | _ -> (Http.Response.make ~status:`Not_found (), Cohttp_eio.Body.of_string "")

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let () =
    let socket =
      Eio.Net.listen env#net ~sw ~backlog:128 ~reuse_addr:true ~reuse_port:true
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, 4242))
    and server = Cohttp_eio.Server.make ~callback:handler () in
    Eio.Fiber.fork_daemon ~sw @@ fun () ->
    let () = Cohttp_eio.Server.run socket server in
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
      "response" "HTTP/1.1 200 OK\r\ncontent-length: 4\r\n\r\nroot"
      Eio.Buf_read.(of_flow ~max_size:max_int socket |> take_all)
  and missing socket =
    let () =
      Eio.Flow.write socket
        [
          Cstruct.of_string "GET /missing HTTP/1.1\r\nconnection: close\r\n\r\n";
        ]
    in
    Alcotest.(check ~here:[%here] string)
      "response" "HTTP/1.1 404 Not Found\r\ncontent-length: 0\r\n\r\n"
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
       transfer-encoding: chunked\r\n\
       \r\n\
       c\r\n\
       hello world!\r\n\
       0\r\n\
       \r\n"
      Eio.Buf_read.(of_flow ~max_size:max_int socket |> take_all)
  in
  Alcotest.run "cohttp-eio"
    [
      ( "cohttp-eio server",
        [
          test_case "root" root;
          test_case "missing" missing;
          test_case "streaming response" streaming_response;
          test_case "request body" request_body;
        ] );
    ]
