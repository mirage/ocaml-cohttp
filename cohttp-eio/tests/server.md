## Setup

```ocaml
open Eio.Std
open Cohttp_eio
```

A mock socket for testing:

```ocaml
let socket = Eio_mock.Flow.make "socket"
```

## Example request handler

```ocaml
let chunk data = Body.Chunk { size = String.length data; data; extensions = [] }
let end_chunks = Body.Last_chunk []

let stream_response () =
  let headers = Http.Header.init () in
  let headers = Http.Header.add_transfer_encoding headers Http.Transfer.Chunked in
  let body_writer fn = fn (chunk "Hello"); Fiber.yield (); traceln "Resuming..."; fn (chunk "World"); fn end_chunks in
  let trailer_writer _fn = () in
  let body = Body.Chunked { body_writer; trailer_writer } in
  Http.Response.make ~version:`HTTP_1_1 ~status:`OK ~headers (), body

let post req body =
  let body = Server.read_fixed req body |> Option.get in
  let buf = Buffer.create 0 in
  let fmt = Format.formatter_of_buffer buf in
  Http.Request.pp fmt req;
  Format.fprintf fmt "\n\n%s%!" body;
  Server.text_response (Buffer.contents buf)

let get_chunks () =
  let rec body_writer chan chunks f =
    match In_channel.input_line chan with
    | Some data ->
        let extensions =
          if chunks = 0 then
            [
              Body.{ name = "ext1"; value = Some "ext1_v" };
              { name = "ext2"; value = Some "ext2_v" };
              { name = "ext3"; value = None };
            ]
          else []
        in
        let chunk =
          Body.Chunk { size = String.length data; data; extensions }
        in
        f chunk;
        body_writer chan (chunks + 1) f
    | None ->
        let last_chunk = Body.Last_chunk [] in
        In_channel.close chan;
        f last_chunk
  in
  let trailer_writer f =
    let trailer_headers =
      Http.Header.of_list
        [
          ("Expires", "Wed, 21 Oct 2015 07:28:00 GMT");
          ("Header1", "Header1 value text");
          ("Header2", "Header2 value text");
        ]
    in
    f trailer_headers
  in
  let chan =
    In_channel.open_gen [ Open_text; Open_rdonly ] 0 "server_chunks.txt"
  in
  let headers =
    Http.Header.of_list
      [
        ("Transfer-Encoding", "chunked");
        ("Content-Type", "text/plain");
        ("Trailer", "Expires, Header1");
      ]
  in
  let response = Http.Response.make ~status:`OK ~headers () in
  let body =
    Body.Chunked { body_writer = body_writer chan 0; trailer_writer }
  in
  (response, body)

let handle_chunk_request req body =
  let dump_chunk buf chunk =
    let s = Format.asprintf "\n%a" Body.pp_chunk chunk in
    Buffer.add_string buf s
  in
  let chunk_buf = Buffer.create 0 in
  match Server.read_chunked req body (dump_chunk chunk_buf) with
  | Some headers ->
      let req = { req with headers } in
      Buffer.contents chunk_buf
      |> Format.asprintf "%a@ %s%!" Http.Request.pp req
      |> Server.text_response
  | None -> Server.bad_request_response

let app (req, body, _client_addr) =
  match Http.Request.resource req with
  | "/" -> Server.text_response "root"
  | "/stream" -> stream_response ()
  | "/post" -> post req body
  | "/get_chunks" -> get_chunks ()
  | "/handle_chunk" -> handle_chunk_request req body
  | _ -> Server.not_found_response

let mock_env =
  let mock_clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time mock_clock 1666627935.85052109 ;
  let fake_domain_mgr = 
    object (_ : #Eio.Domain_manager.t)
      method run fn = fn ()
      method run_raw fn = fn ()
    end 
  in
  object 
    method net        = (Eio_mock.Net.make "mock net" :> Eio.Net.t)
    method clock      = (mock_clock :> Eio.Time.clock)
    method domain_mgr = fake_domain_mgr
  end

let connection_handler = Server.connection_handler app mock_env#clock
```

To test it, we run the connection handler with our mock socket:

```ocaml
let run test_case =
  Eio_mock.Backend.run @@ fun () ->
  Fiber.both test_case
    (fun () ->
       connection_handler socket (`Unix "test-socket")
    );;
```

## Tests

Asking for the root:

```ocaml
# run @@ fun () ->
  Eio_mock.Flow.on_read socket [
    `Return "GET / HTTP/1.1\r\n\r\n";
    `Raise End_of_file;
  ];;
+socket: read "GET / HTTP/1.1\r\n"
+             "\r\n"
+socket: wrote "HTTP/1.1 200 OK\r\n"
+              "Date: Mon, 24 Oct 2022 16:12:15 GMT\r\n"
+              "content-length: 4\r\n"
+              "content-type: text/plain; charset=UTF-8\r\n"
+              "\r\n"
+              "root"
- : unit = ()
```

A missing page:

```ocaml
# run @@ fun () ->
  Eio_mock.Flow.on_read socket [
    `Return "GET /missing HTTP/1.1\r\n\r\n";
    `Raise End_of_file;
  ] ;;
+socket: read "GET /missing HTTP/1.1\r\n"
+             "\r\n"
+socket: wrote "HTTP/1.1 404 Not Found\r\n"
+              "Date: Mon, 24 Oct 2022 16:12:15 GMT\r\n"
+              "Content-Length: 0\r\n"
+              "\r\n"
- : unit = ()
```

Streaming a response:

```ocaml
# run @@ fun () ->
  Eio_mock.Flow.on_read socket [
    `Return "GET /stream HTTP/1.1\r\n\r\n";
    `Raise End_of_file;
  ];;
+socket: read "GET /stream HTTP/1.1\r\n"
+             "\r\n"
+socket: wrote "HTTP/1.1 200 OK\r\n"
+              "Date: Mon, 24 Oct 2022 16:12:15 GMT\r\n"
+              "transfer-encoding: chunked\r\n"
+              "\r\n"
+              "5\r\n"
+              "Hello\r\n"
+Resuming...
+socket: wrote "5\r\n"
+              "World\r\n"
+              "0\r\n"
+              "\r\n"
- : unit = ()
```

Handle POST request:

```ocaml
# run @@ fun () ->
  Eio_mock.Flow.on_read socket [
    `Return "POST /post HTTP/1.1\r\n";
    `Return "Content-Length:12\r\n\r\n";
    `Return "hello world!";
    `Raise End_of_file;
  ];;
+socket: read "POST /post HTTP/1.1\r\n"
+socket: read "Content-Length:12\r\n"
+             "\r\n"
+socket: read "hello world!"
+socket: wrote "HTTP/1.1 200 OK\r\n"
+              "Date: Mon, 24 Oct 2022 16:12:15 GMT\r\n"
+              "content-length: 100\r\n"
+              "content-type: text/plain; charset=UTF-8\r\n"
+              "\r\n"
+              "meth: POST\n"
+              "resource: /post\n"
+              "version: HTTP/1.1\n"
+              "headers: Header { Content-Length = \"12\" }\n"
+              "\n"
+              "hello world!"
- : unit = ()
```

HTTP chunk-stream response with chunk extensions and trailers:

```ocaml
# run @@ fun () ->
  Eio_mock.Flow.on_read socket [
    `Return "GET /get_chunks HTTP/1.1\r\n";
    `Return "TE:trailers\r\n\r\n";
    `Raise End_of_file;
  ];;
+socket: read "GET /get_chunks HTTP/1.1\r\n"
+socket: read "TE:trailers\r\n"
+             "\r\n"
+socket: wrote "HTTP/1.1 200 OK\r\n"
+              "Date: Mon, 24 Oct 2022 16:12:15 GMT\r\n"
+              "Trailer: Expires, Header1\r\n"
+              "Content-Type: text/plain\r\n"
+              "Transfer-Encoding: chunked\r\n"
+              "\r\n"
+              "7;ext1=ext1_v;ext2=ext2_v;ext3\r\n"
+              "Mozilla\r\n"
+              "9\r\n"
+              "Developer\r\n"
+              "7\r\n"
+              "Network\r\n"
+              "0\r\n"
+              "Header2: Header2 value text\r\n"
+              "Header1: Header1 value text\r\n"
+              "Expires: Wed, 21 Oct 2015 07:28:00 GMT\r\n"
+              "\r\n"
- : unit = ()
```

The same request to `/get_chunks` will not write chunk headers because request is missing `TE:
trailers` header in the request. The `TE: trailers` is required for the server to determine if
a HTTP client agent has support for HTTP chunk trailer headers:

```ocaml
# run @@ fun () ->
  Eio_mock.Flow.on_read socket [
    `Return "GET /get_chunks HTTP/1.1\r\n\r\n";
    `Raise End_of_file;
  ];;
+socket: read "GET /get_chunks HTTP/1.1\r\n"
+             "\r\n"
+socket: wrote "HTTP/1.1 200 OK\r\n"
+              "Date: Mon, 24 Oct 2022 16:12:15 GMT\r\n"
+              "Trailer: Expires, Header1\r\n"
+              "Content-Type: text/plain\r\n"
+              "Transfer-Encoding: chunked\r\n"
+              "\r\n"
+              "7;ext1=ext1_v;ext2=ext2_v;ext3\r\n"
+              "Mozilla\r\n"
+              "9\r\n"
+              "Developer\r\n"
+              "7\r\n"
+              "Network\r\n"
+              "0\r\n"
+              "\r\n"
- : unit = ()
```

Server should handle chunk requests from clients:

```ocaml
# run @@ fun () ->
  Eio_mock.Flow.on_read socket [
    `Return "POST /handle_chunk HTTP/1.1\r\n";
    `Return "Content-Type: text/plain\r\n";
    `Return "Transfer-Encoding: chunked\r\n";
    `Return "Trailer: Expires, Header1\r\n\r\n";
    `Return "7;ext1=ext1_v;ext2=ext2_v;ext3\r\n";
    `Return "Mozilla\r\n";
    `Return "9\r\n";
    `Return "Developer\r\n";
    `Return "7\r\n";
    `Return "Network\r\n";
    `Return "0\r\n";
    `Return "Expires: Wed, 31 Oct 2015 07:28:00 GMT\r\n";
    `Return "Header1: Header1 value text\r\n";
    `Return "Header2: Header2 value text\r\n\r\n";
    `Raise End_of_file;
  ];;
+socket: read "POST /handle_chunk HTTP/1.1\r\n"
+socket: read "Content-Type: text/plain\r\n"
+socket: read "Transfer-Encoding: chunked\r\n"
+socket: read "Trailer: Expires, Header1\r\n"
+             "\r\n"
+socket: read "7;ext1=ext1_v;ext2=ext2_v;ext3\r\n"
+socket: read "Mozilla\r\n"
+socket: read "9\r\n"
+socket: read "Developer\r\n"
+socket: read "7\r\n"
+socket: read "Network\r\n"
+socket: read "0\r\n"
+socket: read "Expires: Wed, 31 Oct 2015 07:28:00 GMT\r\n"
+socket: read "Header1: Header1 value text\r\n"
+socket: read "Header2: Header2 value text\r\n"
+             "\r\n"
+socket: wrote "HTTP/1.1 200 OK\r\n"
+              "Date: Mon, 24 Oct 2022 16:12:15 GMT\r\n"
+              "content-length: 354\r\n"
+              "content-type: text/plain; charset=UTF-8\r\n"
+              "\r\n"
+              "meth: POST\n"
+              "resource: /handle_chunk\n"
+              "version: HTTP/1.1\n"
+              "headers: Header {\n"
+              " Content-Length = \"23\"; Header1 = \"Header1 value text\";\n"
+              " Content-Type = \"text/plain\" }\n"
+              "\n"
+              "size: 7\n"
+              " data: Mozilla\n"
+              " extensions:\n"
+              "  name: ext1\n"
+              "  value: ext1_v;\n"
+              "  name: ext2\n"
+              "  value: ext2_v;\n"
+              "  name: ext3\n"
+              "  value: \n"
+              "size: 9\n"
+              " data: Developer\n"
+              " extensions: \n"
+              "size: 7\n"
+              " data: Network\n"
+              " extensions: \n"
- : unit = ()
```
