## Setup

```ocaml
# #require "eio.mock";;
# #require "cohttp-eio";;
```

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

let app (req, _body, _client_addr) =
  match Http.Request.resource req with
  | "/" -> Server.text_response "root"
  | "/stream" -> stream_response ()
  | _ -> Server.not_found_response

let connection_handler = Server.connection_handler app
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
