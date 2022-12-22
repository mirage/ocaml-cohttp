## Setup

```ocaml
open Eio.Std
open Cohttp_eio
```

A mock client socket and host for testing:

```ocaml
let host = "localhost"
let conn = Eio_mock.Flow.make "socket"
let mock_env =
  object 
    method net = (Eio_mock.Net.make "mock net" :> Eio.Net.t)
  end

let run ~response ~test =
  Eio_mock.Backend.run @@ fun () ->
  Fiber.both
    (fun () -> Eio_mock.Flow.on_read conn response)
    test
```

## Tests

GET method request:

```ocaml
# run ~response:
    [`Return "HTTP/1.1 200 OK\r\n";
     `Return "content-length: 4\r\n";
     `Return "content-type: text/plain; charset=UTF-8\r\n\r\n";
     `Return "root";
     `Raise End_of_file
    ]
    ~test:(fun () ->
      (Client.get
        ~headers:(Http.Header.of_list [ ("Accept", "application/json") ])
        ~conn
        ~host
        mock_env
        "/")
      |> Client.read_fixed
      |> print_string);;
+socket: wrote "GET / HTTP/1.1\r\n"
+              "Host: localhost\r\n"
+              "Connection: TE\r\n"
+              "TE: trailers\r\n"
+              "User-Agent: cohttp-eio\r\n"
+              "Accept: application/json\r\n"
+              "\r\n"
+socket: read "HTTP/1.1 200 OK\r\n"
+socket: read "content-length: 4\r\n"
+socket: read "content-type: text/plain; charset=UTF-8\r\n"
+             "\r\n"
+socket: read "root"
root
- : unit = ()
```

POST request:

```ocaml
# run ~response:
    [`Return "HTTP/1.1 200 OK\r\n";
     `Return "content-length: 5\r\n\r\n";
     `Return "hello";
     `Raise End_of_file
    ]
    ~test:(fun () ->
      let content = "hello world!" in
      let content_length = String.length content |> string_of_int in
      Client.post
        ~headers:
          (Http.Header.of_list [("Accept", "application/json"); ("Content-Length", content_length);])
          ~body:(Body.Fixed content) 
          ~conn 
          ~host
                  mock_env
          "/post"
      |> Client.read_fixed
      |> print_string);;
+socket: wrote "POST /post HTTP/1.1\r\n"
+              "Host: localhost\r\n"
+              "Connection: TE\r\n"
+              "TE: trailers\r\n"
+              "User-Agent: cohttp-eio\r\n"
+              "Content-Length: 12\r\n"
+              "Accept: application/json\r\n"
+              "\r\n"
+              "hello world!"
+socket: read "HTTP/1.1 200 OK\r\n"
+socket: read "content-length: 5\r\n"
+             "\r\n"
+socket: read "hello"
hello
- : unit = ()
```

Chunk request:

```ocaml
# run ~response:
    [`Return "HTTP/1.1 200 OK\r\n";
     `Return "content-length:0\r\n\r\n";
     `Raise End_of_file;
    ]
    ~test:(fun () ->
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
      In_channel.with_open_text "chunks.txt" (fun chan ->
          Client.post
            ~headers:
              (Http.Header.of_list
                 [
                   ("Transfer-Encoding", "chunked");
                   ("Content-Type", "text/plain");
                   ("Trailer", "Expires, Header1");
                 ])
            ~body:
              (Body.Chunked { body_writer = body_writer chan 0; trailer_writer })
            ~conn 
            ~host
            mock_env
            "/handle_chunk"
      )
      |> Client.read_fixed
      |> print_string);;
+socket: wrote "POST /handle_chunk HTTP/1.1\r\n"
+              "Host: localhost\r\n"
+              "Connection: TE\r\n"
+              "TE: trailers\r\n"
+              "User-Agent: cohttp-eio\r\n"
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
+socket: read "HTTP/1.1 200 OK\r\n"
+socket: read "content-length:0\r\n"
+             "\r\n"
- : unit = ()
```

```ocaml
# run ~response:
    [`Return "HTTP/1.1 200 OK\r\n";
    `Return "Trailer: Expires, Header1\r\n";
    `Return "Content-Type: text/plain\r\n";
    `Return "Transfer-Encoding: chunked\r\n";
    `Return "\r\n";
    `Return "7;ext1=ext1_v;ext2=ext2_v;ext3\r\n";
    `Return "Mozilla\r\n";
    `Return "9\r\n";
    `Return "Developer\r\n";
    `Return "7\r\n";
    `Return "Network\r\n";
    `Return "0\r\n";
    `Return "\r\n";
    `Raise End_of_file
    ]
    ~test:(fun () ->
        let print_chunk chunk = traceln "chunk body: %a\n" Body.pp_chunk chunk in
        let res = Client.get ~conn ~host mock_env "/get_chunk" in
        match Client.read_chunked res print_chunk with
        | None -> print_string "FAIL"
        | Some _ -> print_string "PASS"
    );;
+socket: wrote "GET /get_chunk HTTP/1.1\r\n"
+              "Host: localhost\r\n"
+              "Connection: TE\r\n"
+              "TE: trailers\r\n"
+              "User-Agent: cohttp-eio\r\n"
+              "\r\n"
+socket: read "HTTP/1.1 200 OK\r\n"
+socket: read "Trailer: Expires, Header1\r\n"
+socket: read "Content-Type: text/plain\r\n"
+socket: read "Transfer-Encoding: chunked\r\n"
+socket: read "\r\n"
+socket: read "7;ext1=ext1_v;ext2=ext2_v;ext3\r\n"
+socket: read "Mozilla\r\n"
+chunk body: size: 7
+            data: Mozilla
+            extensions:
+             name: ext1
+             value: ext1_v;
+             name: ext2
+             value: ext2_v;
+             name: ext3
+             value:
+
+socket: read "9\r\n"
+socket: read "Developer\r\n"
+chunk body: size: 9
+            data: Developer
+            extensions:
+
+socket: read "7\r\n"
+socket: read "Network\r\n"
+chunk body: size: 7
+            data: Network
+            extensions:
+
+socket: read "0\r\n"
+socket: read "\r\n"
+chunk body:
+
PASS
- : unit = ()
```
