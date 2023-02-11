# Response

```ocaml
open Cohttp_eio
```

## Response.parse

```ocaml
let make_buf_read () =
  Eio.Buf_read.of_string @@
    "HTTP/1.1 200 OK\r\n" ^
    "content-length: 13\r\n" ^
    "date: Wed, 08 Feb 2023 16:18:17 GMT\r\n" ^
    "content-type: text/html; charset=utf-8\r\n" ^
    "x-powered-by: Express\r\n" ^
    "cache-control: public, max-age=86400\r\n" ^
    "cf-cache-status: DYNAMIC\r\n" ^
    "server: cloudflare\r\n" ^
    "cf-ray: 7965ae27fa7c75bf-LHR\r\n" ^
    "content-encoding: br\r\n" ^
    "X-Firefox-Spdy: h2\r\n" ^
    "\r\n" ^
    "hello, world!"
    ;;
```

```ocaml
# let version, headers, status = Response.parse @@ make_buf_read () ;;
val version : Http.Version.t = `HTTP_1_1
val headers : Header.t = <abstr>
val status : Http.Status.t = `OK

# Eio.traceln "%a" Header.pp headers ;;
+{
+  content-length:  13;
+  date:  Wed, 08 Feb 2023 16:18:17 GMT;
+  content-type:  text/html; charset=utf-8;
+  x-powered-by:  Express;
+  cache-control:  public, max-age=86400;
+  cf-cache-status:  DYNAMIC;
+  server:  cloudflare;
+  cf-ray:  7965ae27fa7c75bf-LHR;
+  content-encoding:  br;
+  X-Firefox-Spdy:  h2
+}
- : unit = ()
```

## server_response

```ocaml
let mock_clock = Eio_mock.Clock.make ()
let () = Eio_mock.Clock.set_time mock_clock 1666627935.85052109
```

A `Buffer.t` sink to test `Body.writer`.

```ocaml
let test_server_response r =
  Eio_main.run @@ fun env ->
  let b = Buffer.create 10 in
  let s = Eio.Flow.buffer_sink b in
  Eio.Buf_write.with_flow s (fun bw ->
    Response.write r mock_clock bw;
  );
  Eio.traceln "%s" (Buffer.contents b);;
```

## Response.text

```ocaml
# test_server_response @@ Response.text "hello, world";;
+HTTP/1.1 200 OK
+Date: Mon, 24 Oct 2022 16:12:15 GMT
+Content-Length: 12
+Content-Type: text/plain; charset=UTF-8
+
+hello, world
- : unit = ()
```

## Response.html

```ocaml
# test_server_response @@ Response.html "hello, world";;
+HTTP/1.1 200 OK
+Date: Mon, 24 Oct 2022 16:12:15 GMT
+Content-Length: 12
+Content-Type: text/html; charset=UTF-8
+
+hello, world
- : unit = ()
```

## Response.not_found

```ocaml
# test_server_response @@ Response.not_found ;;
+HTTP/1.1 404 Not Found
+Date: Mon, 24 Oct 2022 16:12:15 GMT
+Content-Length: 0
+
+
- : unit = ()
```

## Response.internal_server_error

```ocaml
# test_server_response @@ Response.internal_server_error ;;
+HTTP/1.1 500 Internal Server Error
+Content-Length: 0
+
+
- : unit = ()
```

## Response.bad_request

```ocaml
# test_server_response @@ Response.bad_request ;;
+HTTP/1.1 400 Bad Request
+Date: Mon, 24 Oct 2022 16:12:15 GMT
+Content-Length: 0
+
+
- : unit = ()
```

## Response.chunked_response

```ocaml
# let write_chunk f =
    f @@ Chunked_body.make ~extensions:["ext1",Some "ext1_v"] "Hello, ";
    f @@ Chunked_body.make ~extensions:["ext2",None] "world!";
    f @@ Chunked_body.make "Again!";
    f @@ Chunked_body.make "";;
val write_chunk : (Chunked_body.t -> 'a) -> 'a = <fun>

# let write_trailer f =
    let trailer_headers =
      Http.Header.of_list
        [
          ("Expires", "Wed, 21 Oct 2015 07:28:00 GMT");
          ("Header1", "Header1 value text");
          ("Header2", "Header2 value text");
        ]
    in
    f trailer_headers;;
val write_trailer : (Header.t -> 'a) -> 'a = <fun>
```

Writes chunked response trailer headers.

```ocaml
# test_server_response @@ Response.chunked_response ~ua_supports_trailer:true write_chunk write_trailer ;;
+HTTP/1.1 200 OK
+Date: Mon, 24 Oct 2022 16:12:15 GMT
+Transfer-Encoding: chunked
+
+7;ext1=ext1_v
+Hello, 
+6;ext2
+world!
+6
+Again!
+0
+Header2: Header2 value text
+Header1: Header1 value text
+Expires: Wed, 21 Oct 2015 07:28:00 GMT
+
+
- : unit = ()
```

No chunked trailer headers.
```ocaml
# test_server_response @@ Response.chunked_response ~ua_supports_trailer:false write_chunk write_trailer ;;
+HTTP/1.1 200 OK
+Date: Mon, 24 Oct 2022 16:12:15 GMT
+Transfer-Encoding: chunked
+
+7;ext1=ext1_v
+Hello, 
+6;ext2
+world!
+6
+Again!
+0
+
+
- : unit = ()
```
## Response.pp

Pretty print `Response.server`

```ocaml
# Response.pp Format.std_formatter @@ Response.html "hello, world";;
{
  Version:  HTTP/1.1;
  Status:  200 OK;
  Headers :
    { }
}
- : unit = ()
```
