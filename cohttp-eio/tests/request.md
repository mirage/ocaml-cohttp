# Request

```ocaml
open Cohttp_eio
```

A `Buffer.t` sink to test `Body.writer`.

```ocaml
let test_client r =
  Eio_main.run @@ fun env ->
  let b = Buffer.create 10 in
  let s = Eio.Flow.buffer_sink b in
  Eio.Buf_write.with_flow s (fun bw ->
    Request.write r bw;
  );
  Eio.traceln "%s" (Buffer.contents b);;
```

Attempt at creating a client request with invalid url results in `Invalid_argument` exception. Url must have host information. 

```ocaml
# let r = Request.get "/products" ;;
Exception: Invalid_argument "invalid url: host not defined".
```

## Request.get - client

Create a `GET` request and write it.

```ocaml
# let r = Request.get "www.example.com/products" ;;
val r : Body.none Request.client_request = <obj>

# test_client r ;;
+GET /products HTTP/1.1
+Host: www.example.com
+Connection: TE
+TE: trailers
+User-Agent: cohttp-eio
+
+
- : unit = ()

# test_client @@ Request.get "www.example.com" ;;
+GET / HTTP/1.1
+Host: www.example.com
+Connection: TE
+TE: trailers
+User-Agent: cohttp-eio
+
+
- : unit = ()
```

## Request.head - client

```ocaml
# test_client @@ Request.head "www.example.com" ;;
+HEAD / HTTP/1.1
+Host: www.example.com
+Connection: TE
+TE: trailers
+User-Agent: cohttp-eio
+
+
- : unit = ()
```

## Request.post - client

```ocaml
# let body = Body.content_writer ~content:"Hello World!" ~content_type:"text/plain" in
  test_client @@ Request.post body "www.example.com/say_hello";;
+POST /say_hello HTTP/1.1
+Host: www.example.com
+Content-Length: 12
+Content-Type: text/plain
+Connection: TE
+TE: trailers
+User-Agent: cohttp-eio
+
+Hello World!
- : unit = ()
```

## Request.post_form_values - client

```ocaml
# let form_values = ["field1", ["val 1"]; "field2", ["v2";"v3";"v4"]] in
  test_client @@ Request.post_form_values form_values "www.example.com/form_a" ;;
+POST /form_a HTTP/1.1
+Host: www.example.com
+Content-Length: 30
+Content-Type: application/x-www-form-urlencoded
+Connection: TE
+TE: trailers
+User-Agent: cohttp-eio
+
+field1=val%201&field2=v2,v3,v4
- : unit = ()
```

## Request.client

```ocaml
# let headers = Http.Header.of_list ["Header1", "val 1"; "Header2", "val 2"] in
  test_client @@ Request.client_request 
    ~version:`HTTP_1_1 
    ~headers 
    ~port:8080 
    ~host:"www.example.com" 
    ~resource:"/update" 
    Method.Get 
    Body.none ;;
+GET /update HTTP/1.1
+Host: www.example.com:8080
+Connection: TE
+TE: trailers
+User-Agent: cohttp-eio
+Header2: val 2
+Header1: val 1
+
+
- : unit = ()
```

## Request.parse

Mock the client addr.

```ocaml
let client_addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8081)

let make_buf_read version meth connection = 
  let s = Printf.sprintf "%s /products HTTP/%s\r\nHost: www.example.com\r\nConnection: %s\r\nTE: trailers\r\nUser-Agent: cohttp-eio\r\n\r\n" meth version connection in
  Eio.Buf_read.of_string s
```

### Parse HTTP/1.1 GET request.

```ocaml
# let r = Request.parse client_addr @@ make_buf_read "1.1" "get" "TE";;
val r : Request.server_request = <obj>

# Request.version r;;
- : Http.Version.t = `HTTP_1_1

# Eio.traceln "%a" Header.pp @@ Request.headers r;;
+{
+  Host:  www.example.com;
+  Connection:  TE;
+  TE:  trailers;
+  User-Agent:  cohttp-eio
+}
- : unit = ()

# Request.meth r;;
- : Body.none Method.t = Cohttp_eio__.Method.Get

# Request.resource r ;;
- : string = "/products"

# Request.supports_chunked_trailers r ;;
- : bool = true

# Request.keep_alive r ;;
- : bool = true

# Request.client_addr r = client_addr ;;
- : bool = true
```

### Parse HTTP/1.0 GET request. Keep-alive should be `false`.

```ocaml
# let r = Request.parse client_addr @@ make_buf_read "1.0" "get" "TE" ;;
val r : Request.server_request = <obj>

# Request.version r;;
- : Http.Version.t = `HTTP_1_0

# Eio.traceln "%a" Header.pp @@ Request.headers r;;
+{
+  Host:  www.example.com;
+  Connection:  TE;
+  TE:  trailers;
+  User-Agent:  cohttp-eio
+}
- : unit = ()

# Request.keep_alive r ;;
- : bool = false
```

### Parse HTTP/1.0 GET request. Keep-alive should be `true`.

```ocaml
# let r = Request.parse client_addr @@ make_buf_read "1.0" "get" "keep-alive, TE" ;;
val r : Request.server_request = <obj>

# Eio.traceln "%a" Header.pp @@ Request.headers r;;
+{
+  Host:  www.example.com;
+  Connection:  keep-alive, TE;
+  TE:  trailers;
+  User-Agent:  cohttp-eio
+}
- : unit = ()

# Request.keep_alive r ;;
- : bool = true
```

### Parse request methods - Head, Delete, Options, Trace, Connect, Post, Put and Patch - correctly.

```ocaml
let parse_method m = 
  let r = Request.parse client_addr @@ make_buf_read "1.1" m "TE" in
  Request.meth r
```

```ocaml
# parse_method "head" = Method.Head ;;
- : bool = true

# parse_method "delete" = Method.Delete ;;
- : bool = true

# parse_method "options" = Method.Options ;;
- : bool = true

# parse_method "trace" = Method.Trace ;;
- : bool = true

# parse_method "connect" = Method.Connect ;;
- : bool = true

# parse_method "post" = Method.Post ;;
- : bool = true

# parse_method "put" = Method.Put ;;
- : bool = true

# parse_method "patch" = Method.Patch ;;
- : bool = true
```

## Request.pp

Pretty-print `Request.client_request`.

```ocaml
# let headers = Http.Header.of_list ["Header1", "val 1"; "Header2", "val 2"] ;;
val headers : Header.t = <abstr>
# let req = 
    Request.client_request 
      ~version:`HTTP_1_1 
      ~headers 
      ~port:8080 
      ~host:"www.example.com" 
      ~resource:"/update" 
      Method.Get 
      Body.none ;;
val req : Body.none Request.client_request = <obj>

# Request.pp Format.std_formatter req ;;
{
  Version:  HTTP/1.1;
  Method:  GET;
  URI:  /update;
  Headers :
    {
      Header1:  val 1;
      Header2:  val 2
    };
  Host:  www.example.com:8080
}
- : unit = ()
```

Pretty-print `Request.server`.

```ocaml
# let headers = Http.Header.of_list ["Header1", "val 1"; "Header2", "val 2"] ;;
val headers : Header.t = <abstr>
# let req = 
    Request.server_request
      ~version:`HTTP_1_1 
      ~headers 
      ~resource:"/update" 
      Method.Get
      client_addr
      (Eio.Buf_read.of_string "")
       ;;
val req : Request.server_request = <obj>

# Request.pp Format.std_formatter req ;;
{
  Version:  HTTP/1.1;
  Method:  GET;
  URI:  /update;
  Headers :
    {
      Header1:  val 1;
      Header2:  val 2
    };
  Client Address:  tcp:127.0.0.1:8081
}
- : unit = ()
```
