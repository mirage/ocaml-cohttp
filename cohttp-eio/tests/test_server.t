Test GET success.

  $ port=8080
  $ test-server -p ${port} &
  $ running_pid=$!
  $ crlf << EOF | ncat localhost ${port}
  > GET /get HTTP/1.1
  > 
  > EOF
  HTTP/1.1 200 OK
  content-length: 63
  content-type: text/plain; charset=UTF-8
  
  meth: GET
  resource: /get
  version: HTTP/1.1
  headers: Header {  }
  $ kill ${running_pid}

Test GET error.
The test should respond with error message since we are trying to read request body. HTTP 1.1 doesn't support request bodies in GET.

  $ port=8081
  $ test-server -p ${port} &
  $ running_pid=$!
  $ crlf << EOF | ncat localhost ${port}
  > GET /get_error HTTP/1.1
  > 
  > EOF
  HTTP/1.1 200 OK
  content-length: 4
  content-type: text/plain; charset=UTF-8
  
  PASS
  $ kill ${running_pid}

Test POST

  $ port=8082
  $ test-server -p ${port} &
  $ running_pid=$!
  $ crlf << EOF | ncat localhost ${port}
  > POST /post HTTP/1.0
  > Content-Length:12
  > 
  > hello world!
  > EOF
  HTTP/1.1 200 OK
  content-length: 100
  content-type: text/plain; charset=UTF-8
  
  meth: POST
  resource: /post
  version: HTTP/1.0
  headers: Header { Content-Length = "12" }
  
  hello world!
  $ kill ${running_pid}

Test chunk request processing:
1. Test chunks
2. Test chunk extension parsing
3. Test chunk trailer header processing

  $ port=8083
  $ test-server -p ${port} &
  $ running_pid=$!
  $ crlf << EOF | ncat localhost ${port}
  > POST /handle_chunk HTTP/1.1
  > Content-Type: text/plain
  > Transfer-Encoding: chunked
  > Trailer: Expires, Header1
  > 
  > 7;ext1=ext1_v;ext2=ext2_v;ext3
  > Mozilla
  > 9
  > Developer
  > 7
  > Network
  > 0
  > Expires: Wed, 21 Oct 2015 07:28:00 GMT
  > Header1: Header1 value text
  > Header2: Header2 value text
  > 
  > EOF
  HTTP/1.1 200 OK
  content-length: 354
  content-type: text/plain; charset=UTF-8
  
  meth: POST
  resource: /handle_chunk
  version: HTTP/1.1
  headers: Header {
   Content-Length = "23"; Header1 = "Header1 value text";
   Content-Type = "text/plain" }
  
  size: 7
   data: Mozilla
   extensions:
    name: ext1
    value: ext1_v;
    name: ext2
    value: ext2_v;
    name: ext3
    value: 
  size: 9
   data: Developer
   extensions: 
  size: 7
   data: Network
   extensions: 

  $ kill ${running_pid}
