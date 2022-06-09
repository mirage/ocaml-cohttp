Test chunk request processing.
1. Test chunks
2. Test chunk extension parsing
3. Test chunk trailer header processing. Specifically, the in the test sample below

  $ port=8081
  $ test-chunk-server -p ${port} &
  $ running_pid=$!
  $ crlf << EOF | ncat localhost ${port}
  > POST / HTTP/1.1
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
  content-length: 342
  content-type: text/plain; charset=UTF-8
  
  meth: POST
  resource: /
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
