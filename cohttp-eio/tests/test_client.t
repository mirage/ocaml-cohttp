Test Client.get

  $ port=8082
  $ test-server -p ${port} &
  $ running_pid=$!
  $ test-client -p ${port} -t get
  meth: GET
  resource: /get
  version: HTTP/1.1
  headers: Header {
   Accept = "application/json"; User-Agent = "cohttp-eio"; TE = "trailers";
   Connection = "TE"; Host = "localhost:8082" }
  $ kill ${running_pid}

Test Client.post

  $ port=8082
  $ test-server -p ${port} &
  $ running_pid=$!
  $ test-client -p ${port} -t post
  meth: POST
  resource: /post
  version: HTTP/1.1
  headers: Header {
   Accept = "application/json"; Content-Length = "12";
   User-Agent = "cohttp-eio"; TE = "trailers"; Connection = "TE";
   Host = "localhost:8082" }
  
  hello world!
  $ kill ${running_pid}


Test posting "chunked" data
  $ port=8082
  $ test-server -p ${port} &
  $ running_pid=$!
  $ test-client -p ${port} -t post_chunk
  meth: POST
  resource: /handle_chunk
  version: HTTP/1.1
  headers: Header {
   Content-Length = "23"; Header1 = "Header1 value text";
   Content-Type = "text/plain"; User-Agent = "cohttp-eio"; TE = "trailers";
   Connection = "TE"; Host = "localhost:8082" }
  
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

Test "chunked" response in client and "chunked" response writing in server. 
  $ port=8082
  $ test-server -p ${port} &
  $ running_pid=$!
  $ test-client -p ${port} -t get_chunk
  $ kill ${running_pid}
  $ cat client_chunks2.txt
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
  
  
  
  Header {
  Content-Length = "23"; Header1 = "Header1 value text";
  Content-Type = "text/plain"
  }
