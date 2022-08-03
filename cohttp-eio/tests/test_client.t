Test Client.get

  $ port=8082
  $ test-server -p ${port} &
  $ running_pid=$!
  $ test-client -p ${port} -t get
  meth: GET
  resource: /get
  version: HTTP/1.1
  headers: Header { Accept = "application/json"; Host = "localhost:8082" }

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
   Accept = "application/json"; Content-Length = "12"; Host = "localhost:8082"
   }
  
  hello world!

  $ kill ${running_pid}
