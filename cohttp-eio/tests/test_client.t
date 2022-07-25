Test Client.get

  $ test-server &
  $ running_pid=$!
  $ test-client get
  meth: GET
  resource: /get
  version: HTTP/1.1
  headers: Header { Accept = "application/json" }

  $ kill ${running_pid}

Test Client.post

  $ test-server &
  $ running_pid=$!
  $ test-client post
  meth: POST
  resource: /post
  version: HTTP/1.1
  headers: Header { Accept = "application/json"; Content-Length = "12" }
  
  hello world!

  $ kill ${running_pid}
