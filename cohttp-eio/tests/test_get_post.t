Test GET

  $ test-server &
  $ running_pid=$!
  $ crlf << EOF | ncat localhost 8080
  > GET / HTTP/1.1
  > 
  > EOF
  HTTP/1.1 200 OK
  content-length: 62
  content-type: text/plain; charset=UTF-8
  
  meth: GET
  resource: /
  version: HTTP/1.1
  headers: Header {  }
  
  $ kill ${running_pid}

Test POST

  $ test-server &
  $ running_pid=$!
  $ crlf << EOF | ncat localhost 8080
  > POST / HTTP/1.0
  > Content-Length:12
  > 
  > hello world!
  > EOF
  HTTP/1.1 200 OK
  content-length: 96
  content-type: text/plain; charset=UTF-8
  
  meth: POST
  resource: /
  version: HTTP/1.0
  headers: Header { Content-Length = "12" }
  
  hello world!
  $ kill ${running_pid}

Test parser
  $ test-server &
  $ running_pid=$!
  $ crlf << EOF | ncat localhost 8080
  > GET /wp-content/uploads/2010/03/hello-kitty-darth-vader-pink.jpg HTTP/1.1
  > Host: www.kittyhell.com
  > User-Agent: Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6; ja-JP-mac; rv:1.9.2.3) Gecko/20100401 Firefox/3.6.3 Pathtraq/0.9
  > Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
  > Accept-Language: ja,en-us;q=0.7,en;q=0.3
  > Accept-Encoding: gzip,deflate
  > Accept-Charset: Shift_JIS,utf-8;q=0.7,*;q=0.7
  > Keep-Alive: 115
  > Connection: keep-alive
  > Cookie: wp_ozh_wsa_visits=2; wp_ozh_wsa_visit_lasttime=xxxxxxxxxx;__utma=xxxxxxxxx.xxxxxxxxxx.xxxxxxxxxx.xxxxxxxxxx.xxxxxxxxxx.x;__utmz=xxxxxxxxx.xxxxxxxxxx.x.x.utmccn=(referral)|utmcsr=reader.livedoor.com|utmcct=/reader/|utmcmd=referral
  > 
  > EOF
  HTTP/1.1 200 OK
  content-length: 780
  content-type: text/plain; charset=UTF-8
  
  meth: GET
  resource: /wp-content/uploads/2010/03/hello-kitty-darth-vader-pink.jpg
  version: HTTP/1.1
  headers: Header {
   Cookie =
   "wp_ozh_wsa_visits=2; wp_ozh_wsa_visit_lasttime=xxxxxxxxxx;__utma=xxxxxxxxx.xxxxxxxxxx.xxxxxxxxxx.xxxxxxxxxx.xxxxxxxxxx.x;__utmz=xxxxxxxxx.xxxxxxxxxx.x.x.utmccn=(referral)|utmcsr=reader.livedoor.com|utmcct=/reader/|utmcmd=referral";
   Connection = "keep-alive"; Keep-Alive = "115";
   Accept-Charset = "Shift_JIS,utf-8;q=0.7,*;q=0.7";
   Accept-Encoding = "gzip,deflate";
   Accept-Language = "ja,en-us;q=0.7,en;q=0.3";
   Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8";
   User-Agent =
   "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6; ja-JP-mac; rv:1.9.2.3) Gecko/20100401 Firefox/3.6.3 Pathtraq/0.9";
   Host = "www.kittyhell.com" }
  
  $ kill ${running_pid}

