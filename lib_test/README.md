The Server Side Events example here is lifted from the HTML5 Doctor, which is
under the Apache 2 license at: <https://github.com/html5rocks/www.html5rocks.com>

To run it, build the test cases in the main repository and then:

* `cd lib_test`
* env COHTTP_DEBUG=1 ../_build/lib_test/test_net_lwt_server.native
* Browse to <http://localhost:8081/sse.html>

You should be able to see the timestamp printing to the server
console and reflecting on the clock on the webpage.  Magic!
