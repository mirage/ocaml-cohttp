0.21.1
* Remove -principal from type checking temporarily, to workaround
  a bug in the OCaml 4.03 type checker that causes compilation
  hangs (http://caml.inria.fr/mantis/view.php?id=7305).
* Improve documentation in the `test_xhr.ml` js_of_ocaml test.

0.21.0 (2016-05-22)
* Allow to request paths as strings (#470, #478)

0.20.2 (2016-04-04)
* Update META version (#473)
* uri.services is only required by cohttp.async

0.20.1 (2016-04-01)
* Switch cohttp to use ppx (#457)
* Lwt: Fix leak on HEAD client requests (#467)

0.20.0 (2016-03-25)
* Switch to pa_fields_conv and pa_sexp_conv for camlp4 extensions (#465)
* Compatibility with latest async (#468)
* Async: Add support for SSL parameters in client (#466)
* Lwt: ignore Sig.sigpipe under Windows (#456)
* Lwt: Fix FD leak (#447)
* Lwt: Log uncaught user exceptions
* Async: Close non-persistent async connections (#442)

0.19.3 (2015-09-28):
* Support Async 113.00 by explicitly using the blocking Core `printf` (#431)
* cohttp_curl_async: add `-data-binary` to send POST data. (#425)

0.19.2 (2015-08-20):

* Improve Cohttp_async.Client error handling. When a Uri.t fails to resolve it is
  now included in the error. (#420)

0.19.1 (2015-08-08):

* Bring make_body_writer and write_header in Cohttp.S.Http_io. Needed by
  ocaml-git

0.19.0 (2015-08-05):
Compatibility breaking interface changes:
* Remove `read_form` from the `Request/Response/Header` interfaces
  as this should be done in `Body` handling instead (#401).

New features and bug fixes:
* Remove `IO.write_line` as it was unused in any interfaces.
* Do not use the `lwt` camlp4 extension. No observable external difference.
* Do not return a code stacktrace in the default 500 handler.
* Add `Cohttp.Header.compare` (#411)
* Fix typos in CLI documentation (#413 via @moonlightdrive)
* Use the Lwt 2.5.0 buffer API.
* `Cohttp_lwt.read_response` now has a non-optional `closefn` parameter (#400).
* Add a `Cohttp_lwt_s` module that contains all the Lwt module types
  in one convenient place (#397).

0.18.3 (2015-07-12):
* Allow `DELETE` requests to have request bodies (#383).
* Improve the Lwt client `callv` for HTTP/1.1 pipelined
  requests (#379 via Török Edwin).

0.18.2 (2015-06-19):
* Do not add content encoding for 204's (#375)

0.18.1 (2015-06-05):
* Remove trailing whitespace from headers (#372)
* Don't reverse order of list valued headers (#372)

0.18.0 (2015-06-02):
* Add Cohttp_async.Client.callv. Allows for making requests while reusing an
  HTTP connection (#344)
* Responses of status 1xx/204/304 have no bodies and cohttp should not attempt
  to read them (#355)
* Add top level printers. See cohttp.top findlib package (#363)
* Add `Header.to_string` (#362)
* Fix chunk truncation in chunked transfer encoding (#360)

Compatibility breaking interface changes:
* Remove `Request`/`Response` modules outside of Cohttp pack (#349)

0.17.2 (2015-05-24):
* Remove dependency on the Lwt Camlp4 syntax extension (#334).
* Add `make github` target to push documentation to GitHub Pages
  (#338 from Jyotsna Prakash).
* Add Async integration tests and consolidate Lwt tests using the
  new framework (#337).
* Fix allocation of massive buffer when handling fixed size http bodies (#345)

0.17.1 (2015-04-24):
* [async] Limit buffer size to a maximum of 32K in the Async backend
  (#330 from Stanislav Artemkin).
* Add `Cohttp.Conf.version` with the library version number included.
* Remove debug output from `cohttp-curl-async`.
* Add the beginning of a `DESIGN.md` document to explain the library structure.

0.17.0 (2015-04-17):

Compatibility breaking interface changes:
* `CONNECT` and `TRACE` methods added to `Code`.Exhaustive matches will need updating.

New features and bug fixes:
* `Link` header parsing has been added as `Cohttp.Link`, `Header.get_links` and `Header.add_links`
* `cohttp_server_*` now obeys `HEAD` requests and responds 405 to unknown methods
* `Cohttp_async.Server.response` type is now exposed as a `response * body` pair
* Failure to read a body in a pipelined response no longer terminates the stream
* Fix `cohttp_curl_lwt -X HEAD` sending empty chunked body (#313)
* Fix a bug which left extra `\r\n` in buffer at end of chunked reads
* Fix handling of request URI for query strings and `CONNECT` proxies (#308, #318)
* Fix precedence of `Host` header when request-URI is absolute URI
* Fix request URI path to be non-empty except for * requests (e.g. `OPTIONS *`)

0.16.1 (2015-04-09):
New features and bug fixes:
* Fix handling of request paths starting with multiple slashes (#308)

0.16.0 (2015-03-23):

Compatibility breaking interface changes:
* Response.t and Request.t fields are no longer mutable
* [lwt] Fix types in `post_form` to be a `string * string list` instead
  of a `Header.t` (#257)
* Simplify the `Net` signature which needs to be provided for Lwt servers
  to not be required.  Only the Lwt client needs a `Net` functor argument
  to make outgoing connections. (#274)
* The `Request` and `Response` records are no longer mutable, so use
  functional updates instead via `Fieldslib.Field.fset Request.Fields.<field>`. (#296)
* `Request.has_body` does not permit a body to be set for methods that
  RFC7231 forbids from having one (`HEAD`, `GET` and `DELETE`).

New features and bug fixes:
* Fix linking problem caused by sub-libraries using cohttp modules outside the
  cohttp pack.
* Added async client for S3. (#304)
* Fix String_io.read_line to trim '\r' from end of string (#300)
* Fix `cohttp-server-lwt` to correctly bind to a specific interface (#298).
* Add `Cohttp_async.request` to send raw, unmodified requests.
* Supplying a `content-range` or `content-range` header in any client
  request will always override any other encoding preference (#281).
* Add a `cohttp-lwt-proxy` to act as an HTTP proxy. (#248)
* Extend `cohttp-server-async` file server to work with HTTPS (#277).
* Copy basic auth from `Uri.userinfo` into the Authorization header
  for HTTP requests. (#255)
* Install binaries via an OPAM `.install` file to ensure that they are
  reliably uninstalled. (#252)
* Use the `magic-mime` library to add a MIME type by probing filename
  during static serving in the Lwt/Async backends. (#260)
* Add `Cohttp.Header.add_opt_unless_exists` to set a header only if
  an override wasn't supplied, and to initialise a fresh Header value
  if none is present.
* Do not override user-supplied headers in `post_form` or `redirect`.
* `Request.make` does not inject a `transfer-encoding` header if there
  is no body present in the request (#246).
* `Server.respond` no longer overrides user-supplied headers that
  specify the `content-length` or `transfer-encoding` headers (#268).
* `cohttp_server_lwt` and `cohttp_server_async` now include sizes in
  directory listing titles
* Add `Header.add_multi` to initialise a header structure with multiple
  fields more efficiently (#272).
* Expose `IO.ic` and `IO.oc` types for `Cohttp_async` (#271).
* Skip empty body chunks in `Transfer_io.write` (#270).
* With the Lwt backend, `read` hangs if trying to fetch more than
  `Sys.max_string_length` (which can be triggered on 32-bit platforms).
  Read only a maximum that fits into a string (#282).
* `cohttp-curl-lwt` now takes http method as parameter (#288)
* Fix installation of server binaries in OPAM metadata. (#295)

0.15.2 (2015-02-15):
* When transfer encoding is unknown, read until EOF when body size is unknown. (#241)
* Add some missing documentation to `Cohttp.S.IO` signature. (#233)
* Add `Cohttp.Header.mem` to check if a header exists.
* Add `Cohttp.Conf` module to expose the library version number. (#259)
* Add `Cohttp.Header.add_unless_exists` to update a key if it doesn't already exist. (#244)
* Add `Cohttp.Header.get_location` to retrieve redirection information. (#254)
* [async] Clean up the `Net.lookup` function to use `Or_error.t` instead of raising. (#247)
* [tests] Add more tests for `content-range` handling. (#249)

0.15.1 (2015-01-10):
* Lwt 2.4.7 renamed `blit_bytes_string` to `blit_to_bytes`, so depend
  on the newer API now. (#230)
* Use `cmdliner` in all of the Lwt client and server binaries.  This gives
  `cohttp-lwt-server` a nice Unix-like command-line interface now that
  can be viewed with the `--help` option. (#218 via Runhang Li)
* Improve `oasis` constraints and regenerate `opam` file (#229 via
  Christophe Troestler).

0.15.0 (2014-12-24):

Compatibility breaking interface changes:
* Change `Cohttp_lwt_body.map` to use a non-labelled type to fit the Lwt
  style better (#200).
* Depend on Base64 version 2, which uses `B64` as the toplevel module name (#220).

New features and bug fixes:
* Remove use of deprecated `Lwt_unix.run` and replace it with `Lwt_main.run`.
  Should be no observable external change (#217).
* Improve ocamldoc of `Cohttp.S` signature (#221).

0.14.0 (2014-12-18):

Compatibility breaking interface changes:
* Simplify the Lwt server signature, so that manual construction of
  a `callback` is no longer required (#210).
  Code that previous looked like:
    ```
    let conn_closed (_,conn_id) () = <...>
    let config = { Server.callback; conn_closed } in
    ```
  should now be:
    ```
    let conn_closed (_,conn_id) = <...>
    let config = Server.make ~callback ~conn_closed () in
    ```

* Remove the `Cohttp.Base64` module in favour of the external `base64`
  library (which is now a new dependency).

New features and bug fixes:
* Lwt `respond_error` now defaults to an internal server error if no
  status code is specified (#212).
* Modernise the `opam` file using the OPAM 1.2 workflow (#211).
* Flush the response body to the network by default, rather than
  buffering by default.  The `?flush` optional parameter can still
  be explicitly set to false if flushing is not desired (#205).

0.13.0 (2014-12-05):

Compatibility breaking interface changes:

* Add sexp converters for Conduit contexts and `Lwt` client and server
  modules and module types.

New features and bug fixes:
* Can use the Conduit 0.7+ `CONDUIT_TLS=native` environment variable to
  make HTTPS requests using the pure OCaml TLS stack instead of depending
  on OpenSSL bindings.  All of the installed binaries (client and server)
  can work in this mode.
* Add `Cohttp_lwt_unix_debug` which lets libraries control the debugging
  output from Cohttp.  Previously the only way to do this was to set the
  `COHTTP_DEBUG` environment variable at the program start.
* Add `cohttp-curl-lwt` as a lightweight URI fetcher from the command-line.
  It uses the `cmdliner` as a new dependency.
* Remove build dependency check on `lwt.ssl` for `cohttp.lwt`.
  This has been moved to conduit, so only `lwt.unix` is needed here now.

0.12.0 (2014-11-07):

Compatibility breaking interface changes:

* Rename `Cohttp.Auth.t` to `Cohttp.Auth.credential` and `Cohttp.Auth.req`
  to `Cohttp.Auth.challenge`.  Also expose an `Other` variant
  to make it more extensible for unknown authentication types. The
  `Cohttp.Auth` functions using these types have also been renamed accordingly.
* Rename `Cohttp.Transfer.encoding_to_string` to `string_of_encoding`
  for consistency with the rest of Cohttp's APIs.
* The `has_body` function in the Request and Response modules now
  explicitly signals when the body size is unknown.
* Move all the module type signatures into `Cohttp.S`.
* If users have percent-encoded file names, their resolution is changed:
 `resolve_local_file` in `Cohttp_async` and `Cohttp_lwt` now always
  percent-decode paths (#157)
* Remove the `Cohttp_lwt.Server.server` type synonym to `t`.
* When reading data from a HTTP body stream using the `Fixed` encoding,
  we need to maintain state (bytes remaining) so we know when to finish.
  The `Cohttp.Request` and `Cohttp.Response` interfaces now expose a
  `reader` and `writer` types to track this safely.
* Add `is_empty` function to the `Cohttp.S.Body` module type.
* Add `Strings` representation to `Cohttp.Body` to efficiently hold a
  list of body chunks.
* Move flushing logic for HTTP bodies into the portable `Request` and
  `Response` modules instead of individual Lwt and Async backends.
* Port module interfaces to the latest Conduit (0.6.0+) API.
* Cohttp requires OCaml 4.01.0 or higher now.

New features and bugfixes:

* Add a `Cohttp_lwt_xhr` JavaScript backend that enables Cohttp logic to be
  mapped to `XMLHTTPRequest` in browsers via `js_of_ocaml` (via Andy Ray).
* Add a `Cohttp.String_io` and `String_io_lwt` module that uses OCaml
  `string` or `Buffer.t` to read and write HTTP requests and responses
  instead of network connections.
* `cohttp_server_lwt` and `cohttp_server_async` now return better errors (#158)
* `cohttp_server_lwt` and `cohttp_server_async` now serve indexes directly (#162)
* [lwt] Add `stop` thread to terminate a running server if it finishes (#147).
* Add `Cohttp.Connection.compare` to make ordering of connections possible.
* Add `Body.map` and `Body.as_pipe` to work with HTTP bodies more easily.
* Remove link-time dependency on camlp4 via META fixes (#127).
* Support HTTP methods and versions other than the standard ones. (#142).
* Improve `cohttp_server_lwt` and `cohttp_server_async` directory listings (#158)
* Fix `Cohttp_async.resolve_local_file` directory traversal vulnerability (#158)
* [async] In the Async server, do not close the Reader too early.
* [async] Close file descriptors more eagerly in the HTTP client (#167).
* Reduce thread allocation by replacing `return <const>` with `return_none`,
  `return_unit` or `return_nil`.

0.11.2 (2014-04-21)
* Fix build by add a missing build-deps in _oasis.

0.11.1 (2014-04-17):
* Remove an errant async_ssl reference left in the _oasis file that is
  now handled by the Conduit library (#116).
* Add an Lwt-based SimpleHTTPServer equivalent as `cohttp-server-lwt` (#108).
* `Cohttp.Connection.t` now exposes sexp accessor functions (#117).

0.11.0 (2014-04-01):
* Remove dependency on `ocaml-re` in order to make library POSIX thread-safe.
* Shift most of the connection handling logic out to a Conduit library that
  worries about which SSL library to use, and fails if SSL is not available.
* Add Async-SSL support for both client and server (#102).
* Add Lwt-SSL support for the server side (the client side existed before).
* Fix buggy Async chunked POST handling.

0.10.0 (2014-03-02):
* Interface change: The `Request` and `Response` module types now explicitly
  signal `Eof` and `Invalid` (for errors), to help the backend distinguish them.
* Interface change: Unify HTTP body handling across backends into a `Cohttp.Body`
  module.  This is extended by Async/Lwt implementations with their specific
  ways of handling bodies (Pipes for Async, or Lwt_stream for Lwt).
* [lwt] Interface change: HTTP client calls now raise Lwt exceptions rather
  than return an option type.  This permits better error handling in Lwt.
* [lwt] Interface change: The `Server` callback now always provides a `body`
  argument, since `Cohttp_lwt_body` now explicitly supports empty bodys.
* Add `Cohttp.Header.is_keep_alive` to test if a connection should be reused.
* [lwt] Respect the `keep-alive` header in the server request handling.
* [async] Add a `Body` that takes a `Pipe` or a `string`, similarly to Lwt.
* Install `cohttp-server` binary even if tests are disabled.
* Begin an `examples` directory with some simple uses of the library.

0.9.16 (2014-01-30):
* Add some module type equalities in `Cohttp_lwt_unix` so that
  `Cohttp_lwt_unix.Server.Request.IO.ic` can be equivalen to `Lwt_io.input_channel`.
* Add sexp converters to most Cohttp types (#83).
* Improve Travis tests to cover more upstream users of Cohttp.
* Refactor build flags to let the portable Lwt-core be built independently of Lwt.unix.

0.9.15 (2014-01-11):
* Remove `Cohttp_mirage` libraries, which have now moved to `mirage/mirage-http-*` on GitHub.
* Add an "HTTP only" `Cookie` attribute (#69).
* Fix parsing of cookies with `=` in the values (#71).
* Add `Max-age` support for cookies (#70).
* Make the `Response` record fields mutable to match the `Request` (#67).
* Fix compilation with Async 109.58.00 (#77).
* Make Header handling case-insensitive (by forcing lowercase) (#75).
* Remove the `>>` operator as it was unused and had incorrect precedence (#79).

0.9.14 (2013-12-15):
* Install a `cohttp-server` binary that serves local directory contents via a web server (#54).
* Add a `flush` function to the `IO` module type and implement in Lwt/Async/Mirage.
* Add option `flush` support in the Async and Lwt responders (#52).
* Autogenerate HTTP codes from @citricsquid's JSON representation of the HTTP RFCs.
* Always set `TCP_NODELAY` for Lwt/Unix server sockets for low-latency responses (#58).
* Added a Server-Side Events test-case from the HTML5 Doctor. See `lib_test/README.md`.
* Async.Server response now takes an optional `body` rather than a mandatory `body option` (#62).
* Regenerate build system using OASIS 0.4.0.

0.9.13 (2013-12-10):
* The `cohttp.lwt-core` is now installed as an OS-independent Lwt library.
* Add support for Mirage 1.0, via `cohttp.mirage-unix` and `cohttp.mirage-xen`.
* Add a new `Cohttp.Connection` module to manage server's connections identifiers.
* Share the same configuration type for the different server implementations.
* Add `Accept_types` module to the `Cohttp` pack.

0.9.12 (2013-11-28):
* Improve documentation for `Cohttp.Header`.
* Expose Fieldslib setters and getters for most of the `Cohttp` types (#38).
* `Cohttp.Set_cookie.t` is no longer an abstract type to make it easier to update (#38).
* [Lwt] ignore SIGPIPE unconditionally if using the Lwt/Unix module (#37).
* Rename `Cookie` creation parameters for consistency (interface breaking, see #44).
* Fix transfer-length detection (regression from 0.9.11 in #42).
* Add Merin editor file (#41).

0.9.11 (2013-10-27):
* Request module: When sending a request, add the port information in the host header field if available.
* Request module: When parsing a request, add scheme, host and port information in the uri.
* TCP server: When creating the socket for the server, do not force PF_INET6 but take the sockaddr value.
* Add HTTP OPTIONS method.
* Use getaddrinfo instead of gethostbyname for DNS resolution.
* Async: improve HTTP/1.0 support (#35).
* Build with debug symbols, binary annotations by default.
* Add Travis CI test scripts.

0.9.10 (2013-06-21):
* Add `set-cookie` header extraction functions for clients that read cookies.
* Explicitly flush the debug output when the `COHTTP_DEBUG` env variable is set.
* [async] Add client head/post/patch/delete methods.
* [lwt] Client.head no longer returns a response body, just the metadata.
* [lwt] Do not send chunked encoding headers with GET/DELETE requests that have no body.

0.9.9 (2013-06-12):
* Disable the mirage executable test as it was building too aggressively and breaking builds.

0.9.8 (2013-05-24):
* Lwt interface change: Rewrite Lwt backends to share code, and remove duplicate function calls from Uri.
* Depend on `Uri` 1.3.8+ as it exposes the parameter query functions now removed from `Request`.
* Do not depend on Cstruct in core library, as only Mirage needs it.
* Remove `Cohttp_async.body` type alias and just use `string Pipe.Reader.t` for more explicit types.

0.9.7 (2013-05-10):
* Attach a GC finaliser to the Lwt client to ensure that even an HTTP body isn't consumed, the socket will eventually be closed (#11).
* Add an Async.Server interface, and revise the Client interface to be more in line with Core standards.
* Add 422 Unprocessable Entity code.
* Refactor modules better across Lwt/Async, but incompatible with earlier releases for Async (Lwt is unchanged at present).
* Add user agent string and User-Agent header helper function
* The git history of this release is full of adventures in parameterised monads and refactoring, but this isn't in the actual release. Yet.

0.9.6 (2013-03-18):
* Depend on Async (>= 109.12.00), which has an incompatible API with earlier versions.
* Rearrange core library files for `obuild` support.

0.9.5 (2012-12-29):
* Fix cookie parsing to retrieve the correct header.
* Update to `mirage-net` 0.5.0 API (based on cstruct 0.6.0).

0.9.4 (2012-12-19):
* Add Lwt `respond_redirect` and `respond_need_auth` helpers.
* Add enough Basic authorization support to serve a password-protected website.
* Fix Lwt file serving to not throw exception on trying to serve a directory.
* Port Async interface to 108.07.00 or higher (incompatible
  with earlier versions).

0.9.3 (2012-10-27):
* Add basic cookie support back to the portable library.
* `Cohttp_lwt.Client.post_form` now uses non-chunked encoding for
  the POST instead of chunked.
* Various improvements and tests for the pipelined Lwt Client.callv
* If an Lwt callback does not consume a body, ensure it has
  been drained by the API to prevent future pipelines from stalls.
* Fix handling of Lwt server non-empty POST bodies.
* Map the `put` functions to HTTP PUT instead of POST.

0.9.2 (2012-09-20):
* Add Request.get_param to extract a singleton key from queries.
* Fix chunked encoding handling when short reads occur.
* Install HTML documentation for all enabled drivers.
* Use ocaml-uri-1.3.2 interface for query parsing.
* Lwt: Add Server.respond_file and resolve_file for the Unix
  library to make it easier to serve static files.
* Lwt: Server.respond_not_found takes an optional Uri.t now.

0.9.1 (2012-09-11):
* Functorise for Async, Lwt_unix and Mirage.
* Use URI and Re libraries to not need Str any more.
* More robust parsing for various HTTP headers.

0.9.0 (2012-08-01):
* Initial public release.
