CoHTTP is an OCaml library for creating HTTP daemons.  It has a portable
HTTP parser, and implementations using various asynchronous programming
libraries:

* `Cohttp_lwt_unix` uses the [Lwt](http://ocsigen.org/lwt) library, and
  specifically the UNIX bindings.
* `Cohttp_async` uses the [Async](https://realworldocaml.org/v1/en/html/concurrent-programming-with-async.html)
  library.
* `Cohttp_lwt` exposes an OS-independent Lwt interface, which is used
  by the the [Mirage](http://www.openmirage.org) interface
  to generate standalone microkernels (see the [mirage-http](https://github.com/mirage/mirage-http)
  repository).

You can implement other targets using the parser very easily.  Look at the
`lib/IO.mli` signature and implement that in the desired backend.

You can activate some runtime debugging by setting `COHTTP_DEBUG` to any
value, and all requests and responses will be written to stderr.

For build requirements, please see the `_oasis` file, or use OPAM to install
it from <http://github.com/ocaml/opam>.

## Simple HTTP server

If you install the Async dependency, then a `cohttp-server-async` binary will
also be built and installed that acts in a similar fashion to the Python
`SimpleHTTPServer`.  Just run `cohttp-server-async` in a directory and it will
open up a local port and serve the files over HTTP.

There is also an Lwt version of the SimpleHTTPServer installed as the
`cohttp-server-lwt` binary.  The source code for both is in the `bin/`
subdirectory and is a good starting point for how to write servers using
the library.
