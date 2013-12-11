CoHTTP is an OCaml library for creating HTTP daemons.  It has a portable
HTTP parser, and implementations using various asynchronous programming
libraries:

* `Cohttp_lwt_unix` uses the [Lwt](http://ocsigen.org/lwt) library, and
specifically the UNIX bindings.
* `Cohttp_async` uses the [Async](https://realworldocaml.org/v1/en/html/concurrent-programming-with-async.html)
library.
* `Cohttp_lwt_mirage` uses the [Mirage](http://www.openmirage.org) interface
to generate standalone unikernels.

You can implement other targets using the parser very easily.  Look at the
`lib/IO.mli` signature and implement that in the desired backend.

You can activate some runtime debugging by setting `COHTTP_DEBUG` to any
value, and all requests and responses will be written to stderr.

For build requirements, please see the `_oasis` file, or use OPAM to install
it from <http://github.com/ocaml/opam>.

## Simple HTTP server

If you install the Async dependency, then a `cohttp-async-server` binary
will also be built and installed that acts in a similar fashion to the
Python `SimpleHTTPServer`.  Just run `cohttp-async-server` in a directory
and it will open up a local port and serve the files over HTTP.
