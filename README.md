[![Join the chat at https://gitter.im/mirage/ocaml-cohttp](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/mirage/ocaml-cohttp?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

CoHTTP is an OCaml library for creating HTTP daemons.  It has a portable
HTTP parser, and implementations using various asynchronous programming
libraries:

* `Cohttp_lwt_unix` uses the [Lwt](http://ocsigen.org/lwt) library, and
  specifically the UNIX bindings.
* `Cohttp_async` uses the [Async](https://realworldocaml.org/v1/en/html/concurrent-programming-with-async.html)
  library.
* `Cohttp_lwt` exposes an OS-independent Lwt interface, which is used
  by the [Mirage](http://www.openmirage.org) interface
  to generate standalone microkernels (see the [mirage-http](https://github.com/mirage/mirage-http)
  repository).
* `Cohttp_lwt_xhr` compiles to a JavaScript module that maps the Cohttp
   calls to XMLHTTPRequests.  This is used to compile OCaml libraries like
   the GitHub bindings to JavaScript and still run efficiently.

You can implement other targets using the parser very easily.  Look at the
`lib/IO.mli` signature and implement that in the desired backend.

You can activate some runtime debugging by setting `COHTTP_DEBUG` to any
value, and all requests and responses will be written to stderr.  Further
debugging of the connection layer can be obtained by setting `CONDUIT_DEBUG`
to any value.

## Installation

Latest stable version should be obtained from opam. Make sure to install the
specific backends you want as well. E.g.

```
$ opam install cohttp lwt js_of_ocaml
```

You can also obtain the development release:

```
$ opam pin add cohttp --dev-repo
```

## Binaries

Cohttp comes with a few simple binaries that are handy, useful testing cohttp
itself, and serve as examples of how to use cohttp. The binaries come in two
flavours - Async and Lwt based.

* `$ cohttp-curl-{lwt,async}`

This is a simple curl utility implemented using cohttp. An example of an
invocation is:

```
$ cohttp-curl-lwt -v -X GET "http://www.reddit.com/"
```

* `$ cohttp-server-{lwt,async}`

This binary acts in a similar fashion to the Python `SimpleHTTPServer`. Just
run `cohttp-server-async` in a directory and it will open up a local port and
serve the files over HTTP.

```
$ cohttp-server-async
```

Assuming that the server is running cohttp's source directory:

```
$ cohttp-curl-lwt 'http://0.0.0.0:8080/_oasis'
```
