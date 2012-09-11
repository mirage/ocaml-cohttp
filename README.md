CoHTTP is an OCaml library for creating HTTP daemons.  It has a portable
HTTP parser, and implementations using various asynchronous programming
libraries:

* `Cohttp_lwt_unix` uses the [Lwt](http://ocsigen.org/lwt) library, and
specifically the UNIX bindings.
* `Cohttp_async` uses the [Async](https://bitbucket.org/yminsky/ocaml-core/wiki/DummiesGuideToAsync)
library from Jane Street.
* `Cohttp_lwt_mirage` uses the [Mirage](http://www.openmirage.org) interface
to generate standalone microkernels.

You can implement other targets using parser very easily.  Look at the
`lib/IO.ml` signature, and implement that in the desired backend.

You can activate some runtime debugging by setting `COHTTP_DEBUG` to any
value, and all requests and responses will be written to stderr.

For build requirements, please see the `_oasis` file, or use OPAM to install
it from http://github.com/OCamlPro/opam
