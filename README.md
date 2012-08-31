CoHTTP is an OCaml library for creating HTTP daemons.  It uses cooperative
threading via the LWT library to handle concurrency, making it quite
lightweight and portable.

You can activate some runtime debugging by setting `COHTTP_DEBUG` to any
value, and all requests and responses will be written to stderr.

For build requirements, please see the `_oasis` file, or use OPAM to install
it from http://github.com/OCamlPro/opam
