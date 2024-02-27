(lang dune 3.0)

(name cohttp)

(license ISC)

(using mdx 0.2)

(cram enable)

(maintainers "Anil Madhavapeddy <anil@recoil.org>")

(authors
 "Anil Madhavapeddy"
 "Stefano Zacchiroli"
 "David Sheets"
 "Thomas Gazagnaire"
 "David Scott"
 "Rudi Grinberg"
 "Andy Ray"
 "Anurag Soni")

(source
 (github mirage/ocaml-cohttp))

(documentation "https://mirage.github.io/ocaml-cohttp/")

(generate_opam_files true)

(package
 (name cohttp)
 (synopsis "An OCaml library for HTTP clients and servers")
 (description
  "Cohttp is an OCaml library for creating HTTP daemons. It has a portable\nHTTP parser, and implementations using various asynchronous programming\nlibraries.\n\nSee the cohttp-async, cohttp-lwt, cohttp-lwt-unix, cohttp-lwt-jsoo and\ncohttp-mirage libraries for concrete implementations for particular\ntargets.\n\nYou can implement other targets using the parser very easily. Look at the `IO`\nsignature in `lib/s.mli` and implement that in the desired backend.\n\nYou can activate some runtime debugging by setting `COHTTP_DEBUG` to any\nvalue, and all requests and responses will be written to stderr.  Further\ndebugging of the connection layer can be obtained by setting `CONDUIT_DEBUG`\nto any value.\n")
 (depends
  (http
   (= :version))
  (ocaml
   (>= 4.08))
  (re
   (>= 1.9.0))
  (uri
   (>= 2.0.0))
  uri-sexp
  logs
  sexplib0
  (ppx_sexp_conv
   (>= v0.13.0))
  stringext
  (base64
   (>= 3.1.0))
  (fmt :with-test)
  (alcotest :with-test)))

(package
 (name cohttp-top)
 (synopsis "CoHTTP toplevel pretty printers for HTTP types")
 (description
  "This library installs toplevel prettyprinters for CoHTTP\ntypes such as the `Request`, `Response` and `Types` modules.\nOnce this library has been loaded, you can directly see the\nvalues of those types in toplevels such as `utop` or `ocaml`.\n")
 (depends
  (ocaml
   (>= 4.08))
  (cohttp
   (= :version))))

(package
 (name cohttp-lwt)
 (synopsis "CoHTTP implementation using the Lwt concurrency library")
 (description
  "This is a portable implementation of HTTP that uses the Lwt concurrency library\nto multiplex IO.  It implements as much of the logic in an OS-independent way\nas possible, so that more specialised modules can be tailored for different\ntargets.  For example, you can install `cohttp-lwt-unix` or `cohttp-lwt-jsoo`\nfor a Unix or JavaScript backend, or `cohttp-mirage` for the MirageOS unikernel\nversion of the library. All of these implementations share the same IO logic\nfrom this module.")
 (depends
  (ocaml
   (>= 4.08))
  (http
   (= :version))
  (cohttp
   (= :version))
  (lwt
   (>= 5.4.0))
  sexplib0
  (ppx_sexp_conv
   (>= v0.13.0))
  logs
  (uri
   (>= 2.0.0))))

(package
 (name cohttp-lwt-unix)
 (synopsis "CoHTTP implementation for Unix and Windows using Lwt")
 (description
  "An implementation of an HTTP client and server using the Lwt\nconcurrency library. See the `Cohttp_lwt_unix` module for information\non how to use this.  The package also installs `cohttp-curl-lwt`\nand a `cohttp-server-lwt` binaries for quick uses of a HTTP(S)\nclient and server respectively.\n\nAlthough the name implies that this only works under Unix, it\nshould also be fine under Windows too.\n")
 (depends
  (ocaml
   (>= 4.08))
  (http
   (= :version))
  (cohttp
   (= :version))
  (cohttp-lwt
   (= :version))
  (cmdliner
   (>= 1.1.0))
  (lwt
   (>= 3.0.0))
  (conduit-lwt
   (>= 5.0.0))
  (conduit-lwt-unix
   (>= 5.0.0))
  (fmt
   (>= 0.8.2))
  base-unix
  (ppx_sexp_conv
   (>= v0.13.0))
  magic-mime
  logs
  (ounit :with-test)))

(package
 (name cohttp-server-lwt-unix)
 (synopsis "Lightweight Cohttp + Lwt based HTTP server")
 (description
  "This server implementation is faster than cohttp-lwt-unix and is independent of\nconduit.\n")
 (depends
  (ocaml
   (>= 4.08))
  (http
   (= :version))
  (lwt
   (>= 5.5.0))
  (conduit-lwt-unix :with-test)
  (cohttp-lwt-unix
   (and
    :with-test
    (= :version)))
  (cohttp-lwt
   (and
    :with-test
    (= :version)))
  lwt))

(package
 (name cohttp-lwt-jsoo)
 (synopsis "CoHTTP implementation for the Js_of_ocaml JavaScript compiler")
 (description
  "An implementation of an HTTP client for JavaScript, but using the\nCoHTTP types.  This lets you build HTTP clients that can compile\nnatively (using one of the other Cohttp backends such as `cohttp-lwt-unix`)\nand also to native JavaScript via js_of_ocaml.\n")
 (depends
  (ocaml
   (>= 4.08))
  (http
   (= :version))
  (cohttp
   (= :version))
  (cohttp-lwt
   (= :version))
  logs
  (lwt
   (>= 3.0.0))
  (lwt_ppx :with-test)
  (conf-npm :with-test)
  (js_of_ocaml
   (>= 3.3.0))
  (js_of_ocaml-ppx
   (>= 3.3.0))
  (js_of_ocaml-lwt
   (>= 3.5.0))))

(package
 (name cohttp-async)
 (synopsis "CoHTTP implementation for the Async concurrency library")
 (description
  "An implementation of an HTTP client and server using the Async\nconcurrency library. See the `Cohttp_async` module for information\non how to use this.  The package also installs `cohttp-curl-async`\nand a `cohttp-server-async` binaries for quick uses of a HTTP(S)\nclient and server respectively.\n")
 (depends
  (ocaml
   (>= 4.14))
  (http
   (= :version))
  (cohttp
   (= :version))
  (async_kernel
   (>= v0.16.0))
  (async_unix
   (>= v0.16.0))
  (async
   (>= v0.16.0))
  (base
   (>= v0.16.0))
  (core :with-test)
  (core_unix
   (>= v0.14.0))
  (conduit-async
   (>= 1.2.0))
  magic-mime
  (mirage-crypto :with-test)
  logs
  (fmt
   (>= 0.8.2))
  sexplib0
  (ppx_sexp_conv
   (>= v0.13.0))
  (ounit :with-test)
  (uri
   (>= 2.0.0))
  uri-sexp
  ipaddr))

(package
 (name cohttp-mirage)
 (synopsis "CoHTTP implementation for the MirageOS unikernel")
 (description
  "This HTTP implementation uses the Cohttp portable implementaiton\nalong with the Lwt threading library in order to provide a\n`Cohttp_mirage` functor that can be used in MirageOS unikernels\nto build very small and efficient HTTP clients and servers\nwithout having a hard dependency on an underlying operating\nsystem.\n\nPlease see <https://mirage.io> for a self-hosted explanation\nand instructions on how to use this library.")
 (depends
  (ocaml
   (>= 4.08))
  (mirage-flow
   (>= 2.0.0))
  (mirage-channel
   (>= 4.0.0))
  (conduit
   (>= 2.0.2))
  (conduit-mirage
   (>= 2.3.0))
  (mirage-kv
   (>= 3.0.0))
  (lwt
   (>= 2.4.3))
  (cohttp-lwt
   (= :version))
  (cstruct
   (>= 6.0.0))
  (fmt
   (>= 0.8.7))
  astring
  magic-mime
  (ppx_sexp_conv
   (>= v0.13.0))
  (cohttp
   (= :version))))

(package
 (name http)
 (synopsis "Type definitions of HTTP essentials")
 (description
  "This package contains essential type definitions used in Cohttp. It is designed\nto have no dependencies and make it easy for other packages to easily\ninteroperate with Cohttp.")
 (depends
  (ocaml
   (>= 4.08))
  (ppx_expect :with-test)
  (alcotest :with-test)
  (base_quickcheck :with-test)
  (ppx_assert :with-test)
  (ppx_sexp_conv :with-test)
  (ppx_compare :with-test)
  (ppx_here :with-test)
  (crowbar
   (and
    :with-test
    (>= 0.2)))
  (sexplib0 :with-test)))

(package
 (name cohttp-curl)
 (synopsis "Shared code between the individual cohttp-curl clients")
 (description "Use cohttp-curl-lwt or cohttp-curl-async")
 (depends
  (ocaml
   (>= 4.08))
  ocurl
  (http
   (= :version))
  stringext))

(package
 (name cohttp-curl-lwt)
 (synopsis "Cohttp client using Curl & Lwt as the backend")
 (description
  "An HTTP client that relies on Curl + Lwt for the backend. Does not require\nconduit for SSL.")
 (depends
  (ocaml
   (>= 4.08))
  ocurl
  (http
   (= :version))
  (cohttp-curl
   (= :version))
  stringext
  (lwt
   (>= 5.3.0))
  (uri
   (and
    :with-test
    (>= 4.2.0)))
  (alcotest :with-test)
  (cohttp-lwt-unix
   (and
    :with-test
    (= :version)))
  (cohttp
   (and
    :with-test
    (= :version)))
  (cohttp-lwt
   (and
    :with-test
    (= :version)))
  (conduit-lwt :with-test)
  (ounit :with-test)))

(package
 (name cohttp-curl-async)
 (allow_empty)
 (synopsis "Cohttp client using Curl & Async as the backend")
 (description
  "An HTTP client that relies on Curl + Async for the backend. Does not require\nconduit for SSL.")
 (depends
  ocurl
  (http
   (= :version))
  stringext
  (cohttp-curl
   (= :version))
  (core
   (>= v0.16.0))
  (core_unix
   (>= v0.14.0))
  async_kernel
  async_unix
  (cohttp-async
   (and
    :with-test
    (= :version)))
  (uri
   (and
    :with-test
    (>= 4.2.0)))
  (fmt :with-test)
  (ounit :with-test)
  (alcotest :with-test)))

(package
 (name cohttp-bench)
 (allow_empty)
 (synopsis "Benchmarks binaries for Cohttp")
 (description
  "This package contains some benchmarks for http and cohttp.\nThe benchmarks for the server latency will require wrk2\n(https://github.com/giltene/wrk2) to run. The latency graphs\ncan then be generated with HdrHistogram plotter, also available\nonline at https://hdrhistogram.github.io/HdrHistogram/plotFiles.html.")
 (depends
  (core
   (>= v0.13.0))
  core_bench
  (eio
   (>= 0.12))
  (http
   (= :version))
  (cohttp
   (= :version))
  (cohttp-eio
   (= :version))
  (cohttp-lwt-unix
   (= :version))
  (cohttp-server-lwt-unix
   (= :version))
  (cohttp-async
   (= :version))))

(package
 (name cohttp-eio)
 (synopsis "CoHTTP implementation with eio backend")
 (description
  "A CoHTTP server and client implementation based on `eio` library. `cohttp-eio`features a multicore capable HTTP 1.1 server. The library promotes and is built with direct style of coding as opposed to a monadic.")
 (depends
  (alcotest :with-test)
  base-domains
  (cohttp
   (= :version))
  (eio
   (>= 0.12))
  (eio_main :with-test)
  (mdx :with-test)
  logs
  uri
  (tls-eio (and :with-test (>= 0.17.2)))
  (mirage-crypto-rng-eio (and :with-test (>= 0.11.2)))
  fmt
  ptime
  (http
   (= :version))
  (ppx_here :with-test)))
