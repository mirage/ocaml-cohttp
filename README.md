## ocaml-cohttp -- an OCaml library for HTTP clients and servers

[![Join the chat at https://gitter.im/mirage/ocaml-cohttp](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/mirage/ocaml-cohttp?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Cohttp is an OCaml library for creating HTTP daemons. It has a portable
HTTP parser, and implementations using various asynchronous programming
libraries:

* `Cohttp_lwt_unix` uses the [Lwt](http://ocsigen.org/lwt) library, and
  specifically the UNIX bindings.
* `Cohttp_async` uses the [Async](https://realworldocaml.org/v1/en/html/concurrent-programming-with-async.html)
  library.
* `Cohttp_lwt` exposes an OS-independent Lwt interface, which is used
  by the [Mirage](http://www.openmirage.org) interface
  to generate standalone microkernels use the cohttp-mirage subpackage.
* `Cohttp_lwt_xhr` compiles to a JavaScript module that maps the Cohttp
   calls to XMLHTTPRequests.  This is used to compile OCaml libraries like
   the GitHub bindings to JavaScript and still run efficiently.

You can implement other targets using the parser very easily. Look at the `IO`
signature in `lib/s.mli` and implement that in the desired backend.

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

### Findlib (Ocamlfind)

Cohttp ships with 6 findlib libraries:

* cohttp - Base `Cohttp` module. No platform specific functionality.
* cohttp-async - Async backend `Cohttp_async`
* cohttp-lwt-jsoo - Jsoo (XHR) client
* cohttp-lwt - Lwt backend without unix specifics.
* cohttp-lwt-unix - Unix based lwt backend
* cohttp-top - Print cohttp types in the toplevel (`#require "cohttp-top"`)

## Client Tutorial

Cohttp provides clients for Async, Lwt, and jsoo (Lwt based). In this tutorial,
we will use the lwt client but it should be easily translateable to Async.

To create a simple request, use one of the methods in `Cohttp_lwt_unix.Client`.
`call` is the most general, there are also http method specialized such as
`get`, `post`, etc.

For example downloading the reddit frontpage:

```ocaml
(* client_example.ml *)
open Lwt
open Cohttp
open Cohttp_lwt_unix

let body =
  Client.get (Uri.of_string "http://www.reddit.com/") >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

let () =
  let body = Lwt_main.run body in
  print_endline ("Received body\n" ^ body)
```

Build with:

```
ocamlbuild -pkg cohttp-lwt-unix client_example.native
```

There's a few things to notice:

* We open 2 modules. `Cohttp` contains the backend independent stuff and
  `Cohttp_lwt_unix` is the lwt + unix specific stuff.

* `Client.get` accepts a `Uri.t` and makes an http request. `Client.get` also
  accepts optional arguments for things like header information.
* The http response is returned in a tuple. The first element of the tuple
  contains the response's status code, headers, http version, etc. The second
  element contains the body.
* The body is then converted to a string and is returned (after the length is
  printed). Note that `Cohttp_lwt.Body.to_string` hence it's up to us to keep
  a reference to the result.
* We must trigger lwt's event loop for the request to run. `Lwt_main.run` will
  run the event loop and return with final value of `body` which we then print.

Consult the following modules for reference:

* [Cohttp_lwt.Client](https://github.com/mirage/ocaml-cohttp/blob/master/lwt-core/cohttp_lwt.mli)
* [Cohttp_async.Client](https://github.com/mirage/ocaml-cohttp/blob/master/async/cohttp_async.mli)

## Basic Server Tutorial

Implementing a server in cohttp is mostly equivalent to implementing a function
of type:

```ocaml
conn -> Cohttp.Request.t -> Cohttp_lwt.Body.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
```

The parameters are self explanatory but we'll summarize them quickly here:

* `conn` - contains connection information
* `Cohttp.Request.t` - Request information such as method, uri, headers, etc.
* `Cohttp_lwt.Body.t` - Contains the request body. You must manually decode the
  request body into json, form encoded pairs, etc. For cohttp, the body is
  simply binary data.

Here's an example of a simple cohttp server that outputs back request
information.

```ocaml
(* server_example.ml *)
open Lwt
open Cohttp
open Cohttp_lwt_unix

let server =
  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.to_string in
    let meth = req |> Request.meth |> Code.string_of_method in
    let headers = req |> Request.headers |> Header.to_string in
    body |> Cohttp_lwt.Body.to_string >|= (fun body ->
      (Printf.sprintf "Uri: %s\nMethod: %s\nHeaders\nHeaders: %s\nBody: %s"
         uri meth headers body))
    >>= (fun body -> Server.respond_string ~status:`OK ~body ())
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () = ignore (Lwt_main.run server)
```

Build with:

```
ocamlbuild -pkg cohttp-lwt-unix server_example.native
```

The following modules are useful references:

* [Cohttp_lwt.Server](https://github.com/mirage/ocaml-cohttp/blob/master/lwt-core/cohttp_lwt_s.ml) - Common to mirage and Unix
* [Cohttp_lwt_unix.Server](https://github.com/mirage/ocaml-cohttp/blob/master/lwt/cohttp_lwt_unix.mli) - Unix specific.
* [Cohttp_async.Server](https://github.com/mirage/ocaml-cohttp/blob/master/async/cohttp_async.mli)


## Installed Binaries

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

Assuming that the server is running in cohttp's source directory:

```
$ cohttp-curl-lwt 'http://0.0.0.0:8080/_oasis'
```
## Important Links
- [API Documentation](http://mirage.github.io/ocaml-cohttp/)
