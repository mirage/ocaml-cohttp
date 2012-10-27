Planned interface-breaking changes before 1.0:

* Make the Header.t header parsing more efficient by only lazily parsing them
  instead of copying into a Map as we do now.

* Make the Lwt response stream bounded (new in lwt-2.4+)

* Server file support and default_spec

* Make Lwt_ssl optional

* Header module needs to deal with repeated keys correctly (by folding
  them into the string list of values)

* Should read_form really be in lib/ ? It could just be implemented in the
  drivers.  But then again, that would be repeated code in all the backends.

Better HTTP support:

- Range requests need to be fully implemented (206)
- 100 Continue should be a noop
- Awwww crap, so much to do : http://www.and.org/texts/server-http
- Lwt Connection: close support
- A client interface that deals with redirects
- Proxy support (manual means a full URI in the request)
- SSL support for Async?

Tests:

- Test the lib_test server scripts via external invocations of
  curl and httperf, so that the tests terminate.

- Test the HTTP timeout support
