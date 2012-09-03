Planned interface-breaking changes before 1.0:

* Make the Header.t header parsing more efficient by only lazily parsing them
  instead of copying into a Map as we do now.

* Make the Lwt response stream bounded (new in lwt-2.4+)

* Server file support and default_spec

Better HTTP support:

- Range requests need to be fully implemented (206)
- 100 Continue should be a noop
- Lwt Connection: close support
- A client interface that deals with redirects
- Proxy support (manual means a full URI in the request)
- SSL support for Async?
