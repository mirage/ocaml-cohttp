Planned interface-breaking changes before 1.0:

* Make the Header.t header parsing more efficient by only lazily parsing them
  instead of copying into a Map as we do now.

* Make the Lwt response stream bounded (new in lwt-2.4+)

