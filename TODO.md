Planned interface-breaking changes before 1.0:

* Functorize the use of Lwt in lib/ so that we can plug in Async and Mirage as
  well as Unix Lwt.  This should remain broadly compatible with existing users,
  but will probably have a new module name.

* Make header parsing more efficient by only lazily parsing them instead of
  copying into a Hashtbl as we do now.

