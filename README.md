mirage-http-unix
=================

Unix implementation of the Mirage HTTP client and server.  Based on the
[Cohttp](http://github.com/mirage/ocaml-cohttp) HTTP implementation.

This implementation currently only works using the Mirage TCP/IP stack
in userspace, but will eventually also work with normal kernel sockets
(and potentially even invoke `curl`).

E-mail: <mirageos-devel@lists.xenproject.org>
