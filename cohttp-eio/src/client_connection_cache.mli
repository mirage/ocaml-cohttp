(** The client connection cache provides a mechanism for efficient reuse of
    connections.

    {{1} Rationale}

    The canonical considerations related to reusing connections is described in
    {{:https://datatracker.ietf.org/doc/html/rfc7230#section-6.3} Section 6.6 of
    RFC7230}. Additional discussion on can be found in the
    {{:https://developer.mozilla.org/en-US/docs/Web/HTTP/Guides/Connection_management_in_HTTP_1.x}
    MDN's "Connection management in HTTP/1.x"}.

    The considerations relevant for this client library can described concisely
    in terms of Eio's resource model. In Eio the lifetime of resources are bound
    to the switch they are created with. Thus, the lifetime of a socket created
    with [Eio.Net.connect ~sw] is delimited by the [sw]. As a result, clients
    making a high volume of requests or being used within a resource constrained
    environment may face two complementary problems in the absence of connection
    caching:

    1. If many calls are made with a long-lived switch, applications could
    suffer from socket leaks.

    2. Creating new sockets for every request incurs the overhead of repeated
    TCP handshakes, increasing latency and resource usage.

    Connection caching (more commonly called "Connection pooling", a special
    case of {{:https://en.wikipedia.org/wiki/Pool_(computer_science)} resource
    pooling}) addresses these problems.

    {{2} Specification}

    The following requirements specify the intended correct behaviour of the
    connection cache.

    In the following requirements, we refer to "calls" as a generalization of a
    sequence of communications over a connection. In particular, we generalize
    over individual requests, to account for pipelining. 

    1. {b Connection access.} When given the address of an available host, the
    cache must enable calls to be made on connections to that host.

    2. {b End-user cache configuration.} Users of libraries that depend on
    cohttp-eio transitively must be able to set connection cache configurations
    without altering the source code of their intermediate dependencies. (This
    is for general convenience and to maintain compatibility with cohttp-lwt.)

    2. {b Connection lifetimes.} To avoid connections being closed and
    interrupting active communication, for any connection [c] stored in the
    cache and any call [f] that is given access to [c], the lifetime of [c] must
    be at least as long as the lifetime of [f].

    3. {b Connection access.} To avoid communications being corrupted, for
    any connection [c] stored in the cache and any set of calls [F] requesting
    access to [c], only one call in [F] must be given access to [c] at a time.

    4. {b Multiple connections per domain.} To avoid the "head-of-line-blocking"
    problem, multiple open connections may be maintained to the same remote host. 
    However, to comply with {{:https://datatracker.ietf.org/doc/html/rfc7230#section-6.4}
    Section 6.4 of RFC7230}, the number of concurrent open connections should be limited.

    5. {b Closing gracefully.} To deal with timeouts and failures gracefully,
    the cache should close connections when a remote host sends closure signals,
    as per {{:https://datatracker.ietf.org/doc/html/rfc7230#section-6.5} Section
    6.5 of RFC7230}.

    {{3} Compared with [Cohttp_lwt.Connection_cache]}

    This connection cache has been designed to match the API of the
    {!mod:Cohttp_lwt.Connection_cache}, but only as it pertains to its {i
    connection pooling}. It does not directly manage behavior pertaining to
    proxying or governing how calls cary out communication on a connection, once
    it has been given access.
    
*)

type t

val get : unit -> t option
(** TODO doc *)

val use :
  t ->
  https:
    (Uri.t ->
    [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Std.r ->
    [> Eio.Resource.close_ty ] Eio.Flow.two_way)
    option ->
  net:_ Eio.Net.t ->
  Client_connection.address_info ->
  Client_connection.call ->
  Http.Response.t * Eio.Flow.source_ty Eio.Resource.t
(** TODO doc *)

val with_cache :
  ?keep:int64 ->
  ?parallel:int ->
  time:_ Eio.Time.Mono.t ->
  (Eio.Switch.t -> 'a) ->
  'a
(** TODO doc

    @param keep Number of nanoseconds to keep an idle connection around.
    @param parallel
      maximum number of connections to establish to a single endpoint. Beware: A
      single hostname may resolve to multiple endpoints. In such a case
      connections may be created in excess to what was intended. *)
