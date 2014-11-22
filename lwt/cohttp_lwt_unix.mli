(*
 * Copyright (c) 2012-2013 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

(** HTTP client and server using the [Lwt_unix] interfaces. *)

(** {2 Request and Response modules} *)

(** The [Request] module holds the information about a HTTP request, and
    also includes the {! Cohttp_lwt_unix_io} functions to handle large
    message bodies. *)
module Request : Cohttp_lwt.Request with module IO = Cohttp_lwt_unix_io

(** The [Response] module holds the information about a HTTP response, and
    also includes the {! Cohttp_lwt_unix_io} functions to handle large
    message bodies. *)
module Response : Cohttp_lwt.Response with module IO = Cohttp_lwt_unix_io

(** {2 Module types for Client and Server} *)

(** The [Client] module type defines the additional UNIX-specific functions
  that are exposed in addition to the {!Cohttp_lwt.Client} interface. *)
module type C = sig

  include Cohttp_lwt.Client
    with module IO = Cohttp_lwt_unix_io
     and module Request = Request
     and module Response = Response
     and type ctx = Cohttp_lwt_unix_net.ctx

  (** [custom_ctx ?ctx ?resolver ()] will return a context that is the
     same as the {!default_ctx}, but with either the connection handling
     or resolution module overridden with [ctx] or [resolver] respectively.

     This is useful to supply a {!Conduit_lwt_unix.ctx} with a custom
     source network interface, or a {!Resolver_lwt.t} with a different
     name resolution strategy (for instance to override a hostname to
     point it to a Unix domain socket). *)
  val custom_ctx:
    ?ctx:Conduit_lwt_unix.ctx ->
    ?resolver:Resolver_lwt.t -> unit -> ctx
end

(** This module type defines the additional UNIX-specific functions that are
  exposed in addition to the {! Cohttp_lwt.Server} interface.  These are
  primarily filesystem functions, and also {! create} to actually bind
  the server to a socket and respond to incoming requests. *)
module type S = sig

  include Cohttp_lwt.Server with module IO = Cohttp_lwt_unix_io
                             and module Request = Request
                             and module Response = Response
                             and type ctx = Cohttp_lwt_unix_net.ctx

  val resolve_file : docroot:string -> uri:Uri.t -> string

  val respond_file :
    ?headers:Cohttp.Header.t ->
    fname:string -> unit -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t

  val create :
    ?timeout:int ->
    ?stop:unit Lwt.t ->
    ?ctx:Cohttp_lwt_unix_net.ctx ->
    ?mode:Conduit_lwt_unix.server -> t -> unit Lwt.t
end

(** {2 Lwt-Unix Client and Server implementations} *)

(** The [Client] module implements the full UNIX HTTP client interface,
  including the UNIX-specific functions defined in {!C }. *)
module Client : C

(** The [Server] module implements the full UNIX HTTP server interface,
  including the UNIX-specific functions defined in {!S}. *)
module Server : S
