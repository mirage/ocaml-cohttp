(*
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazazagnaire.org>
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

(** HTTP client and server using the [mirage-tcpip] interfaces. *)

open Net

(** The [Request] module holds the information about a HTTP request,
    and also includes the {! Cohttp_lwt_mirage_io} functions to handle
    large message bodies. *)
module Request : Cohttp_lwt.Request with module IO = Cohttp_mirage_io

(** The [Response] module holds the information about a HTTP response,
    and also includes the {! Cohttp_lwt_mirage_io} functions to handle
    large message bodies. *)
module Response : Cohttp_lwt.Response with module IO = Cohttp_mirage_io

(** The [Client] module implements an HTTP client interface. *)
module Client : Cohttp_lwt.Client with module IO = Cohttp_mirage_io

(** The Mirage [S] module type defines the additional Mirage-specific
    functions that are exposed by the {!  Cohttp_lwt.Server}
    interface.  This is primarily the {! listen} function to actually
    create the server instance and response to incoming requests. *)
module type S = sig
  include Cohttp_lwt.Server with module IO = Cohttp_mirage_io
  val listen : ?timeout:float -> Manager.t -> Nettypes.ipv4_src -> t -> unit Lwt.t
end

(** The [Server] module implement the full Mirage HTTP server
    interface, including the Mirage-specific functions defined in {!
    S }. *)
module Server : S

(** This [listen] call is the same as {! Server.listen}, but here for
    compatibility with the Mirari build tool. *)
val listen : ?timeout:float -> Manager.t -> Nettypes.ipv4_src -> Server.t -> unit Lwt.t
