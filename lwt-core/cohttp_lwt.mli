(*{{{ Copyright (c) 2013-2014 Anil Madhavapeddy <anil@recoil.org>
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
  }}}*)

open Cohttp

(** Portable Lwt implementation of HTTP client and server, without
    depending on a particular I/O implementation.  The various [Make]
    functors must be instantiated by an implementation that provides
    a concrete IO monad. *)

module type IO = S.IO with type 'a t = 'a Lwt.t
(** The IO module is specialized for the [Lwt] monad. *)

module S : (module type of Cohttp_lwt_s)

(** Aliases for module types inside S. These are deprecated and are only
    here for backwards comaptibility *)
module type Client = S.Client
module type Server = S.Server
module type Net = S.Net

open Cohttp_lwt_s

module Request : (Cohttp.S.Request with type t = Cohttp.Request.t)
module Response : (Cohttp.S.Response with type t = Cohttp.Response.t)

(** The [Make_client] functor glues together a {! Cohttp.S.IO } implementation
    to send requests down a connection that is established by the  {! Net }
    module.  The resulting module satisfies the {! Client } module type. *)
module Make_client (IO:IO) (Net:Net with module IO = IO) : Client
  with type ctx = Net.ctx

(** The [Make_server] functor glues together a {! Cohttp.S.IO } implementation
    to send requests down a connection that is established by the  {! Net }
    module.  The resulting module satisfies the {! Server } module type. *)
module Make_server (IO:IO) : Server with module IO = IO
