(*
 * Copyright (c) 2013 Anil Madhavapeddy <anil@recoil.org>
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

module type S = (module type of Slwt)

open Slwt

(** Portable Lwt implementation of HTTP client and server, without
    depending on a particular I/O implementation.  The various [Make]
    functors must be instantiated by an implementation that provides
    a concrete IO monad. *)

(** Functor to build a concrete {! Request } from an IO implementation *)
module Make_request(IO:Cohttp.S.IO) : Request with module IO = IO

(** Functor to build a concrete {! Response } from an IO implementation *)
module Make_response(IO:Cohttp.S.IO) : Response with module IO = IO

(** The [Make_client] functor glues together a {! Cohttp.S.IO } implementation
    with {! Cohttp.Request } and {! Cohttp.Response } to send requests down
    a connection that is established by the  {! Net } module.
    The resulting module satisfies the {! Client } module type. *)
module Make_client
    (IO:Cohttp.S.IO with type 'a t = 'a Lwt.t)
    (Request:Request with module IO = IO)
    (Response:Response with module IO = IO)
    (Net:Net with module IO = IO) :
    Client with module IO=IO and module Request=Request and module Response=Response

(** The [Make_server] functor glues together a {! Cohttp.S.IO } implementation
    with {! Cohttp.Request } and {! Cohttp.Response } to send requests down
    a connection that is established by the  {! Net } module.
    The resulting module satisfies the {! Server } module type. *)
module Make_server
    (IO:Cohttp.S.IO with type 'a t = 'a Lwt.t)
    (Request:Request with module IO=IO)
    (Response:Response with module IO=IO)
    (Net:Net with module IO = IO) :
    Server with module IO = IO
            and module Request = Request
            and module Response = Response
