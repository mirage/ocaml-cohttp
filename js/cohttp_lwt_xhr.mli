(*
 * Copyright (c) 2014 Andy Ray <andy.ray@ujamjar.com>
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

(** HTTP client for JavaScript using XMLHttpRequest. *)

module String_io_lwt : S.IO
  with type 'a t = 'a Lwt.t
  and type ic = Cohttp.String_io.buf
  and type oc = Buffer.t

(** The [Request] module holds the information about a HTTP request *)
module Request : Cohttp_lwt.Request with module IO = String_io_lwt

(** The [Response] module holds the information about a HTTP response *)
module Response : Cohttp_lwt.Response with module IO = String_io_lwt

(** The [Client] module implements an HTTP client interface. *)
module Client : Cohttp_lwt.Client with module IO = String_io_lwt

