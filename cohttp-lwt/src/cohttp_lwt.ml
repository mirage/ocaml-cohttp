(*{{{ Copyright (c) 2012-2013 Anil Madhavapeddy <anil@recoil.org>
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

module type IO = S.IO

module Request = Cohttp.Request [@@deprecated "Use Cohttp.Request directly"]
module Response = Cohttp.Response [@@deprecated "Use Cohttp.Response directly"]
module Connection = Connection
module Connection_cache = Connection_cache
module Client = Client
module Server = Server

(** @deprecated use {!module:Client.Make} instead. *)
module Make_client (IO : IO) (Net : S.Net with module IO = IO) =
  Client.Make (Connection.Make (Net))

module Make_server = Server.Make
(** @deprecated use {!module:Server.Make} instead. *)

module S = S
module Body = Body

module Private = struct
  module Bytebuffer = Bytebuffer
  module String_io = String_io
end
