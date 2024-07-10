(*{{{ Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
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

module Request = struct
  include Cohttp.Request

  include (
    Private.Make (Io) : module type of Private.Make (Io) with type t := t)
  end

module Response = struct
  include Cohttp.Response

  include (
    Private.Make (Io) : module type of Private.Make (Io) with type t := t)
  end

module Connection = Cohttp_lwt.Connection.Make (Net)

module Connection_cache =
  Cohttp_lwt.Connection_cache.Make
    (Connection)
    (struct
      (* : Mirage_time.S *)
      let sleep_ns ns = Lwt_unix.sleep (Int64.to_float ns /. 1_000_000_000.)
    end)

module Connection_proxy =
  Cohttp_lwt.Connection_cache.Make_proxy
    (Connection)
    (struct
      (* : Mirage_time.S *)
      let sleep_ns ns = Lwt_unix.sleep (Int64.to_float ns /. 1_000_000_000.)
    end)

module Client : sig
  (** The [Client] module implements the full UNIX HTTP client interface,
      including the UNIX-specific functions defined in {!C}. *)

  include Cohttp_lwt.S.Client with type ctx = Net.ctx

  val custom_ctx :
    ?ctx:Conduit_lwt_unix.ctx -> ?resolver:Resolver_lwt.t -> unit -> Net.ctx
  (** [custom_ctx ?ctx ?resolver ()] will return a context that is the same as
      the {!default_ctx}, but with either the connection handling or resolution
      module overridden with [ctx] or [resolver] respectively. This is useful to
      supply a {!Conduit_lwt_unix.ctx} with a custom source network interface,
      or a {!Resolver_lwt.t} with a different name resolution strategy (for
      instance to override a hostname to point it to a Unix domain socket). *)
end = struct
  include Cohttp_lwt.Client.Make (Connection)

  let custom_ctx = Net.init
end

module Server = Server
module Debug = Debug
module Net = Net
module IO = Io [@@deprecated "This module is not for public consumption"]

module Private = struct
  module Input_channel = Input_channel
  module IO = Io
end
