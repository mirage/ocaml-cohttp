(*{{{ Copyright (c) 2015 David Sheets <sheets@alum.mit.edu>
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

(** Basic satisfaction of {!Cohttp_lwt.Net} *)

type ctx = { ctx : Conduit_lwt_unix.ctx; resolver : Resolver_lwt.t }
[@@deriving sexp_of]

include
  Cohttp_lwt.S.Net
    with module IO = Io
     and type ctx := ctx
     and type endp = Conduit.endp
     and type client = Conduit_lwt_unix.client

val init : ?ctx:Conduit_lwt_unix.ctx -> ?resolver:Resolver_lwt.t -> unit -> ctx
(** [init ?ctx ?resolver ()] is a network context that is the same as the
    {!default_ctx}, but with either the connection handling or resolution module
    overridden with [ctx] or [resolver] respectively. This is useful to supply a
    {!Conduit_lwt_unix.resolver} with a custom source network interface, or a
    {!Resolver_lwt.t} with a different name resolution strategy (for instance to
    override a hostname to point it to a Unix domain socket). *)
