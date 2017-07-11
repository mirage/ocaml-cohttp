(*
 * Copyright (c) 2012-2015 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazazagnaire.org>
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
 * %%NAME%% %%VERSION%%
 *)

(** Cohttp client and server implementations for Mirage *)

(** HTTP client. *)
module Client: sig
  include Cohttp_lwt.S.Client
  val ctx: Resolver_lwt.t -> Conduit_mirage.t -> ctx
end

(** HTTP server. *)
module Server (Flow: Mirage_flow_lwt.S): sig
  include Cohttp_lwt.S.Server with type IO.conn = Flow.flow
  val listen: t -> IO.conn -> unit Lwt.t
end

(** HTTP server with conduit. *)
module Server_with_conduit : sig
  include Cohttp_lwt.S.Server with type IO.conn = Conduit_mirage.Flow.flow
  val connect:
    Conduit_mirage.t ->
    (Conduit_mirage.server -> t -> unit Lwt.t) Lwt.t
end
