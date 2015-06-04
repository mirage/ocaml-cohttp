 (*
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
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

(* Miscellaneous net-helpers used by Cohttp. Ideally, these will disappear
 * into some connection-management framework such as andrenth/release *)

open Lwt

module IO = Cohttp_lwt_unix_io

type ctx = {
  ctx: Conduit_lwt_unix.ctx;
  resolver: Resolver_lwt.t;
} with sexp_of

let init ?(ctx=Conduit_lwt_unix.default_ctx)
         ?(resolver=Resolver_lwt_unix.system) () =
  { ctx; resolver }

let default_ctx = {
  resolver = Resolver_lwt_unix.system;
  ctx = Conduit_lwt_unix.default_ctx;
}

let connect_uri ~ctx uri =
  Resolver_lwt.resolve_uri ~uri ctx.resolver
  >>= fun endp ->
  Conduit_lwt_unix.endp_to_client ~ctx:ctx.ctx endp
  >>= fun client ->
  Conduit_lwt_unix.connect ~ctx:ctx.ctx client

let close c = Lwt.catch (fun () -> Lwt_io.close c) (fun _ -> return_unit)

let close_in ic = ignore_result (close ic)

let close_out oc = ignore_result (close oc)

let close ic oc = ignore_result (close ic >>= fun () -> close oc)
