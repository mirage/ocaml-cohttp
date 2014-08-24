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

type 'a io = 'a Lwt.t
type ic = Lwt_io.input_channel
type oc = Lwt_io.output_channel
type ctx = Lwt_unix_conduit.ctx

let resolver = Lwt_unix_resolver.system

let connect_uri ?ctx uri =
  Lwt_unix_conduit.Client.connect_to_uri ?ctx uri

let close_in ic =
  ignore_result (try_lwt Lwt_io.close ic with _ -> return ())

let close_out oc =
  ignore_result (try_lwt Lwt_io.close oc with _ -> return ())

let close' ic oc =
  try_lwt Lwt_io.close oc with _ -> return () >>= fun () ->
  try_lwt Lwt_io.close ic with _ -> return ()

let close ic oc =
  ignore_result (close' ic oc)
