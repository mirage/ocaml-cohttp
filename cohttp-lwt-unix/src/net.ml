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

(* Miscellaneous net-helpers used by Cohttp. Ideally, these will disappear
 * into some connection-management framework such as andrenth/release *)

open Lwt.Infix

module IO = Io

type ctx = (Conduit.resolvers[@sexp.opaque]) [@@deriving sexp]

let () = Mirage_crypto_rng_unix.initialize ()

let authenticator ~host:_ _ = Ok None

let tls_config =
  Tls.Config.client ~authenticator ()

let empty =
  Conduit_lwt.empty
  |> Conduit_lwt.add Conduit_lwt.TCP.protocol
    (Conduit_lwt.TCP.resolve ~port:80)
  |> Conduit_lwt.add ~priority:10 Conduit_lwt_tls.TCP.protocol
    (Conduit_lwt_tls.TCP.resolve ~port:443 ~config:tls_config)

let failwith fmt = Format.kasprintf (fun err -> Lwt.fail (Failure err)) fmt

let connect_uri ?host:(default= "localhost") ~ctx uri =
  let domain_name = Domain_name.(host_exn (of_string_exn (Uri.host_with_default ~default uri))) in
  Conduit_lwt.resolve ctx domain_name >>= function
  | Ok flow ->
    let ic, oc = Conduit_lwt.io_of_flow flow in
    Lwt.return (flow, ic, oc)
  | Error err ->
    failwith "%a" Conduit_lwt.pp_error err

let close c = Lwt.catch
  (fun () -> Lwt_io.close c)
  (fun e ->
    Logs.warn (fun f -> f "Closing channel failed: %s" (Printexc.to_string e));
    Lwt.return_unit
  )

let close_in ic = Lwt.ignore_result (close ic)

let close_out oc = Lwt.ignore_result (close oc)

let close ic oc = Lwt.ignore_result (close ic >>= fun () -> close oc)
