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

open Lwt.Infix

module Channel = Mirage_channel.Make(Conduit_mirage_flow)
module HTTP_IO = Io.Make(Channel)


module Net_IO = struct

  module IO = HTTP_IO

  type ctx = (Conduit.resolvers[@sexp.opaque]) [@@deriving sexp]

  let default_ctx = Conduit.empty

  let failwith fmt = Fmt.kstrf (fun err -> Lwt.fail (Failure err)) fmt

  let uri_to_endpoint uri =
    (match Uri.host uri with
     | None -> failwith "Invalid uri: no host component in %a" Uri.pp uri
     | Some h -> Lwt.return h) >>= fun v ->
    let ( >>= ) x f = match x with Ok x -> f x | Error err -> Error err in
    match Domain_name.(of_string v >>= host), Ipaddr.of_string v with
    | Ok domain_name, _ -> Lwt.return (Conduit.Endpoint.domain domain_name)
    | Error _, Ok v -> Lwt.return (Conduit.Endpoint.ip v)
    | Error _, Error _ -> failwith "Invalid uri: %a" Uri.pp uri

  let connect_uri ~ctx uri =
    uri_to_endpoint uri >>= fun edn ->
    Conduit_mirage.resolve ctx edn >>= function
    | Ok flow ->
      let ch = Channel.create flow in
      Lwt.return (flow, ch, ch)
    | Error err ->
      failwith "%a" Conduit_mirage.pp_error err

  let close_in _ = ()
  let close_out _ = ()
  let close ic _oc = Lwt.ignore_result @@ Lwt.catch
    (fun () -> Channel.close ic)
    (fun e  ->
      Logs.warn (fun f ->
        f "Closing channel failed: %s" (Printexc.to_string e));
      Lwt.return @@ Ok ()
    )

end

(* Build all the core modules from the [Cohttp_lwt] functors *)
include Cohttp_lwt.Make_client(HTTP_IO)(Net_IO)
