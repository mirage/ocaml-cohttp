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

module IO = Cohttp_lwt_unix_nossl.IO

type ctx = (Conduit.resolvers[@sexp.opaque]) [@@deriving sexp]

let () = Ssl.init ()

let default_ctx = Conduit_lwt.empty

let failwith fmt = Format.kasprintf (fun err -> Lwt.fail (Failure err)) fmt

let uri_to_endpoint uri =
  (match Uri.host uri with
   | None -> failwith "Invalid uri: no host component in %a" Uri.pp uri
   | Some h -> Lwt.return h) >>= fun v ->
  let ( >>= ) x f = match x with Ok x -> f x | Error err -> Error err in
  match Domain_name.(of_string v >>= host), Ipaddr.of_string v with
  | Ok domain_name, _ -> Lwt.return (Conduit.Endpoint.domain domain_name)
  | Error _, Ok v -> Lwt.return (Conduit.Endpoint.ip v)
  | Error _, Error _ -> failwith "Invalid uri: %a" Uri.pp uri

let verify ?host ctx flow =
  let socket = Conduit_lwt.TCP.Protocol.file_descr flow in
  let uninitialized_socket = Lwt_ssl.embed_uninitialized_socket socket ctx in
  let ssl_socket = Lwt_ssl.ssl_socket_of_uninitialized_socket uninitialized_socket in
  Option.iter (Ssl.set_client_SNI_hostname ssl_socket) host ;
  Lwt_ssl.ssl_perform_handshake uninitialized_socket >|= fun v -> Ok v

let connect_uri ~ctx uri =
  uri_to_endpoint uri >>= fun edn ->
  let ctx = match Uri.scheme uri with
    | Some "https" ->
      let port = Option.value ~default:443 (Uri.port uri) in
      let host = Uri.host uri in
      let context = Ssl.create_context Ssl.TLSv1_2 Ssl.Client_context in
      Conduit_lwt.add
        Conduit_lwt_ssl.TCP.protocol
        (Conduit_lwt_ssl.TCP.resolve ~verify:(verify ?host) ~port ~context) ctx
    | (Some "http" | None) ->
      let port = Option.value ~default:80 (Uri.port uri) in
      Conduit_lwt.add Conduit_lwt.TCP.protocol (Conduit_lwt.TCP.resolve ~port) ctx
    | _ -> ctx in
  Conduit_lwt.resolve ctx edn >>= function
  | Ok (Conduit_lwt_ssl.TCP.T (Value socket) as flow) ->
    let ic = Lwt_ssl.in_channel_of_descr socket in
    let oc = Lwt_ssl.out_channel_of_descr socket in
    Lwt.return (flow, ic, oc)
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
