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

(* Perform a DNS lookup on the addr and generate a sockaddr *)
let build_sockaddr addr port =
  try_lwt
    (* should this be lwt hent = Lwt_lib.gethostbyname addr ? *)
    let hent = Unix.gethostbyname addr in
    return (Unix.ADDR_INET (hent.Unix.h_addr_list.(0), port))
  with _ -> 
    raise_lwt (Failure ("cant resolve hostname: " ^ addr))

(* TODO move to ocaml-uri as a /etc/services module *)
let port_of_uri uri =
  match Uri.port uri with
  |None -> begin
     match Uri.scheme uri with 
     |Some "https" -> 443 (* TODO: actually support https *)
     |Some "http" | Some _ |None -> 80
  end
  |Some p -> p

(* Vanilla TCP connection *)
module Tcp_client = struct
  let connect sa =
    let fd = Lwt_unix.socket (Unix.domain_of_sockaddr sa) Unix.SOCK_STREAM 0 in
    lwt () = Lwt_unix.connect fd sa in
    let ic = Lwt_io.of_fd ~mode:Lwt_io.input fd in
    let oc = Lwt_io.of_fd ~mode:Lwt_io.output fd in
    return (ic, oc)

  let close (ic,oc) =
    let _ = try_lwt Lwt_io.close ic with _ -> return () in
    try_lwt Lwt_io.close oc with _ -> return ()
end

(* SSL TCP connection *)
module Ssl_client = struct
  let sslctx =
    Ssl.init ();
    Ssl.create_context Ssl.SSLv23 Ssl.Client_context

  let connect sa =
    let fd = Lwt_unix.socket (Unix.domain_of_sockaddr sa) Unix.SOCK_STREAM 0 in
    lwt () = Lwt_unix.connect fd sa in
    lwt sock = Lwt_ssl.ssl_connect fd sslctx in
    let ic = Lwt_ssl.in_channel_of_descr sock in
    let oc = Lwt_ssl.out_channel_of_descr sock in
    return (ic,oc)

  let close (ic,oc) =
    let _ = try_lwt Lwt_ssl.close ic with _ -> return () in
    try_lwt Lwt_ssl.close oc with _ -> return ()
end

module Tcp_server = struct

  let close (ic,oc) =
    let _ = try_lwt Lwt_io.close ic with _ -> return () in
    try_lwt Lwt_io.close oc with _ -> return ()

  let init_socket sockaddr =
    let suck = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Lwt_unix.setsockopt suck Unix.SO_REUSEADDR true;
    Lwt_unix.bind suck sockaddr;
    Lwt_unix.listen suck 15;
    suck

  let process_accept ~sockaddr ~timeout callback (client,_) =
    let ic = Lwt_io.of_fd Lwt_io.input client in
    let oc = Lwt_io.of_fd Lwt_io.output client in
 
    let c = callback ic oc in
    let events = match timeout with
      |None -> [c]
      |Some t -> [c; (Lwt_unix.sleep (float_of_int t)) ] in
    Lwt.pick events >> close (ic,oc)
  
  let init ~sockaddr ~timeout callback =
    let s = init_socket sockaddr in
    while_lwt true do
      Lwt_unix.accept s >>=
      process_accept ~sockaddr ~timeout callback
    done
end

let connect_uri uri =
  lwt sa = build_sockaddr (Uri.host_with_default uri) (port_of_uri uri) in
  match Uri.scheme uri with
  |Some "https" -> Ssl_client.connect sa
  |Some "http" -> Tcp_client.connect sa
  |Some _ | None -> fail (Failure "unknown scheme")

let connect ?(ssl=false) host port =
  lwt sa = build_sockaddr host port in
  match ssl with
  |true -> Ssl_client.connect sa
  |false -> Tcp_client.connect sa

let close_in ic =
  ignore_result (try_lwt Lwt_io.close ic with _ -> return ())

let close_out oc =
  ignore_result (try_lwt Lwt_io.close oc with _ -> return ())

let close ic oc =
  let _ = try_lwt Lwt_io.close ic with _ -> return () in
  try_lwt Lwt_io.close oc with _ -> return ()

let close' ic oc =
  ignore_result (close ic oc)
