(*{{{ Copyright (c) 2014 Anil Madhavapeddy <anil@recoil.org>
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
open Lwt

let google_static_ip = Ipaddr.of_string_exn "213.104.143.99"

let () =
  let point_to_static_ip to_ip svc uri =
     let port =
       match Uri.port uri with
       | None -> svc.Resolver.port
       | Some port -> port in
     return (`TCP (to_ip,port))
  in

  let service = Resolver_lwt_unix.static_service in
  let rewrites = [ "", (point_to_static_ip google_static_ip) ] in
  let resolver = Resolver_lwt.init ~service ~rewrites () in
  let ctx = Cohttp_lwt_unix.Client.custom_ctx ~resolver () in

  let fetch uri =
    Lwt_main.run (
      Cohttp_lwt_unix.Client.get ~ctx (Uri.of_string uri)
      >>= fun (r,b) ->
      Cohttp_lwt_body.to_string b
    ) in

  prerr_endline (fetch "https://google.com");
  prerr_endline (fetch "http://google.com")
