(*
 * Copyright (c) 2014 Anil Madhavapeddy <anil@recoil.org>
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

open Core.Std
open Async.Std
open Cohttp_async

let show_headers h =
  Cohttp.Header.iter (fun k v -> List.iter v ~f:(Printf.eprintf "%s: %s\n%!" k)) h

let make_net_req uri () =
  let uri = Uri.of_string uri in
  let headers = Cohttp.Header.of_list [ "connection", "close" ] in
  Client.get ~headers uri
  >>= fun (res, body) ->
   show_headers (Cohttp.Response.headers res);
   body
   |> Body.to_pipe
   |> Pipe.iter ~f:(fun b -> prerr_endline ("XX " ^ b); return ())

let _ =
  let open Command.Spec in
  Command.async_basic ~summary:"Fetch URL and print it"
    (empty+> anon ("url" %: string))
    make_net_req
  |> Command.run

