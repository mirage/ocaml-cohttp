(*
 * Copyright (c) 2014 Hannes Menhert
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

open Lwt
open Cohttp
open Cohttp_lwt_unix

let client uri =
  Printf.printf "client with uri %s\n%!" uri ;
  let uri = Uri.of_string uri in
  Printf.printf "client get\n%!" ;
  Client.get uri >>= fun (resp, body) ->
  Printf.printf "get returned\n%!" ;
  Cohttp_lwt_body.to_string body >>= fun s ->
  Printf.printf "body: %d\n" (String.length s) ;
  return_unit

let _ =
  Lwt_main.run
    (match Sys.argv with
     | [| _ ; uri |] -> client uri
     | args -> Printf.printf "%s URI\n%!" args.(0) ; return_unit)
