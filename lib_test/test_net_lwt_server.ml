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

open OUnit
open Printf
open Lwt

open Cohttp
open Cohttp_lwt_unix

let make_server () =
  let callback conn_id ?body req =
    match Uri.path (Request.uri req) with
    |""|"/" -> Server.respond_string ~status:`OK ~body:"helloworld" ()
    |"/post" -> begin
       lwt body = Cohttp_lwt_body.string_of_body body in
       Server.respond_string ~status:`OK ~body ()
    end
    |"/postnodrain" -> begin
       Server.respond_string ~status:`OK ~body:"nodrain" ()
    end
    |_ ->
       let fname = Server.resolve_file ~docroot:"." ~uri:(Request.uri req) in
       Server.respond_file ~fname ()
  in
  let conn_closed conn_id () =
    Printf.eprintf "conn %s closed\n%!" (Connection.to_string conn_id)
  in
  let config = { Server.callback; conn_closed } in
  Server.create ~address:"0.0.0.0" ~port:8081 config

let _ = Lwt_unix.run (make_server ())
