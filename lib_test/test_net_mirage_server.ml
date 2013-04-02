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
open Cohttp_lwt_mirage

let ip =
  let open Net.Nettypes in
  ( ipv4_addr_of_tuple (10l,0l,0l,2l),
    ipv4_addr_of_tuple (255l,255l,255l,0l),
   [ipv4_addr_of_tuple (10l,0l,0l,1l)]
  )

let make_server () =
  let callback conn_id ?body req =
    Server.respond_string ~status:`OK ~body:"helloworld" ()
  in
  let conn_closed conn_id () =
    Printf.eprintf "conn %s closed\n%!" (Server.string_of_conn_id conn_id)
  in
  let spec = { Server.callback; conn_closed } in
  Net.Manager.create (fun mgr interface id ->
    let src = None, 8081 in
    Net.Manager.configure interface (`IPv4 ip) >>= fun () ->
    listen mgr src spec
  )
    
let _ = OS.Main.run (make_server ()) 
