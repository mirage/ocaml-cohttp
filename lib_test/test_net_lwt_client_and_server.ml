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

let port = 8081
let address = "127.0.0.1"
let url = Uri.of_string (sprintf "http://%s:%d/post" address port)
let url_shutdown = Uri.of_string (sprintf "http://%s:%d/shutdown" address port)

let make_server () =
  let callback conn_id req body =
    match Uri.path (Request.uri req) with
    |""|"/" ->
       Server.respond_string ~status:`OK ~body:"helloworld" ()
    |"/post" -> begin
       lwt body = Cohttp_lwt_body.to_string body in
       Server.respond_string ~status:`OK ~body ()
    end
    |"/postnodrain" ->
       Server.respond_string ~status:`OK ~body:"nodrain" ()
    |_ -> exit 0
  in
  let conn_closed _ () = () in
  lwt ctx = Lwt_unix_conduit.init ~src:address () in
  let mode = `TCP (`Port port) in
  let config = { Server.callback; conn_closed } in
  Server.create ~ctx ~mode config

let not_none n t fn =
  match_lwt t with
  |None -> prerr_endline ("ERR None " ^ n); exit 1
  |Some x -> return (fn x) >>= fun () -> return (prerr_endline ("OK " ^ n))

let not_none_s n t fn =
  match_lwt t with
  |None -> prerr_endline ("ERR None " ^ n); exit 1
  |Some x -> fn x >>= fun () -> return (prerr_endline ("OK " ^ n))

let lwt_test_s t ~name ~assert_ =
  Lwt.catch (fun () ->
    t >>= assert_ >|= fun () -> prerr_endline ("OK " ^ name)
  ) (fun _ ->
    print_endline ("ERR " ^ name);
    exit 1)

let lwt_test t ~name ~assert_ =
  lwt_test_s t ~name ~assert_:(fun a -> a |> assert_ |> return)

let client () =
  (* Do a set of single calls first and consume the body *)
  for_lwt i = 0 to 1000 do
    lwt_test ~name:"get 1" (Client.get url) ~assert_:(fun (_,b) -> assert(b = `Empty))
    >>= fun () ->
    lwt_test_s ~name:"post 1" (Client.post ~body:(Cohttp_lwt_body.of_string "foobar") url)
     ~assert_:(fun (r,b) ->
       lwt b = Cohttp_lwt_body.to_string b in
    lwt_test ~name:"get 1" (Client.get url) ~assert_:(fun (_,b) -> assert(b = `Empty))
    >>= fun () ->
    lwt_test_s ~name:"post 1" (Client.post ~body:(Cohttp_lwt_body.of_string "foobar") url)
     ~assert_:(fun (r,b) ->
       lwt b = Cohttp_lwt_body.to_string b in
       assert (b = "foobar");
       return ()
     ))
  done >>= fun () ->
  (* Repeat but do not consume body *)
  for_lwt i = 0 to 2000 do
    try_lwt
      lwt_test ~name:"get 1" (Client.get url) ~assert_:(fun (r,b) -> assert(b = `Empty))
      >>= fun () ->
      lwt_test_s ~name:"post 1" (Client.post ~body:(Cohttp_lwt_body.of_string "foobar") url)
        ~assert_:(fun (r,b) -> return ())
    with exn ->
      printf "got error, running gc\n%!";
      Gc.compact ();
      lwt_test ~name:"get 1" (Client.get url) ~assert_:(fun (r,b) -> assert(b = `Empty))
      >>= fun () ->
      lwt_test_s ~name:"post 1" (Client.post ~body:(Cohttp_lwt_body.of_string "foobar") url)
        ~assert_:(fun (r,b) -> return ()) 
  done >>= fun () ->
  (* Do a callv *)
  let body () = Cohttp_lwt_body.of_string "foobar" in
  let body1 = body () in
  let body2 = body () in
  let reqs = Lwt_stream.of_list [
    Request.make ~encoding:Transfer.Chunked (Uri.of_string "/post"), body1;
    Request.make ~encoding:Transfer.Chunked ~headers:(Header.of_list ["connection","close"])
      (Uri.of_string "/post"), body2;
  ] in
  lwt resp = Client.callv url reqs in
  Lwt_stream.iter_s (fun (res, body) ->
    lwt body = Cohttp_lwt_body.to_string body in 
    assert(body="foobar");
    return ()
  ) resp >>= fun () -> 
  lwt _ =  Client.get url_shutdown in
  return (exit 1) 
  
let _ = 
  (* Fork into a client and server *)
  match Lwt_unix.fork () with
  |0 -> (* child / client *)
    Unix.sleep 2;
    Printf.eprintf "client is %d\n%!" (Unix.getpid ());
    Lwt_unix.run (client ())
  |pid -> (* parent / server *)
    Printf.eprintf "server is %d\n%!" (Unix.getpid ());
    Lwt_unix.run (make_server ()) 
