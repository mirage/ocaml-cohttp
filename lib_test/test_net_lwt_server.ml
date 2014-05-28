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
  let callback _ req body =
    let uri = Request.uri req in
    Printf.printf "%s\n%!" (Uri.to_string uri);
    match Uri.path uri with
    |""|"/" -> Server.respond_string ~status:`OK ~body:"helloworld" ()
    |"/post" -> begin
       lwt body = Cohttp_lwt_body.to_string body in
       Server.respond_string ~status:`OK ~body ()
    end
    |"/postnodrain" -> begin
       Server.respond_string ~status:`OK ~body:"nodrain" ()
    end
    |"/sse.php" -> begin
       let headers = Header.init_with "content-type" "text/event-stream" in
       let headers = Header.add headers "cache-control" "no-cache" in
       let st,push_st = Lwt_stream.create () in
       let body = Cohttp_lwt_body.of_stream st in
       let cur_time () =
         let tod = Unix.gettimeofday () in
         let tm = Unix.localtime tod in
         let s = Unix.(sprintf "%d:%d:%d" tm.tm_hour tm.tm_min tm.tm_sec) in
         let id = int_of_float (Unix.gettimeofday ()) in
         s,id
       in
       let _, start_time = cur_time () in
       let send_msg id msg =
         let msgs = [
           "data: {";
           sprintf "data: \"msg\": \"%d\"," msg;
           sprintf "data: \"id\": %d" id;
           "data: }" ] in
         let r = sprintf "id: %d\r\n%s\r\n\r\n" id (String.concat "\r\n" msgs) in
         Some r
       in
       let _ =
         try_lwt
           let rec respond () =
              let pp,time = cur_time () in
              print_endline pp;
              (if time - start_time > 10 then
                push_st None
              else
                push_st (send_msg start_time time));
              Lwt_unix.sleep 3.0
              >>= respond
           in respond ()
         with exn -> return ()
       in
       Server.respond ~headers ~flush:true ~status:`OK ~body ()
    end
    |_ ->
       let fname = Server.resolve_file ~docroot:"." ~uri:(Request.uri req) in
       Server.respond_file ~fname ()
  in
  let conn_closed conn () =
    Printf.eprintf "conn %s closed\n%!" (Server.Connection.to_string conn)
  in
  let config = { Server.callback; conn_closed } in
  let address = "0.0.0.0" in
  let port = 8081 in
  let ssl = `SSL (`Crt_file_path "server.crt", `Key_file_path "server.key") in
  let t1 = Server.create ~address ~port config in
  let t2 = Server.create ~mode:ssl ~address ~port:(port+1) config in
  t1 <&> t2

let _ = Lwt_unix.run (make_server ())
