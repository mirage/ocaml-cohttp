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

open OUnit
open Printf
open Lwt

open Cohttp
open Cohttp_lwt_unix
module CLU = Conduit_lwt_unix

let make_server () =
  let callback (ch,conn_id) req body =
    let uri = Request.uri req in
    (* For debugging and demonstration of how to get the original
       Lwt_unix.file_descr, see below *)
    let src_info =
      match ch with
      | CLU.TCP {CLU.fd; ip; port} -> begin
          match Lwt_unix.getpeername fd with
          | Lwt_unix.ADDR_INET (ia,port) ->
              sprintf "%s:%d" (Ipaddr.to_string (Ipaddr_unix.of_inet_addr ia)) port
          | Lwt_unix.ADDR_UNIX path -> sprintf "sock:%s" path
      end
     |_ -> "Non-TCP source" in
    Printf.printf "%s : %s from %s\n%!" (Uri.to_string uri)
      (Sexplib.Sexp.to_string_hum (Conduit_lwt_unix.sexp_of_flow ch)) src_info;
    (* Onto URL parsing *)
    match Uri.path uri with
    |""|"/" -> Server.respond_string ~status:`OK ~body:"helloworld" ()
    |"/post" -> begin
       Cohttp_lwt_body.to_string body >>= fun body ->
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
         Lwt.catch (fun () ->
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
         ) (fun exn -> return ())
       in
       Server.respond ~headers ~flush:true ~status:`OK ~body ()
    end
    |_ ->
       let fname = Server.resolve_file ~docroot:"." ~uri:(Request.uri req) in
       Server.respond_file ~fname ()
  in
  let conn_closed (ch,conn_id) =
    Printf.eprintf "conn %s closed\n%!" (Connection.to_string conn_id)
  in
  let config = Server.make ~callback ~conn_closed () in
  let ctx = Cohttp_lwt_unix_net.init () in
  let port = 8081 in
  let tcp_mode = `TCP (`Port port) in
  let ssl_mode = `TLS (`Crt_file_path "server.crt", `Key_file_path "server.key", `No_password, `Port (port+1)) in
  let t1 = Server.create ~ctx ~mode:tcp_mode config in
  let t2 = Server.create ~ctx ~mode:ssl_mode config in
  t1 <&> t2

let _ = Lwt_main.run (make_server ())
