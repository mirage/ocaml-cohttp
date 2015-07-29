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

open Cohttp_lwt_unix

let make_net_req url () =
  let headers = Cohttp.Header.of_list ["connection","close"] in
  Client.call ~headers `GET (Uri.of_string url) >>= fun (res, body) ->
  let headers = Response.headers res in
  Cohttp.Header.iter
    (fun k v -> List.iter (Printf.eprintf "%s: %s\n%!" k) v) headers;
  Lwt_stream.iter_s (fun s -> return ()) (Cohttp_lwt_body.to_stream body)

let make_net_reqv () =
  let last_header = Cohttp.Header.of_list ["connection","close"] in
  let reqs = [
      Request.make ~meth:`GET (Uri.of_string "/foo"), `Empty;
      Request.make ~meth:`HEAD (Uri.of_string "/foo2"), `Empty;
      Request.make ~meth:`GET ~headers:last_header (Uri.of_string "/foo3"), `Empty;
    ] in
  let uri = Uri.of_string "http://5.153.225.51" in
  Client.callv uri (Lwt_stream.of_list reqs) >>= fun resp ->
  (* Consume the bodies, and we should get 3 responses *)
  let num = ref 0 in
  Lwt_stream.iter_s (fun (res,body) ->
    (* Consume the body *)
    incr num;
    Cohttp_lwt_body.to_string body >>= fun _ ->
    assert_equal (Response.status res) `Not_found;
    return ()
  ) resp >>= fun () ->
  assert_equal !num 3;
  (* Run the callv without consuming bodies *)
  Client.callv uri (Lwt_stream.of_list reqs) >>= fun resp ->
  let num = ref 0 in
  Lwt_stream.iter_s (fun (res,body) ->
    (* Do not consume the body *)
    incr num;
    assert_equal (Response.status res) `Not_found;
    return ()
  ) resp >>= fun () ->
  assert_equal ~printer:string_of_int 3 !num;
  return ()

let parser_tests =
  let tests = [
    "recoil.org",  `Quick, make_net_req "http://anil.recoil.org";
    "recoil.org/", `Quick, make_net_req "http://anil.recoil.org/";
    "github.com/", `Quick, make_net_req "https://github.com/";
    "pipelined",   `Quick, make_net_reqv;
  ] in
  List.map (fun (n,s,x) -> n, s, (fun () -> Lwt_main.run (x ()))) tests

;;
Printexc.record_backtrace true;
Alcotest.run "test_header" [
  "Parser", parser_tests;
]
