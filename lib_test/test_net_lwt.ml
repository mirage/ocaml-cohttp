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
      Request.make ~meth:`GET (Uri.of_string "/foo2"), `Empty;
      Request.make ~meth:`GET ~headers:last_header (Uri.of_string "/foo3"), `Empty;
    ] in
  lwt resp = Client.callv "89.16.177.154" 80 (Lwt_stream.of_list reqs) in
  (* Consume the bodies, and we should get 3 responses *)
  let num = ref 0 in
  Lwt_stream.iter_s (fun (res,body) ->
    (* Consume the body *)
    incr num;
    lwt body = Cohttp_lwt_body.to_string body in
    assert_equal (Response.status res) `Not_found;
    return ()
  ) resp >>= fun () ->
  assert_equal !num 3;
  (* Run the callv without consuming bodies (i.e. a bug), and we only
     get one *)
  lwt resp = Client.callv "89.16.177.154" 80 (Lwt_stream.of_list reqs) in
  let num = ref 0 in
  Lwt_stream.iter_s (fun (res,body) ->
    (* Do not consume the body *)
    incr num;
    assert_equal (Response.status res) `Not_found;
    return ()
  ) resp >>= fun () ->
  assert_equal !num 1;
  return ()

let test_cases =
  let tests = [
    make_net_req "http://anil.recoil.org";
    make_net_req "http://anil.recoil.org/";
    make_net_req "https://github.com/";
    make_net_reqv;
  ] in
  List.map (fun x -> "test" >:: (fun () -> Lwt_unix.run (x ()))) tests

(* Returns true if the result list contains successes only.
   Copied from oUnit source as it isnt exposed by the mli *)
let rec was_successful =
  function
    | [] -> true
    | RSuccess _::t
    | RSkip _::t ->
        was_successful t
    | RFailure _::_
    | RError _::_
    | RTodo _::_ ->
        false

let _ =
  let suite = "Parser" >::: test_cases in
  let verbose = ref false in
  let set_verbose _ = verbose := true in
  Arg.parse
    [("-verbose", Arg.Unit set_verbose, "Run the test in verbose mode.");]
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    ("Usage: " ^ Sys.argv.(0) ^ " [-verbose]");
  if not (was_successful (run_test_tt ~verbose:!verbose suite)) then
  exit 1
