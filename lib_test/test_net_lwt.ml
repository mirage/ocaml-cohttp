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

let make_server () =
  let open Cohttp_lwt in
  let callback conn_id ?body req =
    Server.respond_string ~status:`OK ~body:"helloworld" ()
  in
  let conn_closed conn_id () =
    Printf.eprintf "conn %s closed\n%!" (Server.string_of_conn_id conn_id)
  in
  let config = {
    Server.address = "127.0.0.1"; port=8081;
    callback; conn_closed; root_dir=None; timeout=None
  } in
  Server.main config
     
let make_net_req url () =
  let headers = Header.of_list ["connection","close"] in
  Cohttp_lwt.Client.call ~headers `GET (Uri.of_string url) >>= function 
  |None -> assert false
  |Some (res, None) ->
    let headers = Cohttp_lwt.Response.headers res in
    Header.iter (fun k v -> Printf.eprintf "%s: %s\n%!" k v) headers;
    Printf.eprintf "<no body>\n%!";
    return ()
  |Some (res, Some body) ->
    let headers = Cohttp_lwt.Response.headers res in
    Header.iter (fun k v -> Printf.eprintf "%s: %s\n%!" k v) headers;
    Lwt_stream.iter_s (fun s -> return ()) body

let test_cases =
  let tests = [
    make_net_req "http://anil.recoil.org";
    make_net_req "http://anil.recoil.org/";
    make_net_req "https://github.com/";
(*
    make_server
*)
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
