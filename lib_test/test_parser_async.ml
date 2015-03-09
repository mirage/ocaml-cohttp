(*
 * Copyright (c) 2015 Daniel Patterson <dbp@dbpmail.net>
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

let post_req =
"POST /path/script.cgi HTTP/1.0
From: frog@jmarshall.com
User-Agent: HTTPTool/1.0
Content-Type: application/x-www-form-urlencoded
Content-Length: 32

home=Cosby&favorite+flavor=flies"


open Core.Std
open Async.Std

let ic_of_buffer buf = Reader.of_pipe (Info.of_string "") (Pipe.of_list [buf])

let p_sexp f x = x |> f |> Sexplib.Sexp.to_string

let post_form_parse () =
  let open Cohttp_async in
  ic_of_buffer post_req >>= fun ic ->
  Request.read ic >>= function
  | `Ok req ->
    assert_equal true (Request.is_form req);
    Request.read_form req ic >>= fun params ->
    assert_equal ["Cosby"] (List.Assoc.find_exn params "home");
    assert_equal ["flies"] (List.Assoc.find_exn params "favorite flavor");
    assert_raises Not_found (fun () -> List.Assoc.find_exn params "nonexistent");
    (* multiple requests should still work *)
    assert_equal ["Cosby"] (List.Assoc.find_exn params "home");
    return ()
  | _ -> assert false

let test_cases =
  let tests = [
    "post_form_parse", post_form_parse;
  ] in
  List.map ~f:(fun (n,x) -> n >:: (fun () -> Thread_safe.block_on_async_exn x)) tests

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
  Pervasives.exit 1
