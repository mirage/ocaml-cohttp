(*
 * Copyright (c) 2012 David Sheets <sheets@alum.mit.edu>
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

module A = Cohttp.Accept

let test s v () =
  let s' = A.media_ranges s in
  assert_equal ~printer:A.string_of_media_ranges v s'

let valid_tests = [
  "text/plain", [1000,A.MediaType ("text","plain"),[]];
  "text/*", [1000,A.AnyMediaSubtype "text",[]];
  "*/*", [1000,A.AnyMedia,[]];
  "*/*;q=1", [1000,A.AnyMedia,[]];
  "*/*;q=0", [0,A.AnyMedia,[]];
  "*/*;q=1.", [1000,A.AnyMedia,[]];
  "*/*;q=1.0", [1000,A.AnyMedia,[]];
  "*/*;q=.0", [0,A.AnyMedia,[]];
  "*/*;q=.", [0,A.AnyMedia,[]];
  "*/*;q=0.", [0,A.AnyMedia,[]];
  "*/*;q=0.1", [100,A.AnyMedia,[]];
  "image/*,text/*", [
    1000,A.AnyMediaSubtype "image",[];
    1000,A.AnyMediaSubtype "text",[];
  ];
  "text/plain; q=0.8; charset=utf-8,text/html;q=0.9;charset=utf-8", [
    800,A.MediaType ("text","plain"),["charset",A.T"utf-8"];
    900,A.MediaType ("text","html"),["charset",A.T"utf-8"];
  ];
  "text/*;foo=\"bar\"", [1000,A.AnyMediaSubtype "text",["foo",A.S"bar"]];
  "*/*;qu=\"\\\"\"", [1000,A.AnyMedia,["qu",A.S"\""]];
  "*/*;f=\";q=0,text/plain\"", [1000,A.AnyMedia,["f",A.S";q=0,text/plain"]];
]

let valid_test_suite = List.map (fun (s,v) -> s >:: (test s v)) valid_tests

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
  let suite = "Valid Accept" >::: valid_test_suite in
  let verbose = ref false in
  let set_verbose _ = verbose := true in
  Arg.parse
    [("-verbose", Arg.Unit set_verbose, "Run the test in verbose mode.");]
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    ("Usage: " ^ Sys.argv.(0) ^ " [-verbose]");
  if not (was_successful (run_test_tt ~verbose:!verbose suite)) then
  exit 1

