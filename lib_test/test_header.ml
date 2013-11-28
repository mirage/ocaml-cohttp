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

module H = Cohttp.Header

let valid_auth () =
  let auth = Cohttp.Auth.Basic ("Aladdin", "open sesame") in
  let h = H.add_authorization (H.init ()) auth in
  let digest = H.get h "authorization" in
  assert_equal digest (Some "Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==");
  assert_equal (H.get_authorization h) (Some auth)

let valid_set_cookie () =
  let c = Cohttp.Cookie.Set_cookie_hdr.make ~expiration:`Session
     ~path:"/foo/bar" ~domain:"ocaml.org"
	 ~secure:true ("key", "value") in
  let k, v = Cohttp.Cookie.Set_cookie_hdr.serialize ~version:`HTTP_1_0 c in
  assert_equal ~printer:(fun x -> x) ~msg:"header key" "Set-Cookie" k;
  assert_equal ~printer:(fun x -> x) ~msg:"header value" "key=value; domain=ocaml.org; path=/foo/bar; secure" v

let valid_cookie () =
  let cookies = [ "foo", "bar"; "a", "b" ] in
  let k, v = Cohttp.Cookie.Cookie_hdr.serialize cookies in
  assert_equal ~msg:"key" "cookie" k;
  assert_equal ~msg:"value" "foo=bar; a=b" v;
  let h = Cohttp.Header.of_list [ k, v ] in
  let cookies = Cohttp.Cookie.Cookie_hdr.extract h in
  let printer x = String.concat "; " (List.map (fun (x, y) -> x ^ ":" ^ y) x) in
  assert_equal ~printer ~msg:"headers" [ "foo", "bar"; "a", "b" ] cookies

(* returns true if the result list contains successes only.
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
  let suites = [
    "Valid Auth" >:: valid_auth;
    "Valid Set-Cookie" >:: valid_set_cookie;
    "Valid Cookie" >:: valid_cookie;
  ] in
  let verbose = ref false in
  let set_verbose _ = verbose := true in
  Arg.parse
    [("-verbose", Arg.Unit set_verbose, "Run the test in verbose mode.");]
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    ("Usage: " ^ Sys.argv.(0) ^ " [-verbose]");
  if not
    (List.for_all
       (fun suite -> was_successful (run_test_tt ~verbose:!verbose suite))
       suites)
  then exit 1

