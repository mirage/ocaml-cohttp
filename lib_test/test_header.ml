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
  let auth = `Basic ("Aladdin", "open sesame") in
  let h = H.add_authorization (H.init ()) auth in
  let digest = H.get h "authorization" in
  assert_equal digest (Some "Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==");
  assert_equal (H.get_authorization h) (Some auth)

let valid_set_cookie () =
  let c = Cohttp.Cookie.Set_cookie_hdr.make ~expiration:`Session
     ~path:"/foo/bar" ~domain:"ocaml.org"
     ~secure:true ~http_only:true ("key", "value") in
  let k, v = Cohttp.Cookie.Set_cookie_hdr.serialize ~version:`HTTP_1_0 c in
  assert_equal ~printer:(fun x -> x) ~msg:"header key" "Set-Cookie" k;
  assert_equal ~printer:(fun x -> x) ~msg:"header value" "key=value; domain=ocaml.org; path=/foo/bar; secure; httponly" v;
  let c = Cohttp.Cookie.Set_cookie_hdr.make ~expiration:(`Max_age 100L)
     ~path:"/foo/bar" ~domain:"ocaml.org" ("key", "value") in
  let k, v = Cohttp.Cookie.Set_cookie_hdr.serialize ~version:`HTTP_1_0 c in
  assert_equal ~printer:(fun x -> x) ~msg:"header key2" "Set-Cookie" k;
  assert_equal ~printer:(fun x -> x) ~msg:"header value2" "key=value; Max-Age=100; domain=ocaml.org; path=/foo/bar" v;
  let k, v = Cohttp.Cookie.Set_cookie_hdr.serialize ~version:`HTTP_1_1 c in
  assert_equal ~printer:(fun x -> x) ~msg:"header key 1.1" "Set-Cookie2" k;
  assert_equal ~printer:(fun x -> x) ~msg:"header value 1.1" "Domain=ocaml.org; Max-Age=100; Path=/foo/bar; Version=1" v

let cookie_printer x =
  String.concat "; " (List.map (fun (x, y) -> x ^ ":" ^ y) x)

let cookie_with_eq_val () =
  let cookies = [("test","me=")] in
  let (k, v) = Cohttp.Cookie.Cookie_hdr.serialize cookies in
  let h = Cohttp.Header.of_list [ k, v ] in
  let cookies = Cohttp.Cookie.Cookie_hdr.extract h in
  assert_equal ~printer:cookie_printer cookies [("test", "me=")]

let valid_cookie () =
  let cookies = [ "foo", "bar"; "a", "b" ] in
  let k, v = Cohttp.Cookie.Cookie_hdr.serialize cookies in
  assert_equal ~msg:"key" "cookie" k;
  assert_equal ~msg:"value" "foo=bar; a=b" v;
  let h = Cohttp.Header.of_list [ k, v ] in
  let cookies = Cohttp.Cookie.Cookie_hdr.extract h in
  assert_equal ~printer:cookie_printer
    ~msg:"headers" [ "foo", "bar"; "a", "b" ] cookies

let opt_printer f = function
  | None -> "nothing"
  | Some x -> Printf.sprintf "'%s'" (f x)

let get_media_type () =
  let mt = " foo/bar ; charset=UTF-8" in
  let header = Cohttp.Header.init_with "content-type" mt in
  assert_equal ~msg:"media type" ~printer:(opt_printer (fun x -> x))
    (Some "foo/bar") (Cohttp.Header.get_media_type header)

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

let list_valued_header () =
  let h = H.init () in
  let h = H.add h "accept" "foo" in
  let h = H.add h "accept" "bar" in
  assert_equal
    ~printer:(function
      | None -> "None"
      | Some x -> x) (H.get h "accept") (Some "bar,foo")

module Content_range = struct
  let h1 = H.of_list [("Content-Length", "123")]
  let h2 = H.of_list [("Content-Range", "bytes 200-300/1000")]
  let none () = assert_equal None (H.init () |> H.get_content_range)
  let content_length () = assert_equal (Some 123L) (H.get_content_range h1)
  let content_range () = assert_equal (Some 101L) (H.get_content_range h2)
end

let _ =
  let suites = [
    "Media Type" >:: get_media_type;
    "Valid Auth" >:: valid_auth;
    "Valid Set-Cookie" >:: valid_set_cookie;
    "Valid Cookie" >:: valid_cookie;
    "Cookie with =" >:: cookie_with_eq_val;
    "Content Range - none" >:: Content_range.none;
    "Content Range - content-length" >:: Content_range.content_length;
    "Content Range - content-range" >:: Content_range.content_range;
    "Header - get list valued" >:: list_valued_header;
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

