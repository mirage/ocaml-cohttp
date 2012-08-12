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

let basic_req =
"GET /index.html HTTP/1.1\r\nHost: www.example.com\r\n\r\n"

let basic_res =
"HTTP/1.1 200 OK
Date: Mon, 23 May 2005 22:38:34 GMT
Server: Apache/1.3.3.7 (Unix) (Red-Hat/Linux)
Last-Modified: Wed, 08 Jan 2003 23:11:55 GMT
Etag: \"3f80f-1b6-3e1cb03b\"
Accept-Ranges:  none
Content-Length: 0
Connection: close
Content-Type: text/html; charset=UTF-8"

let basic_res_content =
"HTTP/1.1 200 OK
Date: Mon, 23 May 2005 22:38:34 GMT
Server: Apache/1.3.3.7 (Unix) (Red-Hat/Linux)
Last-Modified: Wed, 08 Jan 2003 23:11:55 GMT
Etag: \"3f80f-1b6-3e1cb03b\"
Accept-Ranges:  none
Content-Length: 32
Connection: close
Content-Type: text/html; charset=UTF-8

home=Cosby&favorite+flavor=flies"

let post_req =
"POST /path/script.cgi HTTP/1.0
From: frog@jmarshall.com
User-Agent: HTTPTool/1.0
Content-Type: application/x-www-form-urlencoded
Content-Length: 32

home=Cosby&favorite+flavor=flies"

let post_data_req =
"POST /path/script.cgi HTTP/1.0
From: frog@jmarshall.com
User-Agent: HTTPTool/1.0
Content-Length: 32

home=Cosby&favorite+flavor=flies"

let post_chunked_req =
"POST /foo HTTP/1.1
Date: Fri, 31 Dec 1999 23:59:59 GMT
Content-Type: text/plain
Transfer-Encoding: chunked

1a; ignore-stuff-here
abcdefghijklmnopqrstuvwxyz
10
1234567890abcdef
0
some-footer: some-value
another-footer: another-value

"

let chunked_res =
"HTTP/1.1 200 OK
Date: Fri, 31 Dec 1999 23:59:59 GMT
Content-Type: text/plain
Transfer-Encoding: chunked

1a; ignore-stuff-here
abcdefghijklmnopqrstuvwxyz
10
1234567890abcdef
0
some-footer: some-value
another-footer: another-value

"

let basic_res_plus_crlf = basic_res ^ "\r\n\r\n"

open Lwt

let basic_req_parse () =
  let open Cohttp_lwt in
  let open IO in
  let ic = ic_of_buffer (Lwt_bytes.of_string basic_req) in
  Parser.parse_request_fst_line ic >>=
  function
  |Some (meth, uri, ver) ->
    assert_equal ver `HTTP_1_1;
    assert_equal meth `GET;
    assert_equal (Uri.to_string uri) "/index.html";
    return ()
  |None -> assert false

let basic_res_parse res () =
  let open Cohttp_lwt in
  let open IO in
  let ic = ic_of_buffer (Lwt_bytes.of_string res) in
  Parser.parse_response_fst_line ic >>=
  function
  |Some (version, status) ->
     (* Parse first line *)
     assert_equal version `HTTP_1_1;
     assert_equal status `OK;
     (* Now parse the headers *)
     Parser.parse_headers ic >>= fun headers ->
     assert_equal (Header.get headers "connection") ["close"];
     assert_equal (Header.get headers "accept-ranges") ["none"];
     assert_equal (Header.get headers "content-type") ["text/html; charset=UTF-8"];
     return ()
  |None -> assert false

let req_parse () =
  let open Cohttp_lwt in
  let open IO in
  let ic = ic_of_buffer (Lwt_bytes.of_string basic_req) in
  Request.read ic >>= function
  |None -> assert false
  |Some req ->
    assert_equal `GET (Request.meth req);
    assert_equal "/index.html" (Request.path req);
    assert_equal `HTTP_1_1 (Request.version req);
    return ()

let post_form_parse () =
  let open Cohttp_lwt in
  let open IO in
  let ic = ic_of_buffer (Lwt_bytes.of_string post_req) in
  Request.read ic >>= function
  |None -> assert false
  |Some req ->
    assert_equal ["Cosby"] (Request.param req "home");
    assert_equal ["flies"] (Request.param req "favorite flavor");
    assert_equal [] (Request.param req "nonexistent");
    (* multiple requests should still work *)
    assert_equal ["Cosby"] (Request.param req "home");
    return ()

let post_data_parse () =
  let open Cohttp_lwt in
  let open IO in
  let ic = ic_of_buffer (Lwt_bytes.of_string post_data_req) in
  Request.read ic >>= function
  |None -> assert false
  |Some req ->
    Request.read_body req ic >>= fun body ->
    assert_equal (Some "home=Cosby&favorite+flavor=flies") body;
    (* A subsequent request for the body will have consumed it, therefore None *)
    Request.read_body req ic >>= fun body ->
    assert_equal None body;
    return ()

let post_chunked_parse () =
  let open Cohttp_lwt in
  let open IO in
  let ic = ic_of_buffer (Lwt_bytes.of_string post_chunked_req) in
  Request.read ic >>= function
  |None -> assert false
  |Some req ->
    assert_equal (Request.transfer_encoding req) "chunked";
    Request.read_body req ic >>= fun chunk ->
    assert_equal chunk (Some "abcdefghijklmnopqrstuvwxyz");
    Request.read_body req ic >>= fun chunk ->
    assert_equal chunk (Some "1234567890abcdef");
    return ()

let res_content_parse () =
  let open Cohttp_lwt in
  let open IO in
  let ic = ic_of_buffer (Lwt_bytes.of_string basic_res_content) in
  Response.read ic >>= function
  |None -> assert false
  |Some res ->
     assert_equal `HTTP_1_1 (Response.version res);
     assert_equal `OK (Response.status res);
     Response.read_body res ic >>= fun body ->
     assert_equal (Some "home=Cosby&favorite+flavor=flies") body;
     return ()

let res_chunked_parse () =
  let open Cohttp_lwt in
  let open IO in
  let ic = ic_of_buffer (Lwt_bytes.of_string chunked_res) in
  Response.read ic >>= function
  |None -> assert false
  |Some res ->
     assert_equal `HTTP_1_1 (Response.version res);
     assert_equal `OK (Response.status res);
     Response.read_body res ic >>= fun chunk ->
     assert_equal chunk (Some "abcdefghijklmnopqrstuvwxyz");
     Response.read_body res ic >>= fun chunk ->
     assert_equal chunk (Some "1234567890abcdef");
     return ()

(* Extract the substring of the byte buffer that has been written to *)
let get_substring oc buf =
  let len = Int64.to_int (Lwt_io.position oc) in
  let b = String.create len in
  Lwt_bytes.blit_bytes_string buf 0 b 0 len;
  b
 
let make_simple_req () =
  let open Cohttp_lwt in
  let open IO in
  let expected = "GET /foo/bar HTTP/1.1\r\nfoo: bar\r\nhost: localhost\r\ntransfer-encoding: chunked\r\n\r\n6\r\nfoobar\r\n0\r\n\r\n" in
  (* Use the low-level write_header/footer API *)
  let buf = Lwt_bytes.create 4096 in
  let oc = oc_of_buffer buf in
  let req = Request.make (Header.of_list [("foo","bar")]) (Uri.of_string "/foo/bar") in
  Request.write_header req oc >>= fun () ->
  Request.write_body "foobar" req oc >>= fun () ->
  Request.write_footer req oc >>= fun () ->
  assert_equal expected (get_substring oc buf);
  (* Use the high-level write API. This also tests that req is immutable
   * by re-using it *)
  let buf = Lwt_bytes.create 4096 in
  let oc = oc_of_buffer buf in
  Request.write (Request.write_body "foobar") req oc >>= fun () ->
  assert_equal expected (get_substring oc buf);
  return ()

let make_simple_res () =
  let open Cohttp_lwt in
  let open IO in
  let expected = "HTTP/1.1 200 OK\r\nfoo: bar\r\ntransfer-encoding: chunked\r\n\r\n6\r\nfoobar\r\n0\r\n\r\n" in
  (* Use the low-level write_header/footer API *)
  let buf = Lwt_bytes.create 4096 in
  let oc = oc_of_buffer buf in
  let res = Response.make (Header.of_list [("foo","bar")]) in
  Response.write_header res oc >>= fun () ->
  Response.write_body "foobar" res oc >>= fun () ->
  Response.write_footer res oc >>= fun () ->
  assert_equal expected (get_substring oc buf);
  (* Use the high-level write API. This also tests that req is immutable
   * by re-using it *)
  let buf = Lwt_bytes.create 4096 in
  let oc = oc_of_buffer buf in
  Response.write (Response.write_body "foobar") res oc >>= fun () ->
  assert_equal expected (get_substring oc buf);
  return ()

let test_cases =
  let tests = [ basic_req_parse; req_parse; post_form_parse; post_data_parse; 
    post_chunked_parse; (basic_res_parse basic_res); (basic_res_parse basic_res_plus_crlf);
    res_content_parse; make_simple_req; make_simple_res;
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
