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

module StringResponse = Cohttp.Response.Make(Cohttp.String_io.M)
module HIO = Cohttp.Header_io.Make(Cohttp.String_io.M)
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

let list_valued_header () =
  let h = H.init () in
  let h = H.add h "accept" "foo" in
  let h = H.add h "accept" "bar" in
  assert_equal
    ~printer:(function
      | None -> "None"
      | Some x -> x) (H.get h "accept") (Some "bar,foo")

let large_header () =
  let sz = 1024 * 1024 * 100 in
  let h = H.init () in
  let v1 = String.make sz 'a' in
  let h = H.add h "x-large" v1 in
  let h = H.add h v1 "foo" in
  assert_equal
    ~printer:(function
      | None -> "None"
      | Some x -> x) (H.get h "x-large") (Some v1);
  let obuf = Buffer.create (sz + 1024) in
  HIO.write h obuf;
  let ibuf = Buffer.contents obuf in
  let sbuf = Cohttp.String_io.open_in ibuf in
  let header_printer h =
    Printf.sprintf "[length %d]%s"
     (List.length (H.to_list h)) 
     (String.concat "\n" (List.map (fun (k,v) -> Printf.sprintf "%s: %s" k v) (H.to_list h))) in 
  assert_equal ~cmp:(fun a b -> H.compare a b = 0) ~printer:header_printer (HIO.parse sbuf) h

let many_headers () =
  let size = 1000000 in
  let rec add_header num h =
    match num with 
    | 0 -> h
    | n ->
       let k = sprintf "h%d" n in
       let v = sprintf "v%d" n in
       let h = H.add h k v in
       add_header (num - 1) h
  in
  let h = add_header size (H.init ()) in
  assert_equal ~printer:string_of_int
    (List.length (H.to_list h)) size
    
module Content_range = struct
  let h1 = H.of_list [("Content-Length", "123")]
  let h2 = H.of_list [("Content-Range", "bytes 200-300/1000")]
  let none () = assert_equal None (H.init () |> H.get_content_range)
  let content_length () = assert_equal (Some 123L) (H.get_content_range h1)
  let content_range () = assert_equal (Some 101L) (H.get_content_range h2)
end

module Link = Cohttp.Link

let links_printer link_list =
  String.concat "\n" (List.map Link.to_string link_list)

let headers_of_response test_name response_string =
  Cohttp.String_io.M.(
    StringResponse.read (Cohttp.String_io.open_in response_string)
    >>= function
    | `Ok resp -> Cohttp.Response.headers resp
    | _ -> assert_failure (test_name ^ " response parse failed")
  )

let get_resp lines =
  "HTTP/1.1 200 OK\r\n"^(String.concat "\r\n" lines)^"\r\n\r\n"

let empty_uri = Uri.of_string ""

let link_simple () =
  let next_tgt = "/page/2" in
  let resp = get_resp ["Link: <"^next_tgt^">; rel=next"] in
  let headers = headers_of_response "link_simple" resp in
  let printer = links_printer in
  assert_equal ~printer Link.([{
    context = empty_uri;
    arc = Arc.({ empty with relation=Rel.([next]) });
    target = Uri.of_string next_tgt;
  }]) (H.get_links headers)

let link_multi_rel () =
  let next_tgt = "/page/2" in
  let resp = get_resp ["Link: <"^next_tgt^">; rel=\"next last\""] in
  let headers = headers_of_response "link_multi_rel" resp in
  let printer = links_printer in
  assert_equal ~printer Link.([{
    context = empty_uri;
    arc = Arc.({ empty with relation=Rel.([next; last]) });
    target = Uri.of_string next_tgt;
  }]) (H.get_links headers)

let link_multi_line () =
  let self_tgt = "/page/1" in
  let next_tgt = "/page/2" in
  let resp = get_resp [
    "Link: <"^next_tgt^">; rel=\"next\"";
    "Link: <"^self_tgt^">; rel=self";
  ] in
  let headers = headers_of_response "link_multi_line" resp in
  let printer = links_printer in
  assert_equal ~printer Link.([
    {
      context = empty_uri;
      arc = Arc.({ empty with relation=Rel.([next]) });
      target = Uri.of_string next_tgt;
    };
    {
      context = empty_uri;
      arc = Arc.({ empty with relation=Rel.([self]) });
      target = Uri.of_string self_tgt;
    };
  ]) (H.get_links headers)

let link_multi_multi () =
  let next_tgt = "/page/2" in
  let last_tgt = "/page/3" in
  let resp = get_resp [
    "Link: <"^next_tgt^">; rel=\"next\", <"^last_tgt^">; rel=last";
  ] in
  let headers = headers_of_response "link_multi_multi" resp in
  let printer = links_printer in
  assert_equal ~printer Link.([
    {
      context = empty_uri;
      arc = Arc.({ empty with relation=Rel.([next]) });
      target = Uri.of_string next_tgt;
    };
    {
      context = empty_uri;
      arc = Arc.({ empty with relation=Rel.([last]) });
      target = Uri.of_string last_tgt;
    };
  ]) (H.get_links headers)

let link_rel_uri () =
  let uri_tgt = "/page/2" in
  let uri_s = "http://example.com/a,valid;uri" in
  let resp = get_resp [
    "Link: <"^uri_tgt^">; rel=\"next "^uri_s^"\"; hreflang=en";
  ] in
  let headers = headers_of_response "link_rel_uri" resp in
  let printer = links_printer in
  assert_equal ~printer Link.([
    {
      context = empty_uri;
      arc = Arc.({ empty with
                   relation = Rel.([
                     next;
                     extension (Uri.of_string uri_s);
                   ]);
                   hreflang = Some "en";
                 });
      target = Uri.of_string uri_tgt;
    };
  ]) (H.get_links headers)

let link_anchor () =
  let anchor = "/page/2" in
  let target = "/page/1" in
  let resp = get_resp [
    "Link: <"^target^">; anchor=\""^anchor^"\"; rel=prev";
  ] in
  let headers = headers_of_response "link_rel_uri" resp in
  let printer = links_printer in
  assert_equal ~printer Link.([
    {
      context = Uri.of_string anchor;
      arc = Arc.({ empty with
                   relation = Rel.([
                     prev;
                   ]);
                 });
      target = Uri.of_string target;
    };
  ]) (H.get_links headers)

let link_rev () =
  let anchor = "/page/2" in
  let resp = get_resp [
    "Link: <"^anchor^">; rev=prev";
  ] in
  let headers = headers_of_response "link_rev" resp in
  let printer = links_printer in
  assert_equal ~printer Link.([
    {
      context = Uri.of_string anchor;
      arc = Arc.({ empty with
                   reverse = true;
                   relation = Rel.([
                     prev;
                   ]);
                 });
      target = empty_uri;
    };
  ]) (H.get_links headers)

let link_media () =
  let target = "/page/2" in
  let resp = get_resp [
    "Link: <"^target^">; media=screen";
  ] in
  let headers = headers_of_response "link_media" resp in
  let printer = links_printer in
  assert_equal ~printer Link.([
    {
      context = empty_uri;
      arc = Arc.({ empty with
                   media = Some "screen";
                 });
      target = Uri.of_string target;
    };
  ]) (H.get_links headers)

let link_media_complex () =
  let target = "/page/2" in
  let resp = get_resp [
    "Link: <"^target^">; media=\"screen, print and dpi < 200\"";
  ] in
  let headers = headers_of_response "link_media_complex" resp in
  let printer = links_printer in
  assert_equal ~printer Link.([
    {
      context = empty_uri;
      arc = Arc.({ empty with
                   media = Some "screen, print and dpi < 200";
                 });
      target = Uri.of_string target;
    };
  ]) (H.get_links headers)

let link_title () =
  let target = "/page/2" in
  let resp = get_resp [
    "Link: <"^target^">; title=\"Next!\"; rel=next";
  ] in
  let headers = headers_of_response "link_title" resp in
  let printer = links_printer in
  assert_equal ~printer Link.([
    {
      context = empty_uri;
      arc = Arc.({ empty with
                   relation = Rel.([ next ]);
                   title = Some "Next!";
                 });
      target = Uri.of_string target;
    };
  ]) (H.get_links headers)

let link_title_star () =
  let target = "/page/2" in
  let resp = get_resp [
    "Link: <"^target^">; title*=UTF-8'en'Next!; rel=next";
  ] in
  let headers = headers_of_response "link_title_star" resp in
  let printer = links_printer in
  assert_equal ~printer Link.([
    {
      context = empty_uri;
      arc = Arc.({ empty with
                   relation = Rel.([ next ]);
                   title_ext = Some
                       (Ext.make
                          ~charset:(Charset.of_string "UTF-8")
                          ~language:(Language.of_string "en")
                          "Next!");
                 });
      target = Uri.of_string target;
    };
  ]) (H.get_links headers)

let link_type_token () =
  let target = "/page/2" in
  let resp = get_resp [
    "Link: <"^target^">; type=text/html; rel=next";
  ] in
  let headers = headers_of_response "link_type_token" resp in
  let printer = links_printer in
  assert_equal ~printer Link.([
    {
      context = empty_uri;
      arc = Arc.({ empty with
                   relation = Rel.([ next ]);
                   media_type = Some ("text", "html");
                 });
      target = Uri.of_string target;
    };
  ]) (H.get_links headers)

let link_type_quoted () =
  let target = "/page/2" in
  let resp = get_resp [
    "Link: <"^target^">; type=\"text/html\"; rel=next";
  ] in
  let headers = headers_of_response "link_type_quoted" resp in
  let printer = links_printer in
  assert_equal ~printer Link.([
    {
      context = empty_uri;
      arc = Arc.({ empty with
                   relation = Rel.([ next ]);
                   media_type = Some ("text", "html");
                 });
      target = Uri.of_string target;
    };
  ]) (H.get_links headers)

let link_ext () =
  let target = "/page/2" in
  let resp = get_resp [
    "Link: <"^target^">; see=saw; rel=next";
  ] in
  let headers = headers_of_response "link_ext" resp in
  let printer = links_printer in
  assert_equal ~printer Link.([
    {
      context = empty_uri;
      arc = Arc.({ empty with
                   relation = Rel.([ next ]);
                   extensions = ["see", "saw"];
                 });
      target = Uri.of_string target;
    };
  ]) (H.get_links headers)

let link_ext_star () =
  let target = "/page/2" in
  let resp = get_resp [
    "Link: <"^target^">; zig*=''zag; rel=next";
  ] in
  let headers = headers_of_response "link_ext" resp in
  let printer = links_printer in
  assert_equal ~printer Link.([
    {
      context = empty_uri;
      arc = Arc.({ empty with
                   relation = Rel.([ next ]);
                   extension_exts = ["zig",
                                     Ext.make
                                       ~charset:(Charset.of_string "")
                                       ~language:(Language.of_string "")
                                       "zag"
                                    ];
                 });
      target = Uri.of_string target;
    };
  ]) (H.get_links headers)

let trim_ws () =
  let resp = get_resp ["Age: 281   "] in
  let headers = headers_of_response "trim whitespace" resp in
  assert_equal
    ~printer:(function
      | None -> "None"
      | Some x -> "\"" ^ x ^ "\"") (H.get headers "age") (Some "281")

let test_cachecontrol_concat () =
  let resp = get_resp ["Cache-Control: public";
                       "Cache-Control: max-age:86400"] in
  let  h = headers_of_response "concat Cache-Control" resp in
  assert_equal
    ~printer:(function
      | None -> "None"
      | Some x -> x) (Some "public,max-age:86400") (H.get h "Cache-Control")

;;
Printexc.record_backtrace true;
Alcotest.run "test_header" [
  "Link", [
    "simple", `Quick, link_simple;
    "multiple rels", `Quick, link_multi_rel;
    "multiple lines", `Quick, link_multi_line;
    "multiheader", `Quick, link_multi_multi;
    "rel uri", `Quick, link_rel_uri;
    "anchor", `Quick, link_anchor;
    "rev", `Quick, link_rev;
    "media", `Quick, link_media;
    "media complex", `Quick, link_media_complex;
    "title", `Quick, link_title;
    "title star", `Quick, link_title_star;
    "type token", `Quick, link_type_token;
    "type quoted", `Quick, link_type_quoted;
    "extension", `Quick, link_ext;
    "extension star", `Quick, link_ext_star;
  ];
  "Media Type", [
    "Media Type", `Quick, get_media_type;
  ];
  "Auth", [
    "Valid Auth", `Quick, valid_auth;
  ];
  "Cookie", [
    "Valid Set-Cookie", `Quick, valid_set_cookie;
    "Valid Cookie", `Quick, valid_cookie;
    "Cookie with =", `Quick, cookie_with_eq_val;
  ];
  "Content Range", [
    "none", `Quick, Content_range.none;
    "content-length", `Quick, Content_range.content_length;
    "content-range", `Quick, Content_range.content_range;
  ];
  "Cache Control", [
    "concat", `Quick, test_cachecontrol_concat
  ];
  "Header", [
    "get list valued", `Quick, list_valued_header;
    "trim whitespace", `Quick, trim_ws;
    "large header", `Slow, large_header;
    "many headers", `Slow, many_headers;
  ];
]
