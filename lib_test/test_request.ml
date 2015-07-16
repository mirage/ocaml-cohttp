open OUnit
open Printf
open Cohttp

module StringRequest = Request.Make(String_io.M)

let uri_userinfo = Uri.of_string "http://foo:bar%2525@ocaml.org"

let header_auth =
  let h = Header.init () in
  let h = Header.add_authorization h (`Basic ("qux", "qwerty")) in
  h

let is_some = function
  | None -> false
  | Some _ -> true

let header_has_auth _ =
  assert_bool "Test header has auth"
    (header_auth |> Header.get_authorization |> is_some)

let uri_has_userinfo _ =
  assert_bool "Uri has user info" (uri_userinfo |> Uri.userinfo |> is_some)

let auth_uri_no_override _ =
  let r = Request.make ~headers:header_auth uri_userinfo in
  assert_equal
    (r |> Request.headers |> Header.get_authorization )
    (Header.get_authorization header_auth)

let auth_uri _ =
  let r = Request.make uri_userinfo in
  assert_equal
    (r |> Request.headers |> Header.get_authorization)
    (Some (`Basic ("foo", "bar%25")))

let opt_default default = function
  | None -> default
  | Some v -> v

let parse_request_uri_ r expected name =
  String_io.M.(
    StringRequest.read (String_io.open_in r)
    >>= fun result -> match result, expected with
    | `Ok req, `Ok uri ->
      let ruri = Request.uri req in
      let msg = Uri.(Printf.sprintf "expected %s %d %s %s\ngot %s %d %s %s"
                       (opt_default "_" (host uri))
                       (opt_default (-1) (port uri))
                       (path uri) (encoded_of_query (query uri))
                       (opt_default "_" (host ruri))
                       (opt_default (-1) (port ruri))
                       (path ruri) (encoded_of_query (query ruri))
                    ) in
      assert_equal ~printer:Uri.to_string ~cmp:Uri.equal ~msg uri ruri
    | `Invalid rmsg, `Invalid msg ->
      assert_equal ~printer:(fun x -> x) rmsg msg
    | `Invalid error, `Ok uri ->
      assert_failure (
        Printf.sprintf "Expected uri:'%s'. Received message: '%s'"
          (Uri.to_string uri) error
      )
    | `Ok _, `Invalid _ -> assert_failure "Parsed invalid URI"
    | _ -> assert_failure (name ^ " unexpected request parse result")
  )

let bad_request = `Invalid "bad request URI"

let parse_request_uri _ =
  let r = "GET / HTTP/1.1\r\n\r\n" in
  let uri = `Ok (Uri.of_string "/") in
  parse_request_uri_ r uri "parse_request_uri"

let parse_request_uri_host _ =
  let r = "GET / HTTP/1.1\r\nHost: example.com\r\n\r\n" in
  let uri = `Ok (Uri.of_string "//example.com/") in
  parse_request_uri_ r uri "parse_request_uri_host"

let parse_request_uri_host_port _ =
  let r = "GET / HTTP/1.1\r\nHost: example.com:8080\r\n\r\n" in
  let uri = `Ok (Uri.of_string "//example.com:8080/") in
  parse_request_uri_ r uri "parse_request_uri_host_port"

let parse_request_uri_double_slash _ =
  let r = "GET // HTTP/1.1\r\n\r\n" in
  let uri = `Ok (Uri.with_path (Uri.of_string "") "//") in
  parse_request_uri_ r uri "parse_request_uri_double_slash"

let parse_request_uri_host_double_slash _ =
  let r = "GET // HTTP/1.1\r\nHost: example.com\r\n\r\n" in
  let uri = `Ok (Uri.of_string "//example.com//") in
  parse_request_uri_ r uri "parse_request_uri_host_double_slash"

let parse_request_uri_triple_slash _ =
  let r = "GET /// HTTP/1.1\r\n\r\n" in
  let uri = `Ok (Uri.with_path (Uri.of_string "") "///") in
  parse_request_uri_ r uri "parse_request_uri_triple_slash"

let parse_request_uri_host_triple_slash _ =
  let r = "GET /// HTTP/1.1\r\nHost: example.com\r\n\r\n" in
  let uri = `Ok (Uri.of_string "//example.com///") in
  parse_request_uri_ r uri "parse_request_uri_host_triple_slash"

let parse_request_uri_no_slash _ =
  let r = "GET foo HTTP/1.1\r\n\r\n" in
  parse_request_uri_ r bad_request "parse_request_uri_no_slash"

let parse_request_uri_host_no_slash _ =
  let r = "GET foo HTTP/1.1\r\nHost: example.com\r\n\r\n" in
  parse_request_uri_ r bad_request "parse_request_uri_host_no_slash"

let parse_request_uri_empty _ =
  let r = "GET  HTTP/1.1\r\n\r\n" in
  let uri = `Ok (Uri.of_string "/") in
  parse_request_uri_ r uri "parse_request_uri_empty"

let parse_request_uri_host_empty _ =
  let r = "GET  HTTP/1.1\r\nHost: example.com\r\n\r\n" in
  let uri = `Ok (Uri.of_string "//example.com/") in
  parse_request_uri_ r uri "parse_request_uri_host_empty"

let parse_request_uri_path_like_scheme _ =
  let r = "GET http://example.net HTTP/1.1\r\n\r\n" in
  let uri = `Ok (Uri.of_string "http://example.net/") in
  parse_request_uri_ r uri "parse_request_uri_path_like_scheme"

let parse_request_uri_host_path_like_scheme _ =
  let r = "GET http://example.net HTTP/1.1\r\nHost: example.com\r\n\r\n" in
  let uri = `Ok (Uri.of_string "http://example.net/") in
  parse_request_uri_ r uri "parse_request_uri_host_path_like_scheme"

let parse_request_uri_path_like_host_port _ =
  let path = "//example.net:8080" in
  let r = "GET "^path^" HTTP/1.1\r\n\r\n" in
  let uri = `Ok (Uri.with_path (Uri.of_string "") path) in
  parse_request_uri_ r uri "parse_request_uri_path_like_host_port"

let parse_request_uri_host_path_like_host_port _ =
  let path = "//example.net:8080" in
  let r = "GET "^path^" HTTP/1.1\r\nHost: example.com\r\n\r\n" in
  let uri = `Ok (Uri.with_path (Uri.of_string "//example.com") path) in
  parse_request_uri_ r uri "parse_request_uri_host_path_like_host_port"

let parse_request_uri_query _ =
  let pqs = "/?foo" in
  let r = "GET "^pqs^" HTTP/1.1\r\n\r\n" in
  let uri = `Ok (Uri.of_string pqs) in
  parse_request_uri_ r uri "parse_request_uri_query"

let parse_request_uri_host_query _ =
  let pqs = "/?foo" in
  let r = "GET "^pqs^" HTTP/1.1\r\nHost: example.com\r\n\r\n" in
  let uri = `Ok (Uri.of_string ("//example.com"^pqs)) in
  parse_request_uri_ r uri "parse_request_uri_host_query"

let parse_request_uri_query_no_slash _ =
  let r = "GET ?foo HTTP/1.1\r\n\r\n" in
  parse_request_uri_ r bad_request "parse_request_uri_query_no_slash"

let parse_request_uri_host_query_no_slash _ =
  let r = "GET ?foo HTTP/1.1\r\nHost: example.com\r\n\r\n" in
  parse_request_uri_ r bad_request "parse_request_uri_host_query_no_slash"

let parse_request_connect _ =
  let r = "CONNECT vpn.example.net:443 HTTP/1.1\r\n" in
  let uri = `Ok (Uri.of_string "//vpn.example.net:443") in
  parse_request_uri_ r uri "parse_request_connect"

let parse_request_connect_host _ =
  let r =
    "CONNECT vpn.example.net:443 HTTP/1.1\r\nHost: vpn.example.com:443\r\n\r\n"
  in
  let uri = `Ok (Uri.of_string "//vpn.example.net:443") in
  parse_request_uri_ r uri "parse_request_connect_host"

let parse_request_options _ =
  let r = "OPTIONS * HTTP/1.1\r\n\r\n" in
  let uri = `Ok (Uri.of_string "") in
  parse_request_uri_ r uri "parse_request_options"

let parse_request_options_host _ =
  let r = "OPTIONS * HTTP/1.1\r\nHost: example.com:443\r\n\r\n" in
  let uri = `Ok (Uri.of_string "//example.com:443") in
  parse_request_uri_ r uri "parse_request_options_host"

let parse_request_uri_traversal _ =
  let r = "GET /../../../../etc/shadow HTTP/1.1\r\n\r\n" in
  let uri = `Ok (Uri.of_string "/etc/shadow") in
  parse_request_uri_ r uri "parse_request_uri_traversal"

let parse_request_uri_host_traversal _ =
  let r = "GET /../../../../etc/shadow HTTP/1.1\r\nHost: example.com\r\n\r\n" in
  let uri = `Ok (Uri.of_string "//example.com/etc/shadow") in
  parse_request_uri_ r uri "parse_request_uri_host_traversal"

;;
Printexc.record_backtrace true;
Alcotest.run "test_request" [
  "Auth", [
    "header has auth", `Quick, header_has_auth;
    "URI has user info", `Quick, uri_has_userinfo;
    "from URI - do not override", `Quick, auth_uri_no_override;
    "from URI", `Quick, auth_uri;
  ];
  "Parse URI", [
    "simple", `Quick, parse_request_uri;
    "with host", `Quick, parse_request_uri_host;
    "with host and port", `Quick, parse_request_uri_host_port;
    "double slash", `Quick, parse_request_uri_double_slash;
    "double slash with host", `Quick, parse_request_uri_host_double_slash;
    "triple slash", `Quick, parse_request_uri_triple_slash;
    "triple slash with host", `Quick, parse_request_uri_host_triple_slash;
    "no slash", `Quick, parse_request_uri_no_slash;
    "no slash with host", `Quick, parse_request_uri_host_no_slash;
    "empty", `Quick, parse_request_uri_empty;
    "empty with host", `Quick, parse_request_uri_host_empty;
    "path like scheme", `Quick, parse_request_uri_path_like_scheme;
    "path like scheme with host", `Quick, parse_request_uri_host_path_like_scheme;
    "path like host:port", `Quick, parse_request_uri_path_like_host_port;
    "path like host:port with host",
    `Quick, parse_request_uri_host_path_like_host_port;
    "with query string", `Quick, parse_request_uri_query;
    "with query with host", `Quick, parse_request_uri_host_query;
    "no slash with query string", `Quick, parse_request_uri_query_no_slash;
    "no slash with query with host",
    `Quick, parse_request_uri_host_query_no_slash;
    "CONNECT", `Quick, parse_request_connect;
    "CONNECT with host", `Quick, parse_request_connect_host;
    "OPTIONS", `Quick, parse_request_options;
    "OPTIONS with host", `Quick, parse_request_options_host;
    "parent traversal", `Quick, parse_request_uri_traversal;
    "parent traversal with host", `Quick, parse_request_uri_host_traversal;
  ];
]
