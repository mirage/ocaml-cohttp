open OUnit
open Printf
open Cohttp

module StringRequest = Request.Make(String_io.M)

let uri_userinfo = Uri.of_string "http://foo:bar@ocaml.org"

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
    (Some (`Basic ("foo", "bar")))

let parse_request_uri_ r uri name =
  String_io.M.(
    StringRequest.read (String_io.open_in r)
    >>= function
    | `Ok pr ->
      assert_equal pr.Request.uri uri
    | _ -> assert_failure (name^" parse failed")
  )

let parse_request_uri _ =
  let r = "GET / HTTP/1.1\r\n\r\n" in
  let uri = Uri.of_string "/" in
  parse_request_uri_ r uri "parse_request_uri"

let parse_request_uri_double_slash _ =
  let r = "GET // HTTP/1.1\r\n\r\n" in
  let uri = Uri.with_path (Uri.of_string "") "//" in
  parse_request_uri_ r uri "parse_request_uri_double_slash"

let parse_request_uri_triple_slash _ =
  let r = "GET /// HTTP/1.1\r\n\r\n" in
  let uri = Uri.with_path (Uri.of_string "") "///" in
  parse_request_uri_ r uri "parse_request_uri_triple_slash"

let parse_request_uri_host _ =
  let r = "GET / HTTP/1.1\r\nHost: example.com\r\n\r\n" in
  let uri = Uri.of_string "//example.com/" in
  parse_request_uri_ r uri "parse_request_uri_host"

let parse_request_uri_host_double_slash _ =
  let r = "GET // HTTP/1.1\r\nHost: example.com\r\n\r\n" in
  let uri = Uri.of_string "//example.com//" in
  parse_request_uri_ r uri "parse_request_uri_host_double_slash"

let parse_request_uri_host_triple_slash _ =
  let r = "GET /// HTTP/1.1\r\nHost: example.com\r\n\r\n" in
  let uri = Uri.of_string "//example.com///" in
  parse_request_uri_ r uri "parse_request_uri_host_triple_slash"

let _ =
  ("Request" >:::
   [ "Test header has auth" >:: header_has_auth
   ; "Test uri has user info" >:: uri_has_userinfo
   ; "Auth from Uri - do not override" >:: auth_uri_no_override
   ; "Auth from Uri" >:: auth_uri
   ; "Parse simple request URI" >:: parse_request_uri
   ; "Parse request URI double slash" >:: parse_request_uri_double_slash
   ; "Parse request URI triple slash" >:: parse_request_uri_triple_slash
   ; "Parse request URI with host" >:: parse_request_uri_host
   ; "Parse request URI double slash with host"
     >:: parse_request_uri_host_double_slash
   ; "Parse request URI triple slash with host"
     >:: parse_request_uri_host_triple_slash
   ]) |> run_test_tt_main
