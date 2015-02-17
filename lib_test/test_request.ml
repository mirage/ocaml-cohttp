open OUnit
open Printf
open Cohttp

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

let _ =
  ("Request" >:::
   [ "Test header has auth" >:: header_has_auth
   ; "Test uri has user info" >:: uri_has_userinfo
   ; "Auth from Uri - do not override" >:: auth_uri_no_override
   ; "Auth from Uri" >:: auth_uri
   ]) |> run_test_tt_main
