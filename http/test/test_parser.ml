open Base
module Parser = Http.Private.Parser

let req =
  "GET /wp-content/uploads/2010/03/hello-kitty-darth-vader-pink.jpg HTTP/1.1\r\n\
   Host: www.kittyhell.com   \r\n\
   User-Agent: Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6; ja-JP-mac; \
   rv:1.9.2.3) Gecko/20100401 Firefox/3.6.3 Pathtraq/0.9\r\n\
   Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n\
   Accept-Language: ja,en-us;q=0.7,en;q=0.3\r\n\
   Accept-Encoding: gzip,deflate\r\n\
   Accept-Charset: Shift_JIS,utf-8;q=0.7,*;q=0.7\r\n\
   Keep-Alive: 115\r\n\
   Connection: keep-alive\r\n\
   Cookie: wp_ozh_wsa_visits=2; wp_ozh_wsa_visit_lasttime=xxxxxxxxxx; \
   __utma=xxxxxxxxx.xxxxxxxxxx.xxxxxxxxxx.xxxxxxxxxx.xxxxxxxxxx.x; \
   __utmz=xxxxxxxxx.xxxxxxxxxx.x.x.utmccn=(referral)|utmcsr=reader.livedoor.com|utmcct=/reader/|utmcmd=referral\r\n\
   Empty:    \r\n\
   \r\n"

let assert_req_success ~here ~expected_req ~expected_consumed ?pos ?len buf =
  let buf = String.subo ?pos ?len buf in
  let req, consumed =
    match Parser.parse_request buf with
    | Error Parser.Partial -> failwith "Unexpected partial parse"
    | Error (Parser.Msg msg) -> failwith msg
    | Ok res -> res
  in
  [%test_result: string] ~here ~message:"HTTP Method mismatch"
    ~expect:(Http.Method.to_string @@ Http.Request.meth expected_req)
    (Http.Method.to_string @@ Http.Request.meth req);
  [%test_result: string] ~here ~message:"path mismatch"
    ~expect:(Http.Request.resource expected_req)
    (Http.Request.resource req);
  [%test_result: (string * string) list] ~here ~message:"header mismatch"
    ~expect:(Http.Header.to_list @@ Http.Request.headers expected_req)
    (Http.Header.to_list @@ Http.Request.headers req);
  [%test_result: int] ~here ~expect:expected_consumed consumed

let[@warning "-3"] make_req ~headers meth resource =
  { Http.Request.headers; meth; resource; version = `HTTP_1_1 }

let req_expected =
  make_req
    ~headers:
      (Http.Header.of_list
         [
           ("Host", "www.kittyhell.com");
           ( "User-Agent",
             "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6; ja-JP-mac; \
              rv:1.9.2.3) Gecko/20100401 Firefox/3.6.3 Pathtraq/0.9" );
           ( "Accept",
             "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
           );
           ("Accept-Language", "ja,en-us;q=0.7,en;q=0.3");
           ("Accept-Encoding", "gzip,deflate");
           ("Accept-Charset", "Shift_JIS,utf-8;q=0.7,*;q=0.7");
           ("Keep-Alive", "115");
           ("Connection", "keep-alive");
           ( "Cookie",
             "wp_ozh_wsa_visits=2; wp_ozh_wsa_visit_lasttime=xxxxxxxxxx; \
              __utma=xxxxxxxxx.xxxxxxxxxx.xxxxxxxxxx.xxxxxxxxxx.xxxxxxxxxx.x; \
              __utmz=xxxxxxxxx.xxxxxxxxxx.x.x.utmccn=(referral)|utmcsr=reader.livedoor.com|utmcct=/reader/|utmcmd=referral"
           );
           ("Empty", "");
         ])
    `GET "/wp-content/uploads/2010/03/hello-kitty-darth-vader-pink.jpg"

let parse_single_request () =
  assert_req_success
    ~here:[ [%here] ]
    ~expected_req:req_expected ~expected_consumed:718 req

let reject_headers_with_space_before_colon () =
  let req =
    "GET / HTTP/1.1\r\nHost : www.kittyhell.com\r\nKeep-Alive: 115\r\n\r\n"
  in
  match Parser.parse_request req with
  | Error (Parser.Msg msg) ->
      [%test_result: string] ~expect:"Invalid Header Key" msg
  | _ -> assert false

let more_requests =
  "GET / HTTP/1.1\r\n\
   Host: www.reddit.com\r\n\
   User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.8; rv:15.0) \r\n\
  \   Gecko/20100101 Firefox/15.0.1\r\n\
   Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n\
   Accept-Language: en-us,en;q=0.5\r\n\
   Accept-Encoding: gzip, deflate\r\n\
   Connection: keep-alive\r\n\
   \r\n\
   GET /reddit.v_EZwRzV-Ns.css HTTP/1.1\r\n\
   Host: www.redditstatic.com\r\n\
   User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.8; rv:15.0) \
   Gecko/20100101 Firefox/15.0.1\r\n\
   Accept: text/css,*/*;q=0.1\r\n\
   Accept-Language: en-us,en;q=0.5\r\n\
   Accept-Encoding: gzip, deflate\r\n\
   Connection: keep-alive\r\n\
   Referer: http://www.reddit.com/\r\n\
   \r\n"

let parse_at_offset () =
  let expected_req =
    make_req
      ~headers:
        (Http.Header.of_list
           [
             ("Host", "www.redditstatic.com");
             ( "User-Agent",
               "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.8; rv:15.0) \
                Gecko/20100101 Firefox/15.0.1" );
             ("Accept", "text/css,*/*;q=0.1");
             ("Accept-Language", "en-us,en;q=0.5");
             ("Accept-Encoding", "gzip, deflate");
             ("Connection", "keep-alive");
             ("Referer", "http://www.reddit.com/");
           ])
      `GET "/reddit.v_EZwRzV-Ns.css"
  in
  assert_req_success
    ~here:[ [%here] ]
    ~expected_req ~expected_consumed:315 ~pos:304 more_requests

let report_partial_parse () =
  let buf = req in
  let err =
    match Parser.parse_request ~pos:0 ~len:50 buf with
    | Error Parser.Partial -> Some "Partial"
    | Error (Parser.Msg msg) -> Some msg
    | Ok _ -> None
  in
  [%test_result: string option] ~expect:(Some "Partial") err

let validate_http_version () =
  let req =
    "GET / HTTP/1.4\r\nHost: www.kittyhell.com\r\nKeep-Alive: 115\r\n\r\n"
  in
  let buf = req in
  let err =
    match Parser.parse_request buf with
    | Error (Parser.Msg msg) -> msg
    | Error Parser.Partial -> failwith "Unexpected partial"
    | Ok _ -> assert false
  in
  [%test_result: String.Caseless.t] ~expect:"Invalid http version" err

let parse_result_notifies_start_of_body () =
  let buf =
    "POST / HTTP/1.1\r\n\
     Host: localhost:8080\r\n\
     User-Agent: curl/7.64.1\r\n\
     Accept: */*\r\n\
     Content-Length: 6\r\n\
     Content-Type: application/x-www-form-urlencoded\r\n\
     \r\n\
     foobar"
  in
  let v = Parser.parse_request buf |> Result.ok in
  let _req, count = Option.value_exn v in
  [%test_result: string] ~expect:"foobar"
    (String.sub buf ~pos:count ~len:(String.length buf - count))

let parse_proxy_get () =
  let buf =
    "GET http://example.com/foo.html HTTP/1.1\r\n\
     Host: example.com\r\n\
     Proxy-Authorization: Basic dXNlcjpwYXNz\r\n\
     \r\n\
     foobar"
  in
  let expected_req =
    make_req
      ~headers:
        (Http.Header.of_list
           [
             ("Host", "example.com");
             ("Proxy-Authorization", "Basic dXNlcjpwYXNz");
           ])
      `GET "http://example.com/foo.html"
  in
  assert_req_success ~here:[ [%here] ] ~expected_req ~expected_consumed:104 buf

open Base_quickcheck

let parse_chunk_length () =
  Test.run_exn
    (module struct
      type t = int64 [@@deriving quickcheck, sexp_of]
    end)
    ~f:(fun num ->
      let payload =
        let s = Printf.sprintf "%Lx\r\n" num in
        s
      in
      match Parser.parse_chunk_length payload with
      | Ok res ->
          [%test_eq: int64 * int] res
            (num, String.length (Printf.sprintf "%Lx" num) + 2)
      | Error (Parser.Msg _) -> ()
      | Error _ -> assert false)

let chunk_length_parse_case_insensitive () =
  let run_test num str =
    let buf = str in
    match Parser.parse_chunk_length buf with
    | Ok res ->
        [%test_eq: int64 * int] res
          (num, String.length (Printf.sprintf "%Lx" num) + 2)
    | Error (Parser.Msg _) -> ()
    | Error _ -> assert false
  in
  Test.run_exn
    (module struct
      type t = int64 [@@deriving quickcheck, sexp_of]
    end)
    ~f:(fun num ->
      let payload = Printf.sprintf "%Lx\r\n" num in
      run_test num (String.uppercase payload);
      run_test num (String.lowercase payload))

type parse_res = [ `Ok of int64 * int | `Msg of string | `Partial ]
[@@deriving sexp, compare]

let parse_chunk_lengths () =
  let run_parser buf =
    match Parser.parse_chunk_length buf with
    | Ok res -> `Ok res
    | Error Parser.Partial -> `Partial
    | Error (Parser.Msg msg) -> `Msg msg
  in
  [%test_result: parse_res] ~expect:(`Ok (2738L, 5)) (run_parser "ab2\r\n");
  [%test_result: parse_res]
    ~expect:(`Ok (4526507L, 8))
    (run_parser "4511ab\r\n");
  (* We will try to use the same chunk length, but this time with a chunk extension. This
     should not result in any change in our output. *)
  [%test_result: parse_res]
    ~expect:(`Ok (4526507L, 13))
    (run_parser "4511ab  ; a\r\n");
  [%test_result: parse_res]
    ~expect:(`Ok (4526507L, 26))
    (run_parser "4511ab; now in extension\r\n");
  [%test_result: parse_res] ~expect:(`Msg "Invalid chunk_length character 'a'")
    (run_parser "4511ab a ; now in extension\r\n");
  [%test_result: parse_res]
    ~expect:(`Ok (76861433640456465L, 17))
    (run_parser "111111111111111\r\n");
  [%test_result: parse_res] ~expect:(`Msg "Chunk size is too large")
    (run_parser "1111111111111111\r\n");
  [%test_result: parse_res] ~expect:(`Msg "Expected_newline")
    (run_parser "abc\r12");
  [%test_result: parse_res]
    ~expect:(`Msg "Invalid chunk_length character '\\n'") (run_parser "abc\n12");
  [%test_result: parse_res] ~expect:`Partial (run_parser "121");
  [%test_result: parse_res] ~expect:`Partial (run_parser "121\r")

let () =
  let open Alcotest in
  run "Parser"
    [
      ( "parse request",
        [
          test_case "single request" `Quick parse_single_request;
          test_case "parse at offset" `Quick parse_at_offset;
          test_case "reject headers with invalid character in key" `Quick
            reject_headers_with_space_before_colon;
          test_case "report partial parse" `Quick report_partial_parse;
          test_case "validate http version" `Quick validate_http_version;
          test_case "parse result notified offset of start of optional body"
            `Quick parse_result_notifies_start_of_body;
          test_case "parse a proxy GET request" `Quick parse_proxy_get;
        ] );
      ( "chunked encoding",
        [
          test_case "can parse chunk length" `Quick parse_chunk_length;
          test_case "chunk length parsing is case insensitive" `Quick
            chunk_length_parse_case_insensitive;
          test_case "parse chunk lengths" `Quick parse_chunk_lengths;
        ] );
    ]
