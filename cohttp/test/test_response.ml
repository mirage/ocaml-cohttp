open Cohttp
module String_io = Cohttp.Private.String_io
module Response_io = Response.Private.Make (String_io.M)

let t_encoding =
  Alcotest.testable
    (fun fmt e -> Format.pp_print_string fmt (Transfer.string_of_encoding e))
    ( = )

let check_encoding name expected response =
  Alcotest.check t_encoding name expected (Response.encoding response)

let check_no_framing_headers name response =
  let headers = Response.headers response in
  Alcotest.(check (option string))
    (name ^ ": no transfer-encoding header")
    None
    (Header.get headers "transfer-encoding");
  Alcotest.(check (option string))
    (name ^ ": no content-length header")
    None
    (Header.get headers "content-length")

let bodyless_statuses =
  [
    `Continue;
    `Switching_protocols;
    `Processing;
    `Checkpoint;
    `No_content;
    `Not_modified;
  ]

let iter_bodyless f =
  List.iter
    (fun status -> f (Code.string_of_status status) status)
    bodyless_statuses

let default_encoding_is_chunked () =
  check_encoding "unspecified encoding defaults to chunked" Transfer.Chunked
    (Response.make ())

let explicit_encoding_is_used () =
  check_encoding "explicit encoding is used" (Transfer.Fixed 5L)
    (Response.make ~status:`OK ~encoding:(Transfer.Fixed 5L) ())

let encoding_from_headers () =
  check_encoding "encoding taken from supplied headers" (Transfer.Fixed 42L)
    (Response.make ~status:`OK
       ~headers:(Header.of_list [ ("content-length", "42") ])
       ())

let bodyless_has_no_encoding () =
  iter_bodyless (fun name status ->
      let r = Response.make ~status () in
      check_encoding (name ^ " is not chunked by default") Transfer.Unknown r;
      check_no_framing_headers name r)

let bodyless_ignores_explicit_encoding () =
  iter_bodyless (fun name status ->
      let chunked = Response.make ~status ~encoding:Transfer.Chunked () in
      check_encoding
        (name ^ " drops an explicit chunked encoding")
        Transfer.Unknown chunked;
      check_no_framing_headers (name ^ " (chunked)") chunked;
      let fixed = Response.make ~status ~encoding:(Transfer.Fixed 0L) () in
      check_encoding
        (name ^ " drops an explicit fixed encoding")
        Transfer.Unknown fixed;
      check_no_framing_headers (name ^ " (fixed)") fixed)

let t_has_body =
  Alcotest.testable
    (fun fmt -> function
      | `No -> Format.pp_print_string fmt "`No"
      | `Unknown -> Format.pp_print_string fmt "`Unknown"
      | `Yes -> Format.pp_print_string fmt "`Yes")
    ( = )

let bodyless_has_no_body () =
  iter_bodyless (fun name status ->
      Alcotest.check t_has_body (name ^ " has no body") `No
        (Response.has_body (Response.make ~status ())))

let write_response r =
  let buf = Buffer.create 128 in
  Response_io.write ~flush:false (fun _ -> ()) r buf;
  Buffer.contents buf

let write_chunked_response () =
  Alcotest.(check string)
    "a 200 response is framed as chunked"
    "HTTP/1.1 200 OK\r\ntransfer-encoding: chunked\r\n\r\n0\r\n\r\n"
    (write_response (Response.make ()))

let write_switching_protocols () =
  let r =
    Response.make ~status:`Switching_protocols
      ~headers:
        (Header.of_list [ ("upgrade", "websocket"); ("connection", "Upgrade") ])
      ()
  in
  Alcotest.(check string)
    "a 101 response is written without any framing"
    "HTTP/1.1 101 Switching Protocols\r\n\
     upgrade: websocket\r\n\
     connection: Upgrade\r\n\
     \r\n"
    (write_response r)

let write_no_content () =
  Alcotest.(check string)
    "a 204 response is written without any framing"
    "HTTP/1.1 204 No Content\r\n\r\n"
    (write_response (Response.make ~status:`No_content ()))

let write_not_modified () =
  Alcotest.(check string)
    "a 304 response is written without any framing"
    "HTTP/1.1 304 Not Modified\r\n\r\n"
    (write_response (Response.make ~status:`Not_modified ()))

let () =
  Alcotest.run "test_response"
    [
      ( "Encoding",
        [
          ("default is chunked", `Quick, default_encoding_is_chunked);
          ("explicit encoding", `Quick, explicit_encoding_is_used);
          ("from headers", `Quick, encoding_from_headers);
        ] );
      ( "Bodyless statuses",
        [
          ("no encoding by default", `Quick, bodyless_has_no_encoding);
          ( "explicit encoding ignored",
            `Quick,
            bodyless_ignores_explicit_encoding );
          ("no body", `Quick, bodyless_has_no_body);
        ] );
      ( "Serialization",
        [
          ("chunked", `Quick, write_chunked_response);
          ("101 Switching Protocols", `Quick, write_switching_protocols);
          ("204 No Content", `Quick, write_no_content);
          ("304 Not Modified", `Quick, write_not_modified);
        ] );
    ]
