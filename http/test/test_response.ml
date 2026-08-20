open Http

let aeb = Alcotest.check Alcotest.bool
let aeo = Alcotest.check Alcotest.(option int)
let no_content_status = Response.make ~status:`No_content ()
let continue_status = Response.make ~status:`Continue ()
let ok_status = Response.make ~status:`OK ()

let chunked_transport_encoding =
  let headers =
    let headers = Http.Header.init () in
    Http.Header.add headers "Transfer-Encoding" "chunked"
  in
  Response.make ~status:`OK ~headers ()

let requires_content_length_tests =
  let no_content_status () =
    Response.requires_content_length no_content_status
    |> aeb "requires_content_length m = true, where s is `No_content (204)"
         false
  in
  let continue_status () =
    Response.requires_content_length continue_status
    |> aeb "requires_content_length m = true, where s is `Continue (100)" false
  in
  let ok_status () =
    Response.requires_content_length ok_status
    |> aeb "requires_content_length s = true, where s is `OK (200" true
  in
  let chunked_transport_encoding () =
    Response.requires_content_length chunked_transport_encoding
    |> aeb "requires_content_length s = true, where s is `OK (200" false
  in
  ( "requires_content_length",
    [
      ("`No_content", `Quick, no_content_status);
      ("`Continue", `Quick, continue_status);
      ("`OK", `Quick, ok_status);
      ("Transport-Encoding: chunked", `Quick, chunked_transport_encoding);
    ] )

let content_length_tests =
  let ok_status () =
    Response.
      {
        ok_status with
        headers = Header.add ok_status.headers "Content-Length" "20";
      }
    |> Response.content_length
    |> aeo "Some len" (Some 20)
  in
  let no_content_status () =
    Response.content_length no_content_status |> aeo "`No_content : None" None
  in

  ( "content_length",
    [ ("OK", `Quick, ok_status); ("`No_content", `Quick, no_content_status) ] )

let body_allowed_tests =
  let case (status, expected) =
    ( Status.to_string status,
      `Quick,
      fun () ->
        Status.body_allowed status
        |> aeb ("body_allowed " ^ Status.to_string status) expected )
  in
  ( "body_allowed",
    List.map case
      [
        (`Continue, false);
        (`Switching_protocols, false);
        (`Processing, false);
        (`Checkpoint, false);
        (`No_content, false);
        (`Not_modified, false);
        (`Code 199, false);
        (`OK, true);
        (`Created, true);
        (`Found, true);
        (`Bad_request, true);
        (`Internal_server_error, true);
        (`Code 250, true);
      ] )

let () =
  Alcotest.run "test_response"
    [ requires_content_length_tests; content_length_tests; body_allowed_tests ]
