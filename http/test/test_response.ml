open Http

let aeb = Alcotest.check Alcotest.bool

let requires_content_length () : unit =
  let response = Response.make ~status:`No_content () in
  Response.requires_content_length response
  |> aeb "requires_content_length m = true, where s is `No_content (204)" false;

  let response = Response.make ~status:`Continue () in
  Response.requires_content_length response
  |> aeb "requires_content_length m = true, where s is `Continue (100)" false;

  let response = Response.make ~status:`OK () in
  Response.requires_content_length response
  |> aeb "requires_content_length s = true, where s is `OK (200" true;

  let headers =
    let headers = Http.Header.init () in
    Http.Header.add headers "Transfer-Encoding" "chunked"
  in
  Response.make ~status:`OK ~headers ()
  |> Response.requires_content_length
  |> aeb "requires_content_length s = true, where s is `OK (200" false

let tests =
  ( "requires_content_length tests",
    [ ("Response.requires_content_length", `Quick, requires_content_length) ] )

let () = Alcotest.run "test_request" [ tests ]
