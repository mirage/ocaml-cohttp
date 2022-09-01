open Http

let aeb = Alcotest.check Alcotest.bool

let requires_content_length_tests =
  let valid_meth () =
    [ `POST; `PUT; `PATCH ]
    |> List.map (fun meth ->
           Request.make ~meth "p" |> Request.requires_content_length)
    |> List.for_all Fun.id
    |> aeb "requires_content_length m = true, where m is `POST, `PUT or `PATCH"
         true
  in
  let invalid_meth () =
    [ `GET; `HEAD; `DELETE; `OPTIONS; `TRACE; `CONNECT; `Other "h" ]
    |> List.map (fun meth ->
           Request.make ~meth "p" |> Request.requires_content_length)
    |> List.for_all not
    |> aeb
         {| requires_content_length m = false, where m is `GET; `HEAD;`DELETE;`OPTIONS;`TRACE; `CONNECT;`Other "h" |}
         true
  in
  ( "requires_content_length",
    [
      ("Valid meth", `Quick, valid_meth); ("Invalid meth", `Quick, invalid_meth);
    ] )

let content_length_tests =
  let some_x () =
    [ (`POST, "0"); (`PUT, "233"); (`PATCH, "012345") ]
    |> List.map (fun (meth, len) ->
           match
             Request.make ~meth
               ~headers:(Header.of_list [ ("Content-Length", len) ])
               "p"
             |> Request.content_length
           with
           | Some x -> int_of_string len = x
           | None -> false)
    |> List.for_all Fun.id
    |> aeb "content_length t = Some x" true
  in

  let none () =
    [ (`POST, "-1"); (`PUT, "-233"); (`PATCH, "abc") ]
    |> List.map (fun (meth, len) ->
           match
             Request.make ~meth
               ~headers:(Header.of_list [ ("Content-Length", len) ])
               "p"
             |> Request.content_length
           with
           | Some _ -> false
           | None -> true)
    |> List.for_all Fun.id
    |> aeb "content_length t = None" true
  in

  let method_ () =
    [ `GET; `HEAD; `DELETE; `OPTIONS; `TRACE; `CONNECT; `Other "h" ]
    |> List.map (fun meth ->
           match Request.make ~meth "p" |> Request.content_length with
           | Some _ -> false
           | None -> true)
    |> List.for_all Fun.id
    |> aeb "content_length t = None" true
  in
  ( "content_length",
    [
      ("Some content_length", `Quick, some_x);
      ("None : Invalid content_length integer", `Quick, none);
      ("None : Method", `Quick, method_);
    ] )

let () =
  Alcotest.run "test_request"
    [ requires_content_length_tests; content_length_tests ]
