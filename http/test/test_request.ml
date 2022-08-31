open Http

let aeb = Alcotest.check Alcotest.bool

let requires_content_length () =
  [ `POST; `PUT; `PATCH ]
  |> List.map (fun meth ->
         Request.make ~meth "p" |> Request.requires_content_length)
  |> List.for_all Fun.id
  |> aeb "requires_content_length m = true, where m is `POST, `PUT or `PATCH"
       true;

  [ `GET; `HEAD; `DELETE; `OPTIONS; `TRACE; `CONNECT; `Other "h" ]
  |> List.map (fun meth ->
         Request.make ~meth "p" |> Request.requires_content_length)
  |> List.for_all not
  |> aeb
       {| requires_content_length m = false, where m is `GET; `HEAD;`DELETE;`OPTIONS;`TRACE; `CONNECT;`Other "h" |}
       true

let tests =
  ( "requires_content_length tests",
    [ ("Request.requires_content_length", `Quick, requires_content_length) ] )

let () = Alcotest.run "test_request" [ tests ]
