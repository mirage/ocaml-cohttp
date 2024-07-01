module Parser = Http.Private.Parser

let print_request = function
  | Error Parser.Partial -> print_endline "partial header"
  | Error (Msg m) -> print_endline m
  | Ok (req, n) ->
      Format.printf "%a@." Http.Request.pp req;
      if n > 0 then Format.printf "leftover: %d@." n

let%expect_test "line folding" =
  let buf =
    "GET / HTTP/1.1\r\n\
     Host: localhost:8080\r\n\
     Line-Folded: foo\r\n\
    \ bar\r\n\
     \r\n\
     foboar"
  in
  print_request (Parser.parse_request buf);
  [%expect {| partial header |}]
