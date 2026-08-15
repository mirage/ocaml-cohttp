open Cohttp

let invalid name f =
  Alcotest.check_raises name (Invalid_argument "") (fun () ->
      match f () with
      | exception Invalid_argument _ -> raise (Invalid_argument "")
      | _ -> ())

let bounds () =
  let t = Bigstring.create 8 in
  Bigstring.blit_from_string "abcdefgh" ~src_off:0 t ~dst_off:0 ~len:8;
  Alcotest.(check string)
    "in range" "cde"
    (Bigstring.sub_string t ~off:2 ~len:3);
  invalid "sub_string past end" (fun () -> Bigstring.sub_string t ~off:6 ~len:3);
  invalid "sub_string negative off" (fun () ->
      Bigstring.sub_string t ~off:(-1) ~len:1);
  invalid "sub_string negative len" (fun () ->
      Bigstring.sub_string t ~off:0 ~len:(-1));
  invalid "blit_from_string past source" (fun () ->
      Bigstring.blit_from_string "ab" ~src_off:1 t ~dst_off:0 ~len:2);
  invalid "blit_from_string past dest" (fun () ->
      Bigstring.blit_from_string "abcdefgh" ~src_off:0 t ~dst_off:1 ~len:8);
  invalid "blit past dest" (fun () ->
      Bigstring.blit t ~src_off:0 (Bigstring.create 4) ~dst_off:0 ~len:8)

let () =
  Alcotest.run "test_bigstring"
    [ ("bounds", [ Alcotest.test_case "bounds" `Quick bounds ]) ]
