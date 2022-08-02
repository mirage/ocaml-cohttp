module Bytebuffer = Http_bytebuffer.Bytebuffer

module Src = struct
  type src = { str : string; mutable pos : int; mutable reads : int }

  let create str = { str; pos = 0; reads = 10 }

  let refill r buf ~pos ~len =
    if r.reads = 0 then raise Exit
    else (
      r.reads <- r.reads - 1;
      let available = String.length r.str - r.pos in
      if available = 0 then `Eof
      else
        let read_len = min len available in
        BytesLabels.blit_string ~src:r.str ~src_pos:r.pos ~dst:buf ~dst_pos:pos
          ~len:read_len;
        r.pos <- r.pos + read_len;
        `Ok read_len)
end

module Refill =
  Bytebuffer.Make
    (struct
      type 'a t = 'a

      let ( >>| ) x f = f x
      let ( >>= ) x f = f x
      let return x = x
    end)
    (Src)

let%expect_test "read line" =
  let test line buf_size =
    let src = Src.create line in
    let buf = Bytebuffer.create buf_size in
    let res = Refill.read_line buf src in
    match res with
    | None -> print_endline "failed to read line"
    | Some line -> Printf.printf "read line: %S\n" line
    | exception Exit -> print_endline "failed to read - infinite loop"
  in
  let line = "foobar\r\n" in
  test line (String.length line);
  [%expect {| read line: "foobar" |}];
  test line (String.length line - 1);
  [%expect {| read line: "foobar" |}];
  let line = "foobar\r\n" in
  test line (String.length line - 1);
  [%expect {| read line: "foobar" |}];
  test line (String.length line);
  [%expect {| read line: "foobar" |}];
  let line = "foobar\r" in
  test line (String.length line + 10);
  [%expect {| failed to read line |}];
  test line (String.length line - 1);
  [%expect {| failed to read line |}]

let%expect_test "read fixed" =
  let src = "foobar" in
  let src_len = String.length src in
  let test buf_size =
    let src = Src.create src in
    let buf = Bytebuffer.create buf_size in
    match Refill.read buf src src_len with
    | res ->
        Printf.printf "buf size=%d: reading %d bytes we get %d bytes\n" buf_size
          src_len (String.length res)
    | exception Exit -> print_endline "failed to read - infinite loop"
  in
  test src_len;
  [%expect {| buf size=6: reading 6 bytes we get 6 bytes |}];
  test (src_len - 1);
  [%expect {| buf size=5: reading 6 bytes we get 5 bytes |}]
