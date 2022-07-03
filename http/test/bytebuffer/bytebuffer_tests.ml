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

let%expect_test "read" =
  let line = "foobar\n" in
  let src = Src.create line in
  let test buf_size =
    let buf = Bytebuffer.create buf_size in
    let res = Refill.read_line buf src in
    match res with
    | None -> print_endline "failed to read line"
    | Some line -> Printf.printf "read line: %S\n" line
    | exception Exit -> print_endline "failed to read"
  in
  test (String.length line);
  [%expect {| read line: "foobar" |}];
  test (String.length line - 1);
  [%expect {| failed to read line |}]
