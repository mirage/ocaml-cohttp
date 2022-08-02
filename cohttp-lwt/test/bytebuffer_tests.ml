module Bytebuffer = Cohttp_lwt.Private.Bytebuffer

let%expect_test "read" =
  let line = "foobar\r\n" in
  let test buf_size =
    let buf = Bytebuffer.create buf_size in
    let refill =
      let line_pos = ref 0 in
      let reads_left = ref 10 in
      fun buf ~pos ~len ->
        if !reads_left = 0 then raise Exit;
        decr reads_left;
        let available = String.length line - !line_pos in
        if available = 0 then Lwt.return `Eof
        else
          let read_len = min len available in
          BytesLabels.blit_string ~src:line ~src_pos:!line_pos ~dst:buf
            ~dst_pos:pos ~len:read_len;
          line_pos := !line_pos + read_len;
          Lwt.return (`Ok read_len)
    in
    let open Lwt.Syntax in
    Lwt_main.run
    @@ Lwt.catch
         (fun () ->
           let+ res = Bytebuffer.read_line buf refill in
           match res with
           | None -> print_endline "failed to read line"
           | Some line -> Printf.printf "read line: %S\n" line)
         (function
           | Exit -> Lwt.return @@ print_endline "failed to read"
           | _ -> assert false)
  in
  test (String.length line);
  [%expect {| read line: "foobar" |}];
  test (String.length line - 1);
  [%expect {| read line: "foobar" |}]
