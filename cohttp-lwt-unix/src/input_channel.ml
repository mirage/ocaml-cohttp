open Lwt.Infix
module Bytebuffer = Cohttp_lwt.Private.Bytebuffer

type t = { buf : Bytebuffer.t; chan : Lwt_io.input_channel }

let refill ic buf ~pos ~len =
  Lwt.catch
    (fun () ->
      Lwt_io.read_into ic buf pos len >|= fun c -> if c > 0 then `Ok c else `Eof)
    (function Lwt_io.Channel_closed _ -> Lwt.return `Eof | exn -> raise exn)

let create ?(buf_len = 0x4000) chan = { buf = Bytebuffer.create buf_len; chan }
let read_line_opt t = Bytebuffer.read_line t.buf (refill t.chan)
let read t count = Bytebuffer.read t.buf (refill t.chan) count
let refill t = Bytebuffer.refill t.buf (refill t.chan)

let with_input_buffer t ~f =
  let buf = Bytebuffer.unsafe_buf t.buf in
  let pos = Bytebuffer.pos t.buf in
  let len = Bytebuffer.length t.buf in
  let res, consumed = f (Bytes.unsafe_to_string buf) ~pos ~len in
  Bytebuffer.drop t.buf consumed;
  res

let close t = Lwt_io.close t.chan
