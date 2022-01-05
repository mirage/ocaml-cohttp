open Lwt.Infix
module Bytebuffer = Cohttp_lwt.Private.Bytebuffer

module Make (Channel : Mirage_channel.S) = struct
  exception Read_exn of Channel.error

  type t = { chan : Channel.t; buf : Bytebuffer.t }

  let refill chan buf ~pos ~len =
    Channel.read_some ~len chan >>= function
    | Ok (`Data v) ->
        let len = Cstruct.length v in
        Cstruct.blit_to_bytes v 0 buf pos len;
        Lwt.return (`Ok len)
    | Ok `Eof -> Lwt.return `Eof
    | Error e -> Lwt.fail (Read_exn e)

  let create ?(buf_len = 0x4000) chan =
    { buf = Bytebuffer.create buf_len; chan }

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

  let close t = Channel.close t.chan
end
