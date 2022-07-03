open! Core
open! Async

module Bytebuffer = struct
  module Bytebuffer = Http_bytebuffer.Bytebuffer
  include Bytebuffer

  include
    Bytebuffer.Make
      (Deferred)
      (struct
        type src = Reader.t

        let refill reader buf ~pos ~len = Reader.read reader ~pos ~len buf
      end)
end

type t = { buf : Bytebuffer.t; reader : Reader.t }

let create ?(buf_len = 0x4000) reader =
  { buf = Bytebuffer.create buf_len; reader }

let read_line_opt t = Bytebuffer.read_line t.buf t.reader
let read t count = Bytebuffer.read t.buf t.reader count
let refill t = Bytebuffer.refill t.buf t.reader

let with_input_buffer t ~f =
  let buf = Bytebuffer.unsafe_buf t.buf in
  let pos = Bytebuffer.pos t.buf in
  let len = Bytebuffer.length t.buf in
  let res, consumed =
    f (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf) ~pos ~len
  in
  Bytebuffer.drop t.buf consumed;
  res

let is_closed t = Reader.is_closed t.reader
let close t = Reader.close t.reader
let close_finished t = Reader.close_finished t.reader

let transfer t writer =
  let finished = Ivar.create () in
  upon (Pipe.closed writer) (fun () -> Ivar.fill_if_empty finished ());
  let rec loop () =
    refill t >>> function
    | `Eof -> Ivar.fill_if_empty finished ()
    | `Ok ->
        let payload =
          with_input_buffer t ~f:(fun buf ~pos ~len ->
              (String.sub buf ~pos ~len, len))
        in
        Pipe.write writer payload >>> fun () -> loop ()
  in
  loop ();
  Ivar.read finished

let to_reader info ic =
  let reader, writer = Pipe.create () in
  ( transfer ic writer >>> fun () ->
    close ic >>> fun () -> Pipe.close writer );
  Reader.of_pipe info reader
