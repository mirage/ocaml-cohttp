open! Core
open! Async

module Bytebuffer = struct
  open Core
  open Async

  type t = {
    mutable buf : Bytes.t;
    mutable pos_read : int;
    mutable pos_fill : int;
  }

  let create size =
    let buf = Bytes.create size in
    { buf; pos_read = 0; pos_fill = 0 }

  let unsafe_buf t = t.buf
  let pos t = t.pos_read

  let compact t =
    if t.pos_read > 0 then (
      let len = t.pos_fill - t.pos_read in
      Bytes.blit ~src:t.buf ~dst:t.buf ~src_pos:t.pos_read ~dst_pos:0 ~len;
      t.pos_read <- 0;
      t.pos_fill <- len)

  let length t = t.pos_fill - t.pos_read

  let drop t len =
    if len < 0 || len > length t then
      invalid_arg "Bytebuffer.drop: Index out of bounds";
    t.pos_read <- t.pos_read + len

  let refill t reader =
    compact t;
    Reader.read reader t.buf ~pos:t.pos_fill
      ~len:(Bytes.length t.buf - t.pos_fill)
    >>| function
    | `Ok count ->
        assert (count > 0);
        t.pos_fill <- t.pos_fill + count;
        `Ok
    | `Eof -> `Eof

  let rec index_rec t ch idx len =
    if idx = len then -1
    else if Char.equal (Bytes.unsafe_get t.buf (t.pos_read + idx)) ch then
      idx + t.pos_read
    else index_rec t ch (idx + 1) len

  let index t ch = index_rec t ch 0 (length t)
  let to_string t = Bytes.To_string.sub t.buf ~pos:t.pos_read ~len:(length t)

  let rec read_line t reader =
    let idx = index t '\n' in
    if idx = -1 then (
      refill t reader >>= function
      | `Ok ->
          Logs.debug (fun m -> m "Read Refill: %S" (to_string t));
          read_line t reader
      | `Eof ->
          Logs.debug (fun m -> m "Read Eof: %S" (to_string t));
          let len = length t in
          if len = 0 then return None
          else
            let line = Bytes.To_string.sub t.buf ~pos:t.pos_read ~len in
            drop t len;
            return (Some line))
    else
      let len = idx - t.pos_read in
      Logs.debug (fun m -> m "Read found: %S" (to_string t));
      Logs.debug (fun m ->
          m "Index: %d, pos_read: %d, length: %d" idx t.pos_read (length t));
      if len >= 1 && Char.equal (Bytes.unsafe_get t.buf (idx - 1)) '\r' then (
        let line = Bytes.To_string.sub t.buf ~pos:t.pos_read ~len:(len - 1) in
        drop t (len + 1);
        return (Some line))
      else
        let line = Bytes.To_string.sub t.buf ~pos:t.pos_read ~len in
        drop t (len + 1);
        return (Some line)

  let rec read t reader len =
    let length = length t in
    if length > 0 then (
      let to_read = min length len in
      let buf = Bytes.To_string.sub t.buf ~pos:t.pos_read ~len:to_read in
      drop t to_read;
      return buf)
    else
      refill t reader >>= function
      | `Ok -> read t reader len
      | `Eof -> return ""
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
