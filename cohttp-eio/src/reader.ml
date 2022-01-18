(* Based on https://github.com/inhabitedtype/angstrom/blob/master/lib/buffering.ml *)
type t = {
  source : Eio.Flow.source;
  mutable buf : Bigstringaf.t;
  mutable off : int;
  mutable len : int;
  mutable pos : int; (* Parser position *)
  mutable committed_bytes : int; (* Total bytes read so far *)
}

let create len source =
  assert (len > 0);
  let buf = Bigstringaf.create len in
  { source; buf; off = 0; len = 0; pos = 0; committed_bytes = 0 }

let default_io_buffer_size = 4096
let writable_space t = Bigstringaf.length t.buf - t.len
let trailing_space t = Bigstringaf.length t.buf - (t.off + t.len)

let create ?(buffer_size = default_io_buffer_size) source =
  assert (buffer_size > 0);
  let buf = Bigstringaf.create buffer_size in
  let off = 0 in
  let len = 0 in
  { buf; off; len; source; buffer_size }

let source t = t.source
let buffer_size t = t.buffer_size
let offset t = t.off
let length t = t.len
let committed_bytes t = t.committed_bytes
let pos t = t.pos
let incr_pos ?(n = 1) t = t.pos <- t.pos + n
let writable_space t = Bigstringaf.length t.buf - t.len
let trailing_space t = Bigstringaf.length t.buf - (t.off + t.len)

let compress t =
  Bigstringaf.unsafe_blit t.buf ~src_off:t.off t.buf ~dst_off:0 ~len:t.len;
  t.off <- 0

let grow t to_copy =
  let old_len = Bigstringaf.length t.buf in
  let new_len = ref old_len in
  let space = writable_space t in
  while space + !new_len - old_len < to_copy do
    new_len := 3 * (!new_len + 1) / 2
  done;
  let new_buf = Bigstringaf.create !new_len in
  Bigstringaf.unsafe_blit t.buf ~src_off:t.off new_buf ~dst_off:0 ~len:t.len;
  t.buf <- new_buf;
  t.off <- 0

let adjust_buffer t to_read =
  if trailing_space t < to_read then
    if writable_space t < to_read then grow t to_read else compress t

let consume t n =
  assert (t.len >= n);
  assert (t.pos >= n);
  t.off <- t.off + n;
  t.len <- t.len - n;
  t.pos <- t.pos - n;
  t.committed_bytes <- t.committed_bytes + n

let commit t = consume t t.pos

let fill t to_read =
  adjust_buffer t to_read;
  let off = t.off + t.len in
  let len = trailing_space t in
  let cs = Cstruct.of_bigarray ~off ~len t.buf in
  let got = Eio.Flow.read t.source cs in
  t.len <- t.len + got;
  got

let unsafe_get t off = Bigstringaf.unsafe_get t.buf (t.off + off)

let substring t ~off ~len =
  let b = Bytes.create len in
  Bigstringaf.unsafe_blit_to_bytes t.buf ~src_off:(t.off + off) b ~dst_off:0
    ~len;
  Bytes.unsafe_to_string b

let copy t ~off ~len = Bigstringaf.copy t.buf ~off:(t.off + off) ~len

let adjust_buffer t to_read =
  if trailing_space t < to_read then
    if writable_space t < to_read then grow t to_read else compress t

let parse t p =
  let open Angstrom in
  let rec loop = function
    | Unbuffered.Partial k ->
        consume t k.committed;
        let got = fill t 128 in
        let more =
          if got = 0 then Unbuffered.Complete else Unbuffered.Incomplete
        in
        loop (k.continue t.buf ~off:t.off ~len:t.len more)
    | Unbuffered.Done (len, a) ->
        consume t len;
        a
    | Unbuffered.Fail (len, marks, err) ->
        consume t len;
        raise (Parse_error (String.concat " > " marks ^ ": " ^ err))
  in
  loop (Unbuffered.parse p)
