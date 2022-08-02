module Bytes = BytesLabels

(* Bytebuffer is split into three regions using two separate indices that are used
   to support read and write operations.
   +--------------------+---------------------------+----------------------------+
   | Consumed Bytes     | Bytes available to read   | Empty space for writing    |
   +--------------------+---------------------------+----------------------------+
   |     0 <=       pos_read         <=          pos_fill              <= capacity

   Consumed Bytes: This is content that's already consumed via a get/read operation.
   This space can be safely reclaimed.

   Bytes available to read: This is the actual content that will be surfaced to users via
   get/read operations on the bytebuffer.

   Empty space for writing: This is space that will be filled by any set/write operations
   on the bytebuffer.
*)

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

let rec index_rec t ch idx len =
  if idx = len then -1
  else if Char.equal (Bytes.unsafe_get t.buf (t.pos_read + idx)) ch then
    idx + t.pos_read
  else index_rec t ch (idx + 1) len

let index t ch = index_rec t ch 0 (length t)
let to_string t = Bytes.sub_string t.buf ~pos:t.pos_read ~len:(length t)

module Make (IO : sig
  type 'a t

  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end) (Refill : sig
  type src

  val refill : src -> bytes -> pos:int -> len:int -> [ `Ok of int | `Eof ] IO.t
end) =
struct
  open IO

  let refill t src =
    compact t;
    Refill.refill src t.buf ~pos:t.pos_fill
      ~len:(Bytes.length t.buf - t.pos_fill)
    >>| function
    | `Eof -> `Eof
    | `Ok count ->
        t.pos_fill <- t.pos_fill + count;
        `Ok

  let get_line t idx =
    let len = idx - t.pos_read in
    if len >= 1 && Char.equal (Bytes.unsafe_get t.buf (idx - 1)) '\r' then (
      let res =
        let len = len - 1 in
        Bytes.sub_string t.buf ~pos:t.pos_read ~len
      in
      drop t (len + 1);
      Some res)
    else None

  let get_line_buf t buf idx =
    let len = idx - t.pos_read in
    Buffer.add_subbytes buf t.buf t.pos_read len;
    drop t (len + 1)

  let rec read_line_slow t reader buf =
    if length t = 0 then
      refill t reader >>= function
      | `Ok -> read_line_slow t reader buf
      | `Eof -> IO.return `Eof
    else
      let idx = index t '\n' in
      if idx > -1 then (
        get_line_buf t buf idx;
        IO.return `Ok)
      else
        let len = length t in
        Buffer.add_subbytes buf t.buf t.pos_read len;
        drop t len;
        read_line_slow t reader buf

  let read_line t reader =
    let idx = index t '\n' in
    if idx = -1 then
      let buf = Buffer.create (length t + 1) in
      read_line_slow t reader buf >>| function
      | `Eof -> None
      | `Ok ->
          let len = Buffer.length buf in
          if len = 0 then None
          else if len >= 2 && Buffer.nth buf (len - 1) = '\r' then
            Some (Buffer.sub buf 0 (len - 1))
          else None
    else
      let line = get_line t idx in
      IO.return line

  let rec read t reader len =
    let length = length t in
    if length > 0 then (
      let to_read = min length len in
      let buf = Bytes.sub_string t.buf ~pos:t.pos_read ~len:to_read in
      drop t to_read;
      IO.return buf)
    else
      refill t reader >>= function
      | `Ok -> read t reader len
      | `Eof -> IO.return ""
end
