(*
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Lwt

module Chunked = struct
  let read ic =
    Lwt_stream.from (fun () ->
      (* Read chunk size *)
      lwt chunk_size_hex = Lwt_io.read_line ic in
      let chunk_size = 
        try
          Some (int_of_string ("0x" ^ chunk_size_hex))
        with _ ->
          None
      in
      match chunk_size with
      |None ->
         return None (* Malformd chunk, terminate stream *)
      |Some 0 ->
         return None
      |Some count -> begin
         let buf = String.create count in
         lwt () = Lwt_io.read_into_exactly ic buf 0 count in
         match_lwt Lwt_io.read_line ic with
         |"" -> return (Some buf)
         |x -> return None (* malformed chunk *)
      end
    )

  (* TODO who closes the channel when this is done? *)
  let writer oc =
    let stream, stream_push = Lwt_stream.create () in
    let stream_t = 
      Lwt_stream.iter_s (fun chunk -> 
        let len = String.length chunk in
        lwt () = Lwt_io.fprintf oc "%x\r\n" len in
        Lwt_io.write oc chunk
      ) stream
    in
    stream_t, stream_push
end

module Fixed = struct
  let read ~len ic =
    let len = ref len in
    Lwt_stream.from (fun () ->
      match !len with
      |0L ->
         return None
      |count -> begin
         match_lwt Lwt_io.read ~count:(Int64.to_int count) ic with 
         |"" ->
           return None
         |chunk ->
           len := Int64.sub !len (Int64.of_int (String.length chunk));
           return (Some chunk)
      end
    )
end

module Unknown = struct
  (* If we have no idea, then read one chunk and return it.
   * Arbitrary timeout in case it hangs for ages *)
  let read ic =
    let timeout = 10.0 in
    Lwt_stream.from (fun () ->
      let timeout_t =
        lwt () = Lwt_unix.timeout timeout in
        return None
      in
      let read_t =
        match_lwt Lwt_io.read ic with
        |"" -> return None
        |buf -> return (Some buf)
      in
      read_t <?> timeout_t
    )
end

type encoding =
  | Chunked
  | Fixed of int64
  | Unknown

let encoding_to_string =
  function
  | Chunked -> "chunked"
  | Fixed i -> Printf.sprintf "fixed[%Ld]" i
  | Unknown -> "unknown"

let read =
  function
  | Chunked -> Chunked.read
  | Fixed len -> Fixed.read ~len
  | Unknown -> Unknown.read
