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

open Transfer

module Make(IO : S.IO) = struct
  open IO
  type reader = unit -> Transfer.chunk IO.t
  type writer = string -> unit IO.t

  module Chunked = struct
    let read ~remaining ic () =
      (* read between 0 and 32Kbytes of a chunk *)
      let read_chunk_fragment () =
        let max_read_len = Int64.of_int 0x8000 in
        let len = min !remaining max_read_len in
        read ic (Int64.to_int len) >>= function
        | "" -> return ""
        | buf ->
          let read_len = Int64.of_int (String.length buf) in
          remaining := Int64.sub !remaining read_len;
          (if !remaining = 0L (* End_of_chunk *)
           then read_line ic (* Junk the CRLF at end of chunk *)
           else return None) >>= fun _ ->
          return buf
      in
      if !remaining = 0L then
        (* Beginning of a chunk: read chunk size, read up to 32K bytes *)
        read_line ic >>= function
        | None -> return Done
        | Some chunk_size_hex -> begin
            let chunk_size =
              let hex =
                (* chunk size is optionally delimited by ; *)
                try String.sub chunk_size_hex 0 (String.rindex chunk_size_hex ';')
                with _ -> chunk_size_hex in
              try Some (Int64.of_string ("0x" ^ hex)) with _ -> None
            in
            match chunk_size with
            | None -> return Done
            | Some 0L -> (* TODO: Trailer header support *)
              let rec read_until_empty_line () =
                read_line ic >>= function
                | None | Some "" -> return Done
                | Some _trailer -> read_until_empty_line ()
              in
              read_until_empty_line ()
            | Some count ->
              remaining := count;
              read_chunk_fragment () >>= function
              | "" -> return Done (* 0 bytes read means EOF *)
              | buf -> return (Chunk buf)
          end
      else (* Middle of a chunk, read up to 32K bytes *)
        read_chunk_fragment () >>= function
        | "" -> return Done (* 0 bytes read means EOF *)
        | buf -> return (Chunk buf)

    let write oc buf =
      let len = String.length buf in
      write oc (Printf.sprintf "%x\r\n" len) >>= fun () ->
      write oc buf >>= fun () ->
      write oc "\r\n"
  end

  module Fixed = struct
    let read ~remaining ic () =
      (* TODO functorise string to a bigbuffer *)
      match !remaining with
      | 0L -> return Done
      | len ->
        let len' = Int64.to_int len in
        let read_len = min len' 0x8000 in
        read ic read_len >>= function
        | "" -> return Done
        | buf ->
          remaining := Int64.sub !remaining (Int64.of_int (String.length buf));
          return (match !remaining with
            | 0L -> Final_chunk buf
            | _  -> Chunk buf)

    (* TODO enforce that the correct length is written? *)
    let write oc buf =
      write oc buf
  end

  module Unknown = struct
    (* If we have no idea, then read until EOF (connection shutdown by
       the remote party). *)
    let read ic () =
      read ic 4096 >>= fun buf ->
      if buf = "" then return(Done)
      else return (Chunk buf)

    let write oc buf =
      write oc buf
  end

  let write_and_flush fn oc buf =
    fn oc buf >>= fun () ->
    IO.flush oc

  let make_reader =
    function
    | Chunked -> Chunked.read ~remaining:(ref 0L)
    | Fixed len -> Fixed.read ~remaining:(ref len)
    | Unknown -> Unknown.read

  let write_ignore_blank writer io s =
    if String.length s = 0
    then return ()
    else writer io s

  let make_writer ?(flush=false) mode =
    match flush with
    | false -> begin
        match mode with
        | Chunked -> Chunked.write
        | Fixed len -> Fixed.write
        | Unknown -> Unknown.write
      end
    | true -> begin
        match mode with
        | Chunked -> write_and_flush Chunked.write
        | Fixed len -> write_and_flush Fixed.write
        | Unknown -> write_and_flush Unknown.write
      end |> write_ignore_blank

  let read reader = reader ()
  let write writer buf = writer buf

  let to_string reader =
    let buf = Buffer.create 256 in
    let rec loop () =
      read reader >>= function
      |Chunk c ->
        Buffer.add_string buf c;
        loop ()
      |Final_chunk c ->
        Buffer.add_string buf c;
        return (Buffer.contents buf)
      |Done ->
        return (Buffer.contents buf)
    in
    loop ()
end
