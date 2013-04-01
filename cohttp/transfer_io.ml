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

module Make(IO : IO.S) = struct
  open IO

  module Chunked = struct
    let read ic =
      (* Read chunk size *)
      read_line ic >>= function
      |Some chunk_size_hex -> begin
        let chunk_size = 
          let hex =
            (* chunk size is optionally delimited by ; *)
            try String.sub chunk_size_hex 0 (String.rindex chunk_size_hex ';')
            with _ -> chunk_size_hex in
          try Some (int_of_string ("0x" ^ hex)) with _ -> None
        in
        match chunk_size with
        |None | Some 0 -> return Done
        |Some count -> begin
          read_exactly ic count >>=
          function
          |None -> return Done
          |Some buf ->
            read_line ic >>= fun _ -> (* Junk the CRLF at end of chunk *)
            return (Chunk buf)
        end
      end
      |None -> return Done
 
    let write oc buf =
      let len = String.length buf in
      write oc (Printf.sprintf "%x\r\n" len) >>
      write oc buf >>
      write oc "\r\n"
  end
  
  module Fixed = struct
    let read ~len ic =
      (* TODO functorise string to a bigbuffer *)
      match len with
      |0 -> return Done
      |len ->
        read_exactly ic len >>= function
        |None -> return Done
        |Some buf -> return (Final_chunk buf)

    (* TODO enforce that the correct length is written? *)
    let write oc buf =
      write oc buf
  end
  
  module Unknown = struct
    (* If we have no idea, then read one chunk and return it.
     * TODO should this be a read with an explicit timeout? *)
    let read ic =
      read ic 16384 >>= fun buf -> return (Final_chunk buf)

    let write oc buf =
      write oc buf
  end
  
  let read =
    function
    | Chunked -> Chunked.read
    | Fixed len -> Fixed.read ~len
    | Unknown -> Unknown.read

  let write =
    function
    | Chunked -> Chunked.write
    | Fixed len -> Fixed.write
    | Unknown -> Unknown.write

  let to_string encoding ic =
    let buf = Buffer.create 256 in
    let rec loop () =
      read encoding ic >>= function
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
