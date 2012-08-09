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

module M(IO:IO.M) = struct
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
        |None | Some 0 -> return None
        |Some count -> begin
          read ic count >>= fun buf ->
          read_line ic >>= fun trailer ->
          match trailer with
          |Some "" -> return (Some buf)
          |_ -> return None (* TODO trailer headers *)
        end
      end
      |None -> return None
  
    let write oc buf =
      let len = String.length buf in
      write oc (Printf.sprintf "%x\r\n" len) >>= fun () ->
      write oc buf
  end
  
  module Fixed = struct
    let read ~len ic =
      (* TODO functorise string to a bigbuffer *)
      let len = Int64.to_int len in
      let buf = String.create len in
      read_exactly ic buf 0 len >>= function
      |false -> return None
      |true -> return (Some buf)   
  end
  
  module Unknown = struct
    (* If we have no idea, then read one chunk and return it. *)
    let read ic =
      read ic 16384 >>= fun buf -> return (Some buf)
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
  
  end
