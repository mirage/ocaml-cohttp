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
    let read ic () =
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
      write oc (Printf.sprintf "%x\r\n" len) >>= fun () ->
      write oc buf >>= fun () ->
      write oc "\r\n"
  end

  module Fixed = struct
    let read ~remaining ic () =
      (* TODO functorise string to a bigbuffer *)
      match !remaining with
      |0L -> return Done
      |len ->
        read ic (Int64.to_int len) >>= function
        | "" -> return Done
        | buf ->
          remaining := Int64.sub !remaining (Int64.of_int (String.length buf));
          return (match !remaining with
                  | 0L -> Final_chunk buf
                  | _ -> Chunk buf)

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
    | Chunked -> Chunked.read
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
