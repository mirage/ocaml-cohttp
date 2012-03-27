(*
  Copyright (C) <2012> Anil Madhavapeddy <anil@recoil.org>

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU Library General Public License as
  published by the Free Software Foundation, version 2.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
  USA
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
      |0 ->
         return None
      |count -> begin
         match_lwt Lwt_io.read ~count ic with 
         |"" ->
           return None
         |chunk ->
           len := !len - (String.length chunk);
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
  | Fixed of int 
  | Unknown

let read =
  function
  | Chunked -> Chunked.read
  | Fixed len -> Fixed.read ~len
  | Unknown -> Unknown.read
