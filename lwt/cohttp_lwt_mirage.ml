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

open Cohttp

module IO = struct

  open Net

  type 'a t = 'a Lwt.t
  let (>>=) = Lwt.bind
  let return = Lwt.return

  type ic = Channel.TCPv4.t
  type oc = Channel.TCPv4.t

  let iter fn x = Lwt_list.iter_s fn x

  let read_line ic =
    match_lwt Channel.TCPv4.read_line ic with
    |[] -> return None
    |bufs -> return (Some (Cstruct.copy_buffers bufs))

  let read ic len = 
   try_lwt
     lwt iop = Channel.TCPv4.read_some ~len ic in
     return (Cstruct.to_string iop)
   with End_of_file -> return ""

  let read_exactly ic buf off len =
    let rec read acc left =
      match left with
      |0 -> 
        return (List.rev acc)
      |len ->
        lwt iop = Channel.TCPv4.read_some ~len ic in
        read (iop::acc) (left - (Cstruct.len iop))
    in
    lwt iov = read [] len in
    (* XXX TODO this is hyper slow! *)
    let srcbuf = Cstruct.copy_buffers iov in
    String.blit srcbuf 0 buf off (String.length srcbuf);
    return true

  let write oc buf = 
    Channel.TCPv4.write_string oc buf 0 (String.length buf);
    return ()

  let write_line oc buf =
    Channel.TCPv4.write_line oc buf;
    return ()
end

module Net = struct
  open Net
  type ic = Channel.TCPv4.t
  type oc = Channel.TCPv4.t

  let connect_uri uri =
    Lwt.fail (Failure "not implemented")

  let connect ?ssl address port =
    Lwt.fail (Failure "not implemented")

  let close_in ic = ()
  let close_out ic = ()
  let close ic oc = Lwt.ignore_result (Channel.TCPv4.close ic)
end

module Body  = Transfer.Make(IO)
module Request = Request.Make(IO)
module Response = Response.Make(IO)
module Client = Cohttp_lwt.Client(Request)(Response)(Net)
module Server = Cohttp_lwt.Server(Request)(Response)(Net)
