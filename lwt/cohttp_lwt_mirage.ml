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

  type ic = Channel.t
  type oc = Channel.t

  let iter fn x = Lwt_list.iter_s fn x

  let read_line ic =
    match_lwt Channel.read_line ic with
    |[] -> return None
    |bufs -> return (Some (Cstruct.copyv bufs))

  let read ic len = 
   try_lwt
     lwt iop = Channel.read_some ~len ic in
     return (Cstruct.to_string iop)
   with End_of_file -> return ""

  let read_exactly ic buf off len =
    let rec read acc left =
      match left with
      |0 -> 
        return (List.rev acc)
      |len ->
        lwt iop = Channel.read_some ~len ic in
        read (iop::acc) (left - (Cstruct.len iop))
    in
    lwt iov = read [] len in
    (* XXX TODO this is hyper slow! *)
    let srcbuf = Cstruct.copyv iov in
    String.blit srcbuf 0 buf off (String.length srcbuf);
    return true

  let write oc buf = 
    Channel.write_string oc buf 0 (String.length buf);
    Channel.flush oc

  let write_line oc buf =
    Channel.write_line oc buf;
    Channel.flush oc
end

open Lwt

module Net_IO = struct
  open Net
  type ic = Channel.t
  type oc = Channel.t

  let connect_uri uri =
    fail (Failure "not implemented")

  let connect ?ssl address port =
    fail (Failure "not implemented")

  let close_in ic = ()
  let close_out ic = ()
  let close ic oc = ignore_result (Channel.close ic)
end

module Request = Request.Make(IO)
module Response = Response.Make(IO)
module Body = Cohttp_lwt_body
module Client = Cohttp_lwt.Client(Request)(Response)(Net_IO)
module Server = Cohttp_lwt.Server(Request)(Response)(Net_IO)

let listen ?timeout mgr src spec =
  (* TODO XXX the cancel-based timeout is almost certainly broken as the
   * thread wont issue a Response *)
  let cb = 
    match timeout with 
    |None -> 
      fun dst ch ->
        Server.callback spec ch ch
    |Some tm ->
      fun dst ch ->
        let tmout = OS.Time.sleep tm in
        let cb_t = Server.callback spec ch ch in
        tmout <?> cb_t
  in
  Net.Channel.listen mgr (`TCPv4 (src, cb))
