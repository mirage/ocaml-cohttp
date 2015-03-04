(*
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazazagnaire.org>
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

module Make(Channel:V1_LWT.CHANNEL) = struct

  type 'a t = 'a Lwt.t
  let (>>=) = Lwt.bind
  let return = Lwt.return

  type ic = Channel.t
  type oc = Channel.t
  type conn = Channel.flow

  let iter fn x = Lwt_list.iter_s fn x

  let read_line ic =
    Channel.read_line ic >>= function
    | [] -> return None
    | bufs -> return (Some (Cstruct.copyv bufs))

  let read ic len =
    Lwt.catch
      (fun () ->
         Channel.read_some ~len ic >>= fun iop ->
         return (Cstruct.to_string iop))
      (function End_of_file -> return "" | e -> Lwt.fail e)

  let read_exactly ic buf off len =
    let rec read acc left =
      match left with
      | 0   -> return (List.rev acc)
      | len ->
        Channel.read_some ~len ic >>= fun iop ->
        read (iop::acc) (left - (Cstruct.len iop))
    in
    read [] len >>= fun iov ->
    (* XXX TODO this is hyper slow! *)
    let srcbuf = Cstruct.copyv iov in
    Bytes.blit srcbuf 0 buf off (String.length srcbuf);
    return true

  let read_exactly ic len =
    let buf = Bytes.create len in
    read_exactly ic buf 0 len >>= function
    | true -> return (Some buf)
    | false -> return None

  let write oc buf =
    Channel.write_string oc buf 0 (String.length buf);
    Channel.flush oc

  let write_line oc buf =
    Channel.write_line oc buf;
    Channel.flush oc

  let flush oc =
    (* NOOP since we flush in the normal writer functions above *)
    return ()
end
