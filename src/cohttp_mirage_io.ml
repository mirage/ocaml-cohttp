(*
 * Copyright (c) 2012-2015 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazazagnaire.org>
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

open Lwt.Infix

module Make(Channel:V1_LWT.CHANNEL) = struct

  type 'a t = 'a Lwt.t
  type ic = Channel.t
  type oc = Channel.t
  type conn = Channel.flow

  let read_line ic =
    Channel.read_line ic >>= function
    | []   -> Lwt.return_none
    | bufs -> Lwt.return (Some (Cstruct.copyv bufs))

  let read ic len =
    Lwt.catch
      (fun () ->
         Channel.read_some ~len ic >>= fun iop ->
         Lwt.return (Cstruct.to_string iop))
      (function End_of_file -> Lwt.return "" | e -> Lwt.fail e)

  let write oc buf =
    Channel.write_string oc buf 0 (String.length buf);
    Channel.flush oc

  let flush _ =
    (* NOOP since we flush in the normal writer functions above *)
    Lwt.return_unit

  let  (>>= ) = Lwt.( >>= )
  let return = Lwt.return

end
