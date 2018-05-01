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
 * %%NAME%% %%VERSION%%
 *)

open Lwt.Infix
open Result

module Make (Channel: Mirage_channel_lwt.S) = struct

  type 'a t = 'a Lwt.t
  type ic = Channel.t
  type oc = Channel.t
  type conn = Channel.flow

  let failf fmt = Fmt.kstrf Lwt.fail_with fmt

  (* The HTTP spec does not define a maximum limit on the length
     of an HTTP header but we need one to avoid a malicious client
     exhausting all our memory.
     Typical values range from 4k to 48k according to
     https://stackoverflow.com/questions/686217/maximum-on-http-header-values
     *)
  let max_line_length = 4096

  let read_line ic =
    Channel.read_line ~len:max_line_length ic >>= function
    | Ok (`Data [])   -> Lwt.return_none
    | Ok `Eof         -> Lwt.return_none
    | Ok (`Data bufs) -> Lwt.return (Some (Cstruct.copyv bufs))
    | Error e         -> failf "Flow error: %a" Channel.pp_error e

  let read ic len =
    Channel.read_some ~len ic >>= function
    | Ok (`Data buf) -> Lwt.return (Cstruct.to_string buf)
    | Ok `Eof        -> Lwt.return ""
    | Error e        -> failf "Flow error: %a" Channel.pp_error e

  let write oc buf =
    Channel.write_string oc buf 0 (String.length buf);
    Channel.flush oc >>= function
    | Ok ()         -> Lwt.return_unit
    | Error `Closed -> Lwt.fail_with "Trying to write on closed channel"
    | Error e       -> failf "Flow error: %a" Channel.pp_write_error e

  let flush _ =
    (* NOOP since we flush in the normal writer functions above *)
    Lwt.return_unit

  let  (>>= ) = Lwt.( >>= )
  let return = Lwt.return

end
