(*{{{ Copyright (c) 2012-2014 Anil Madhavapeddy <anil@recoil.org>
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
  }}}*)

module Input_channel = struct
  open Lwt.Infix
  module Bytebuffer = Cohttp_lwt.Private.Bytebuffer

  type t = { buf : Bytebuffer.t; chan : Lwt_io.input_channel }

  let refill ic buf ~pos ~len =
    Lwt.catch
      (fun () ->
        Lwt_io.read_into ic buf pos len >|= fun c ->
        if c > 0 then `Ok c else `Eof)
      (function Lwt_io.Channel_closed _ -> Lwt.return `Eof | exn -> raise exn)

  let create ?(buf_len = 0x4000) chan =
    { buf = Bytebuffer.create buf_len; chan }

  let read_line_opt t = Bytebuffer.read_line t.buf (refill t.chan)
  let read t count = Bytebuffer.read t.buf (refill t.chan) count
  let refill t = Bytebuffer.refill t.buf (refill t.chan)

  let with_input_buffer t ~f =
    let buf = Bytebuffer.unsafe_buf t.buf in
    let pos = Bytebuffer.pos t.buf in
    let len = Bytebuffer.length t.buf in
    let res, consumed = f (Bytes.unsafe_to_string buf) ~pos ~len in
    Bytebuffer.drop t.buf consumed;
    res

  let close t = Lwt_io.close t.chan
end

module Io = struct
  exception IO_error of exn

  let () =
    Printexc.register_printer (function
      | IO_error e -> Some ("IO error: " ^ Printexc.to_string e)
      | _ -> None);
    if Sys.os_type <> "Win32" then Sys.(set_signal sigpipe Signal_ignore)

  type 'a t = 'a Lwt.t

  let ( >>= ) = Lwt.bind
  let return = Lwt.return

  type ic = Input_channel.t
  type oc = Lwt_io.output_channel
  type conn = unit

  let wrap_read f ~if_closed =
    (* TODO Use [Lwt_io.is_closed] when available:
       https://github.com/ocsigen/lwt/pull/635 *)
    Lwt.catch f (function
      | Lwt_io.Channel_closed _ -> Lwt.return if_closed
      | Unix.Unix_error _ as e -> Lwt.fail (IO_error e)
      | exn -> raise exn)

  let wrap_write f =
    Lwt.catch f (function
      | Unix.Unix_error _ as e -> Lwt.fail (IO_error e)
      | exn -> raise exn)

  let read_line ic =
    wrap_read ~if_closed:None (fun () ->
        Input_channel.read_line_opt ic >>= function
        | None -> Lwt.return_none
        | Some _ as x -> Lwt.return x)

  let read ic count =
    let count = min count Sys.max_string_length in
    wrap_read ~if_closed:"" (fun () ->
        Input_channel.read ic count >>= fun buf -> Lwt.return buf)

  let refill ic = Input_channel.refill ic
  let with_input_buffer ic = Input_channel.with_input_buffer ic
  let write oc buf = wrap_write @@ fun () -> Lwt_io.write oc buf
  let flush oc = wrap_write @@ fun () -> Lwt_io.flush oc

  type error = exn

  let catch f =
    Lwt.try_bind f Lwt.return_ok (function
      | IO_error e -> Lwt.return_error e
      | ex -> Lwt.fail ex)

  let pp_error fmt exn = Format.fprintf fmt "%s" (Printexc.to_string exn)
end

include Cohttp_lwt.Make_server (Io)

let handle_connection t (ic, oc) =
  let ic = Input_channel.create ic in
  Lwt.finalize
    (fun () -> callback t () ic oc)
    (fun () -> Input_channel.close ic)
