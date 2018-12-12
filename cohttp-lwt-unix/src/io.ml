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

let () =
  if Sys.os_type <> "Win32" then
    Sys.(set_signal sigpipe Signal_ignore);

type 'a t = 'a Lwt.t
let (>>=) = Lwt.bind
let return = Lwt.return

type ic = Lwt_io.input_channel
type oc = Lwt_io.output_channel
type conn = Conduit_lwt_unix.flow

let src = Logs.Src.create "cohttp.lwt.io" ~doc:"Cohttp Lwt IO module"
module Log = (val Logs.src_log src : Logs.LOG)

let with_closed_handler f ~if_closed =
  (* TODO Use [Lwt_io.is_closed] when available:
     https://github.com/ocsigen/lwt/pull/635 *)
  Lwt.catch f
    (function
    | Lwt_io.Channel_closed _ -> Lwt.return if_closed
    | exn -> raise exn
    )

let read_line ic =
  with_closed_handler ~if_closed:None
    (fun () ->
      Lwt_io.read_line_opt ic >>= function
      | None ->
        Log.debug (fun f -> f  "<<< EOF");
        Lwt.return_none
      | Some l as x ->
        Log.debug (fun f -> f  "<<< %s" l);
        Lwt.return x
    )

let read ic count =
  let count = min count Sys.max_string_length in
  with_closed_handler ~if_closed:""
    (fun () ->
      Lwt_io.read ~count ic >>= fun buf ->
      Log.debug (fun f -> f  "<<<[%d] %s" count buf);
      Lwt.return buf
    )

let write oc buf =
  Log.debug (fun f -> f  ">>> %s" (String.trim buf));
  Lwt_io.write oc buf

let flush oc =
  Lwt_io.flush oc
