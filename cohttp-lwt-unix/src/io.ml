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

exception IO_error of exn

let () =
  Printexc.register_printer (function
    | IO_error e -> Some ("IO error: " ^ Printexc.to_string e)
    | _ -> None);
  if Sys.os_type <> "Win32" then Sys.(set_signal sigpipe Signal_ignore)

type 'a t = 'a Lwt.t

let ( >>= ) = Lwt.bind
let return = Lwt.return

type ic = Lwt_io.input_channel
type oc = Lwt_io.output_channel
type conn = Conduit_lwt_unix.flow

let src = Logs.Src.create "cohttp.lwt.io" ~doc:"Cohttp Lwt IO module"

module Log = (val Logs.src_log src : Logs.LOG)

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
      Lwt_io.read_line_opt ic >>= function
      | None ->
          Log.debug (fun f -> f "<<< EOF");
          Lwt.return_none
      | Some l as x ->
          Log.debug (fun f -> f "<<< %s" l);
          Lwt.return x)

let read ic count =
  let count = min count Sys.max_string_length in
  wrap_read ~if_closed:"" (fun () ->
      Lwt_io.read ~count ic >>= fun buf ->
      Log.debug (fun f -> f "<<<[%d] %s" count buf);
      Lwt.return buf)

let write oc buf =
  wrap_write @@ fun () ->
  Log.debug (fun f -> f ">>> %s" (String.trim buf));
  Lwt_io.write oc buf

let flush oc = wrap_write @@ fun () -> Lwt_io.flush oc

type error = exn

let catch f =
  Lwt.try_bind f Lwt.return_ok (function
    | IO_error e -> Lwt.return_error e
    | ex -> Lwt.fail ex)

let pp_error = Fmt.exn

let wait_eof_or_closed conn ic sleep_fn =
  let open Lwt.Infix in
  let wait_for_cancel () =
    (* Wait till this promise is cancelled externally.
       We cannot return from the function as it will cancel
       another promise which is handling the request and works
       on the same input channel *)
    fst (Lwt.task ())
  in
  let bytes = Bytes.create 1 in
  let has_recv_eof fd =
    (* MSG_PEEK does not consume data from the stream and does not
       impact normal read operations *)
    Lwt_unix.recv fd bytes 0 1 Unix.[ MSG_PEEK ] >|= fun n -> n = 0
  in
  let rec loop fd =
    (* Calls [sleep_fn] to allow yielding control to the request handler *)
    sleep_fn () >>= fun () ->
    if Lwt_io.is_closed ic then
      (* the connection is closed locally, stop waiting for EOF *)
      wait_for_cancel ()
    else
      has_recv_eof fd >>= function
      | true -> Lwt.return_unit (* got EOF *)
      | false -> loop fd
  in
  match (conn : Conduit_lwt_unix.flow) with
  | Vchan _ -> wait_for_cancel ()
  | TCP { fd; _ } | Domain_socket { fd; _ } -> loop fd
