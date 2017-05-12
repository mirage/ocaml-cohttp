(*{{{ Copyright (c) 2012-2013 Anil Madhavapeddy <anil@recoil.org>
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

open Core.Std
open Async.Std

let log_src_name = "cohttp.async.io"
let src = Logs.Src.create log_src_name ~doc:"Cohttp Async IO module"
module Log = (val Logs.src_log src : Logs.LOG)

let default_reporter () =
  let fmtr, fmtr_flush =
    let b = Buffer.create 512 in
    ( Fmt.with_buffer ~like:Fmt.stderr b
    , fun () ->
      let m = Buffer.contents b in
      Buffer.reset b;
      m ) in
  let report src level ~over k msgf =
    let k _ =
      if Logs.Src.name src = log_src_name then (
        Writer.write (Lazy.force Writer.stderr) (fmtr_flush ())
      );
      over ();
      k () in
    msgf @@ fun ?header ?tags fmt ->
    Format.kfprintf k fmtr ("@[" ^^ fmt ^^ "@]@.")
  in
  { Logs.report }

let set_log = lazy (
  (* If no reporter has been set by the application, set default one
     that prints to stderr. This way a user will see logs when the debug
     flag is set without adding a reporter. *)
  if phys_equal (Logs.reporter ()) Logs.nop_reporter
  then
    Logs.set_level @@ Some Logs.Debug;
    Logs.set_reporter (default_reporter ());
)

let check_debug norm_fn debug_fn =
  match Sys.getenv "COHTTP_DEBUG" with
  | Some _ ->
    Lazy.force set_log; debug_fn
  | None -> norm_fn

type 'a t = 'a Deferred.t
let (>>=) = Deferred.(>>=)
let return = Deferred.return

type ic = Reader.t
type oc = Writer.t
type conn = unit

let mutex = Mvar.create ()
let mutex_ro = Mvar.read_only mutex

let read_line ic =
  let read_line =
    check_debug
    (fun ic ->
       Reader.read_line ic
       >>| function
       | `Ok s -> Some s
       | `Eof -> None
    )
    (fun ic ->
       Reader.read_line ic
       >>| function
       | `Ok s -> Log.debug (fun fmt -> fmt "<<< %s" s); Some s
       | `Eof -> Log.debug (fun fmt -> fmt "<<<EOF"); None
    )
  in
  Mvar.put mutex () >>= fun () ->
  Monitor.protect (fun () -> read_line ic) ~finally:(fun () -> Mvar.take mutex_ro)

let bufsize = 0x8000
let bs = Bigstring.create bufsize

let read ic len =
  let buf = Bigbuffer.create bufsize in
  let rec inner len =
    if len <= 0 then
      return @@ Bigbuffer.contents buf
    else
      let bss = Bigsubstring.create bs ~pos:0 ~len:(Int.min len bufsize) in
      Reader.read_bigsubstring ic bss >>= function
      | `Ok len' ->
        Bigbuffer.add_bigstring buf (Bigstring.sub_shared bs ~pos:0 ~len:len');
        inner (len - len')
      | `Eof ->
        return @@ Bigbuffer.contents buf
  in
  Mvar.put mutex () >>= fun () ->
  Monitor.protect (fun () -> inner len) ~finally:(fun () -> Mvar.take mutex_ro)

let write =
  check_debug
    (fun oc buf ->
       Writer.write oc buf;
       return ())
    (fun oc buf ->
       Log.debug
         (fun fmt -> fmt "%4d >>> %s" (Pid.to_int (Unix.getpid ())) buf);
       Writer.write oc buf;
       return ())

let flush = Writer.flushed
