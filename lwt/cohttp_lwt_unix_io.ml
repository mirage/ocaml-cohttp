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

let check_debug norm_fn debug_fn =
  try
    ignore(Sys.getenv "COHTTP_DEBUG");
    debug_fn
  with Not_found ->
    norm_fn

let () = Sys.(set_signal sigpipe Signal_ignore)

type 'a t = 'a Lwt.t
let (>>=) = Lwt.bind
let return = Lwt.return

type ic = Lwt_io.input_channel
type oc = Lwt_io.output_channel
type conn = Lwt_unix_conduit.conn

let iter fn x = Lwt_list.iter_s fn x

let read_line =
  check_debug
    (fun ic -> Lwt_io.read_line_opt ic)
    (fun ic ->
      match_lwt Lwt_io.read_line_opt ic with
      |None as x -> Printf.eprintf "%4d <<< EOF\n%!" (Unix.getpid ()); return x
      |Some l as x -> Printf.eprintf "%4d <<< %s\n%!" (Unix.getpid ()) l; return x)

let read =
 check_debug
   (fun ic count ->
     try_lwt Lwt_io.read ~count ic
     with End_of_file -> return "")
   (fun ic count ->
     lwt buf = 
       try_lwt Lwt_io.read ~count ic
       with End_of_file -> return "" in
     Printf.eprintf "%4d <<<[%d] %s%!" (Unix.getpid ()) count buf;
     return buf)

let read_exactly =
  check_debug
    (fun (ic : ic) buf off len ->
      try_lwt Lwt_io.read_into_exactly ic buf off len >>= fun () ->  return true
      with End_of_file -> return false)
   (fun ic buf off len ->
      lwt rd =
        try_lwt Lwt_io.read_into_exactly ic buf off len >>= fun () ->  return true
        with End_of_file -> return false in
      (match rd with
      |true -> Printf.eprintf "%4d <<< %S%!" (Unix.getpid ()) (String.sub buf off len)
      |false -> Printf.eprintf "%4d <<< <EOF>\n%!" (Unix.getpid ()));
      return rd)

let read_exactly ic len =
  let buf = String.create len in
  read_exactly ic buf 0 len >>= function
    | true -> return (Some buf)
    | false -> return None

let write =
  check_debug
    (fun oc buf -> Lwt_io.write oc buf)
    (fun oc buf -> Printf.eprintf "\n%4d >>> %s%!" (Unix.getpid ()) buf; Lwt_io.write oc buf)

let write_line =
  check_debug
    (fun oc buf -> Lwt_io.write_line oc buf)
    (fun oc buf -> Printf.eprintf "\n%4d >>> %s\n%!" (Unix.getpid ()) buf; Lwt_io.write_line oc buf)

let flush oc =
  Lwt_io.flush oc
