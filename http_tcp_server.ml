
(*
  OCaml HTTP - do it yourself (fully OCaml) HTTP daemon

  Copyright (C) <2002-2005> Stefano Zacchiroli <zack@cs.unibo.it>

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU Library General Public License as
  published by the Free Software Foundation, version 2.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
  USA
*)

open Http_common
open Lwt

  (** raised when a client timeouts *)
exception Timeout

let backlog = 10

  (** if timeout is given (Some _) @return a new callback which establish
  timeout_callback as callback for signal Sys.sigalrm and register an alarm
  (expiring after timeout seconds) before invoking the real callback given. If
  timeout is None, callback is returned unchanged. *)
let wrap_callback_w_timeout ~callback ~timeout ~timeout_callback =
  match timeout with
  | None -> callback
  | Some timeout -> (* wrap callback setting an handler for ALRM signal and an
                    alarm that ring after timeout seconds *)
      (fun (inchan:Lwt_io.input_channel) (outchan:Lwt_io.output_channel) ->
        (* XXX register Lwt alarm here *)
        callback inchan outchan)

let try_close chan =
  catch (fun () -> Lwt_io.close chan)
  (function |_ -> return ())

let init_socket sockaddr =
  let suck = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    (* shutdown socket on SIGTERM *)
  Lwt_unix.setsockopt suck Unix.SO_REUSEADDR true;
  Lwt_unix.bind suck sockaddr;
  Lwt_unix.listen suck backlog;
  suck

let simple ~sockaddr ~timeout callback =
  let suck = init_socket sockaddr in
  let rec handle_connection () =
    Lwt_unix.accept suck >>= fun (client,_) ->
    debug_print "accepted connection";
    (* client is now connected *)
    let inchan = Lwt_io.of_fd Lwt_io.input client in
    let outchan = Lwt_io.of_fd Lwt_io.output client in
 
    let clisockaddr = Unix.getpeername (Lwt_unix.unix_file_descr client) in
    let srvsockaddr = Unix.getsockname (Lwt_unix.unix_file_descr client) in

    debug_print "callback start";
    callback ~clisockaddr ~srvsockaddr inchan outchan >>= fun () ->
    debug_print "callback end";
    try_close outchan >>= fun () ->
    try_close inchan 
  in
  handle_connection ()
