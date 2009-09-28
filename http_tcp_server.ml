
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
      (fun inchan outchan ->
        ignore (Sys.signal Sys.sigalrm (Sys.Signal_handle timeout_callback));
        ignore (Unix.alarm timeout);
        callback inchan outchan)

  (* try to close nicely a socket *)
let shutdown_socket suck =
  try
    Unix.shutdown suck Unix.SHUTDOWN_ALL
  with Unix.Unix_error(_, "shutdown", "") -> ()

let nice_unix_accept suck =
  try
    Unix.accept suck
  with e -> (* clean up socket before exit *)
    shutdown_socket suck;
    raise e

let init_socket sockaddr =
  let suck = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    (* shutdown socket on SIGTERM *)
  ignore (Sys.signal Sys.sigterm
    (Sys.Signal_handle
      (fun _ -> shutdown_socket suck; exit 17)));
  Unix.setsockopt suck Unix.SO_REUSEADDR true;
  Unix.bind suck sockaddr;
  Unix.listen suck backlog;
  suck

let init_callback callback timeout =
  let timeout_callback signo =
    if signo = Sys.sigalrm then
      raise Timeout
  in
  wrap_callback_w_timeout ~callback ~timeout ~timeout_callback

  (** try to close an outchannel connected to a socket, ignore Sys_error since
  * this probably means that socket is already closed (e.g. on sigpipe) *)
let try_close_out ch = try close_out ch with Sys_error _ -> ()

  (** like Unix.establish_server, but shutdown sockets when receiving SIGTERM
  and before exiting for an uncaught exception *)
let my_establish_server server_fun sockaddr =
  let suck = init_socket sockaddr in
  while true do
    let (s, caller) = nice_unix_accept suck in
      (** "double fork" trick, see {!Unix.establish_server} implementation *)
    match Unix.fork() with
    | 0 ->  (* parent *)
        (try
          if Unix.fork () <> 0 then
            exit 0;  (* The son exits, the grandson works *)
          let inchan = Unix.in_channel_of_descr s in
          let outchan = Unix.out_channel_of_descr s in
          server_fun inchan outchan;
          try_close_out outchan;  (* closes also inchan: socket is the same *)
          exit 0
        with e ->
          shutdown_socket suck; (* clean up socket before exit *)
          raise e)
    | child when (child > 0) -> (* child *)
        Unix.close s;
        ignore (Unix.waitpid [] child) (* Reclaim the son *)
    | _ (* < 0 *) ->
        failwith "Can't fork"
  done

  (** tcp_server which forks a new process for each request *)
let fork ~sockaddr ~timeout callback =
  let timeout_callback signo =
    if signo = Sys.sigalrm then
      exit 2
  in
  my_establish_server
    (wrap_callback_w_timeout ~callback ~timeout ~timeout_callback)
    sockaddr

  (** tcp_server which doesn't fork, requests are server sequentially and in the
  same address space of the calling process *)
let simple ~sockaddr ~timeout callback =
  let suck = init_socket sockaddr in
  let callback = init_callback callback timeout in
  try
    while true do
      let (client, _) = Unix.accept suck in
        (* client is now connected *)
      let (inchan, outchan) =
        (Unix.in_channel_of_descr client, Unix.out_channel_of_descr client)
      in
      (try
        callback inchan outchan;
        ignore (Unix.alarm 0) (* reset alarm *)
      with Timeout -> ());
      try_close_out outchan (* this close also inchan: socket is the same *)
    done
  with e -> (* clean up socket before exit *)
    shutdown_socket suck;
    raise e

  (** tcp_server which creates a new thread for each request to be served *)
let thread ~sockaddr ~timeout callback =
  let suck = init_socket sockaddr in
  let callback = init_callback callback timeout in
  let callback (i, o) =
    (try
      callback i o
    with
    | Timeout -> ()
    | e ->
        try_close_out o;
        raise e);
    try_close_out o
  in
  while true do
    let (client, _) = nice_unix_accept suck in
      (* client is now connected *)
    let (inchan, outchan) =
      (Unix.in_channel_of_descr client, Unix.out_channel_of_descr client)
    in
    Http_threaded_tcp_server.serve callback (inchan, outchan)
  done

  (** @param server an Http_types.tcp_server
  * @return an Http_types.tcp_server which takes care of ignoring SIGPIPE during
  * server execution and restoring previous handler when (if ever) the server
  * returns *)
let handle_sigpipe server =
  fun ~sockaddr ~timeout callback ->
    let old_sigpipe_behavior = Sys.signal Sys.sigpipe Sys.Signal_ignore in
    server ~sockaddr ~timeout callback;
    ignore (Sys.signal Sys.sigpipe old_sigpipe_behavior)

let simple = handle_sigpipe simple
let thread = handle_sigpipe thread
let fork = handle_sigpipe fork

