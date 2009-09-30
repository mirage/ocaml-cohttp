(*pp camlp4o -I `ocamlfind query lwt.syntax` pa_lwt.cmo *)

(*
  OCaml HTTP - do it yourself (fully OCaml) HTTP daemon

  Copyright (C) <2002-2005> Stefano Zacchiroli <zack@cs.unibo.it>
  Copyright (C) <2009> Anil Madhavapeddy <anil@recoil.org>

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
open Http_types
open Lwt

let backlog = 15

let try_close chan =
  catch (fun () -> Lwt_io.close chan)
  (function |_ -> return ())

let init_socket sockaddr =
  let suck = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt suck Unix.SO_REUSEADDR true;
  Lwt_unix.bind suck sockaddr;
  Lwt_unix.listen suck backlog;
  suck

let process_accept ~sockaddr ~timeout callback (client,_) =
  debug_print "accepted connection";
  (* client is now connected *)
  let inchan = Lwt_io.of_fd Lwt_io.input client in
  let outchan = Lwt_io.of_fd Lwt_io.output client in
 
  let clisockaddr = Unix.getpeername (Lwt_unix.unix_file_descr client) in
  let srvsockaddr = Unix.getsockname (Lwt_unix.unix_file_descr client) in

  let c = callback ~clisockaddr ~srvsockaddr inchan outchan in
  let events = match timeout with
    |None -> [c]
    |Some t -> [c; (Lwt_unix.sleep (float_of_int t) >> return ()) ] in
  Lwt.select events >> try_close outchan >> try_close inchan
  
let simple ~sockaddr ~timeout callback =
  let suck = init_socket sockaddr in
  let rec handle_connection () =
     lwt x = Lwt_unix.accept suck in
     let _ =  process_accept ~sockaddr ~timeout callback x in
     handle_connection()
  in
  handle_connection ()
