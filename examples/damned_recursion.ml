
(*
  OCaml HTTP - do it yourself (fully OCaml) HTTP daemon

  Copyright (C) <2002-2004> Stefano Zacchiroli <zack@cs.unibo.it>

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

open Printf
open Http_types

let port = 9999

let callback (req: Http_types.request) outchan =
  let i = int_of_string (req#param "x") in
  let body =
    match i with
    | 0 -> "0"
    | x when x > 0 ->
	let data =
          Http_user_agent.get (sprintf "http://127.0.0.1:%d/foo?x=%d"
				 port (x - 1))
	in
	sprintf "%s %d" data x
    | _ -> assert false
  in
  Http_daemon.respond ~code:(`Code 200) ~body outchan;
  close_out outchan  (* Http_user_agent relies on EOF, not Content-Length *)

let spec =
  { Http_daemon.default_spec with
      callback = callback;
      port = port;
      mode = `Thread;
  }

let _ = Http_daemon.main spec

