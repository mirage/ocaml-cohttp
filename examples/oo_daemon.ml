
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

open Http_daemon
open Http_response

  (* the simple way *)
let d = new daemon ~addr:"127.0.0.1" ~port:9999 ()

let _ =
  while true do
    let (req, conn) = d#getRequest in (* wait for valid request *)
    conn#respond_with (new response ~body:"foo\n" ());
    conn#close
  done

(*
  (* the hard^Wother way *)
let d = new daemon ~addr:"127.0.0.1" ~port:9999 () in
let _ =
  while true do
    let conn = d#accept in (* wait for client connection *)
    (match conn#getRequest with
    | None -> ()  (* invalid request received *)
    | Some req -> conn#respond_with (new response ~body:"foo\n" ()));
    conn#close (* close socket *)
  done
*)

