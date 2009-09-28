
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

(* test for fast rebinding of the tcp port *)

open Printf
open Http_types

let spec =
  { Http_daemon.default_spec with
      callback = (fun _ outchan -> Http_daemon.respond ~body:"foo" outchan);
      port = 9999;
      mode = `Single;
  }

let _ =
  Sys.catch_break true;
  while true do
    try
      Http_daemon.main spec;
    with Sys.Break -> prerr_endline "RESURRECTION!!!!"
  done

