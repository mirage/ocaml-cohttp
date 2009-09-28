
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

let version = `HTTP_1_1 ;;
let server_string = "OCaml HTTP Daemon" ;;
let crlf = "\r\n" ;;

let default_addr = "0.0.0.0"
let default_auth = None
let default_auto_close = false
let default_callback = fun _ _ -> ()
let default_mode = `Fork
let default_port = 80
let default_root_dir = None
let default_exn_handler = Some (fun exn outchan -> ())
let default_timeout = Some 300


