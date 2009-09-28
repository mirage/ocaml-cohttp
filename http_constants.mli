
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

(** Constants *)

  (** default HTTP version *)
val version: Http_types.version

  (** string returned as value of "Server:" response header *)
val server_string: string

  (** "\r\n" string *)
val crlf: string

  (** {2 daemon default values} *)

val default_addr: string
val default_auth: (string * Http_types.auth_info) option
val default_auto_close: bool
val default_callback: Http_types.request -> out_channel -> unit
val default_mode: Http_types.daemon_mode
val default_port: int
val default_root_dir: string option
val default_exn_handler: (exn -> out_channel -> unit) option
val default_timeout: int option

