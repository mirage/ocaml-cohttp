
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

(** Helpers and other not better classified functions which should not be
exposed in the final API *)

  (** @return the current date compliant to RFC 1123, which updates RFC 822
  zone info are retrieved from UTC *)
val date_822: unit -> string

  (** @return true if 'name' is a directory on the file system, false otherwise
  *)
val is_directory: string -> bool

  (** @return the filesize of fname *)
val filesize: string -> int

  (** strip trailing '/', if any, from a string and @return the new string *)
val strip_trailing_slash: string -> string

  (** strip heading '/', if any, from a string and @return the new string *)
val strip_heading_slash: string -> string

  (** given a dir handle @return a list of entries contained *)
val ls: Unix.dir_handle -> string list

  (** explode a string in a char list *)
val string_explode: string -> char list

  (** implode a char list in a string *)
val string_implode: char list -> string

  (** given an HTTP response code return the corresponding reason phrase *)
val reason_phrase_of_code: int -> string

  (** build a Unix.sockaddr inet address from a string representation of an IP
  address and a port number *)
val build_sockaddr: string * int -> Unix.sockaddr

  (** explode an _inet_ Unix.sockaddr address in a string representation of an
  IP address and a port number *)
val explode_sockaddr: Unix.sockaddr -> string * int

  (** given an out_channel build on top of a socket, return peername related to
  that socket *)
val peername_of_out_channel: out_channel -> Unix.sockaddr

  (** as above but works on in_channels *)
val peername_of_in_channel: in_channel -> Unix.sockaddr

  (** given an out_channel build on top of a socket, return sockname related to
  that socket *)
val sockname_of_out_channel: out_channel -> Unix.sockaddr

  (** as above but works on in_channels *)
val sockname_of_in_channel: in_channel -> Unix.sockaddr

  (* TODO replace with Buffer.add_channel which does almost the same :-((( *)
  (** reads from an input channel till it End_of_file and returns what has been
  read; if limit is given returned buffer will contains at most first 'limit'
  bytes read from input channel *)
val buf_of_inchan: ?limit: int -> in_channel -> Buffer.t

  (** like List.assoc but return all bindings of a given key instead of the
  leftmost one only *)
val list_assoc_all: 'a -> ('a * 'b) list -> 'b list

val warn: string -> unit (** print a warning msg to stderr. Adds trailing \n *)
val error: string -> unit (** print an error msg to stderr. Adds trailing \n *)

  (** @param finalizer finalization function (execution both in case of success
   * and in case of raised exception
   * @param f function to be invoked
   * @param arg argument to be passed to function *)
val finally: (unit -> unit) -> ('a -> 'b) -> 'a -> 'b

