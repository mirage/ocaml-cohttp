
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

(** Object Oriented representation of HTTP responses *)

open Http_types;;

  (** OO representation of an HTTP response. *)
class response:
  ?body:string -> ?headers:(string * string) list -> ?version: version ->
  ?clisockaddr: Unix.sockaddr -> ?srvsockaddr: Unix.sockaddr ->
  ?code:int -> ?status:Http_types.status ->
  unit ->
    Http_types.response

