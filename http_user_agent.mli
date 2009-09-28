
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

(** Minimal implementation of an HTTP 1.0/1.1 client. Interface is similar to
 * Gerd Stoplmann's Http_client module. Implementation is simpler and doesn't
 * handle HTTP redirection, proxies, ecc. The only reason for the existence of
 * this module is for performances and incremental elaboration of response's
 * bodies *)

open Http_types

exception Http_error of (int * string)  (* code, body *)

  (** @param head_callback optional calllback invoked on response's status and
   * headers. If not provided no callback will be invoked
   * @param url an HTTP url
   * @return HTTP response's body
   * @raise Http_error when response code <> 200 *)
val get:
  ?head_callback:(status -> (string * string) list -> unit) ->
  string ->
    string

  (** as above but iter callback function on HTTP response's body instead of
   * returning it as a string *)
val get_iter:
  ?head_callback:(status -> (string * string) list -> unit) ->
  (string -> unit) -> string ->
    unit

  (** @param url an HTTP url
   * @return HTTP HEAD raw response
   * @raise Http_error when response code <> 200 *)
val head: string -> string

