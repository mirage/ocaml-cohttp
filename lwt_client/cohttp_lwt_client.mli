(*
 * OCaml HTTP - do it yourself (fully OCaml) HTTP daemon
 *
 * Copyright (C) <2009-2012> Anil Madhavapeddy <anil@recoil.org>
 * Copyright (C) <2002-2005> Stefano Zacchiroli <zack@cs.unibo.it>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Library General Public License as
 * published by the Free Software Foundation, version 2.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
 * USA
 *)

type headers = (string * string) list

type request_body = [
  | `InChannel of int * Lwt_io.input_channel
  | `None
  | `String of string 
]

type tcp_error_source = Connect | Read | Write
exception Tcp_error of tcp_error_source * exn
exception Http_error of (int * headers * string)

val head : ?headers:headers -> Uri.t -> (headers * string) Lwt.t
val get : ?headers:headers -> Uri.t -> (headers * string) Lwt.t
val post : ?headers:headers -> ?body:request_body -> Uri.t -> (headers * string) Lwt.t
val put : ?headers:headers -> ?body:request_body -> Uri.t -> (headers * string) Lwt.t
val delete : ?headers:headers -> Uri.t -> (headers * string) Lwt.t

val head_to_chan : ?headers:headers -> Uri.t -> Lwt_io.output_channel -> headers Lwt.t
val get_to_chan : ?headers:headers -> Uri.t -> Lwt_io.output_channel -> headers Lwt.t
val post_to_chan : ?headers:headers -> ?body:request_body -> Uri.t -> Lwt_io.output_channel -> headers Lwt.t
val put_to_chan : ?headers:headers -> ?body:request_body -> Uri.t -> Lwt_io.output_channel -> headers Lwt.t
val delete_to_chan : ?headers:headers -> Uri.t -> Lwt_io.output_channel -> headers Lwt.t
