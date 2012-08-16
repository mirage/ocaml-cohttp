
(*
  OCaml HTTP - do it yourself (fully OCaml) HTTP daemon

  Copyright (C) <2011-2012> Anil Madhavapeddy <anil@recoil.org>
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

(** HTTP messages parsing *)

(** parse content-range header in a request
  @return number of bytes to read, or None if all available should be read
 *)
val parse_content_range: Header.t -> int option

(** parse the media type portion of a header, e.g. foo/bar from "foo/bar ; charset=UTF-8" *)
val parse_media_type: string -> string option
