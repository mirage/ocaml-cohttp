(*
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

type t
val init : unit -> t
val init_with  : string -> string -> t
val add : t -> string -> string -> t
val add_opt : t option -> string -> string -> t
val remove : t -> string -> t
val get : t -> string -> string option
val get_multi : t -> string -> string list
val iter : (string -> string list -> unit) -> t -> unit
val map : (string -> string list -> string list) -> t -> t
val fold : (string -> string -> 'a -> 'a) -> t -> 'a -> 'a
val of_list : (string * string) list -> t
val to_list : t -> (string * string) list
val to_lines : t -> string list

val get_content_range : t -> int option
val get_media_type : t -> string option
val get_connection_close : t -> bool
val get_acceptable_media_ranges : t -> (Accept.media_range * Accept.p list) Accept.qlist
val get_acceptable_charsets : t -> Accept.charset Accept.qlist
val get_acceptable_encodings : t -> Accept.encoding Accept.qlist
val get_acceptable_languages : t -> Accept.language Accept.qlist
val get_transfer_encoding : t -> Transfer.encoding
val add_transfer_encoding : t -> Transfer.encoding -> t
val add_authorization : t -> Auth.t -> t
val get_authorization : t -> Auth.t option
val add_authorization_req : t -> Auth.req -> t
val is_form : t -> bool

