(*{{{ Copyright (c) 2012-2014 Anil Madhavapeddy <anil@recoil.org>
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
  }}}*)

(** HTTP/1.1 request handling *)

(** This contains the metadata for a HTTP/1.1 request header, including
    the {!headers}, {!version}, {!meth} and {!uri}.  The body is handled by
    the separate {!S} module type, as it is dependent on the IO
    implementation.

    The interface exposes a [fieldslib] interface which provides individual
    accessor functions for each of the records below.  It also provides [sexp]
    serializers to convert to-and-from an {!Core.Std.Sexp.t}. *)

type t = {
  headers: Header.t;    (** HTTP request headers *)
  meth: Code.meth;      (** HTTP request method *)
  uri: Uri.t;           (** Full HTTP request uri *)
  version: Code.version; (** HTTP version, usually 1.1 *)
  encoding: Transfer.encoding; (** transfer encoding of this HTTP request *)
} with fields, sexp

val make : ?meth:Code.meth -> ?version:Code.version ->
  ?encoding:Transfer.encoding -> ?headers:Header.t ->
  Uri.t -> t
(** Return true whether the connection should be reused *)
val is_keep_alive : t -> bool

val make_for_client:
  ?headers:Header.t ->
  ?chunked:bool ->
  ?body_length:int64 ->
  Code.meth -> Uri.t -> t

(** Human-readable output, used by the toplevel printer *)
val pp_hum : Format.formatter -> t -> unit

val prepare : t -> t

val has_body : t -> [`Yes | `No | `Unknown]

val to_string_list : t -> string list

val of_string_list : string list -> [`Ok of t | `Error of string]
