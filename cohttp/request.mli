(*
 * Copyright (c) 2012-2013 Anil Madhavapeddy <anil@recoil.org>
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

(** HTTP/1.1 request handling *)

(** This contains the metadata for a HTTP/1.1 request header, including
    the {!headers}, {!version}, {!meth} and {!uri}.  The body is handled by
    the separate {!S} module type, as it is dependent on the IO
    implementation. *)
type t

(** Retrieve the HTTP request headers *)
val headers : t -> Header.t

(** Retrieve the HTTP request method *)
val meth : t -> Code.meth

(** Retrieve the full HTTP request uri *)
val uri : t -> Uri.t

(** Retrieve the HTTP version, usually 1.1 *)
val version : t -> Code.version

(** Retrieve the transfer encoding of this HTTP request *)
val encoding : t -> Transfer.encoding

val make : ?meth:Code.meth -> ?version:Code.version ->
  ?encoding:Transfer.encoding -> ?headers:Header.t ->
  Uri.t -> t

val make_for_client:
  ?headers:Header.t ->
  ?chunked:bool ->
  ?body_length:int ->
  Code.meth -> Uri.t -> t

module type S = sig
  module IO : IO.S

  val read : IO.ic -> t option IO.t
  val has_body : t -> bool
  val read_body_chunk : t -> IO.ic -> Transfer.chunk IO.t

  val write_header : t -> IO.oc -> unit IO.t
  val write_body : t -> IO.oc -> string -> unit IO.t
  val write_footer : t -> IO.oc -> unit IO.t
  val write : (t -> IO.oc -> unit IO.t) -> t -> IO.oc -> unit IO.t

  val is_form: t -> bool
  val read_form : t -> IO.ic -> (string * string list) list IO.t
end

module Make(IO : IO.S) : S with module IO := IO
