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

(** HTTP/1.1 response handling *)

(** This contains the metadata for a HTTP/1.1 response header, including
    the {!encoding}, {!headers}, {!version}, {!status} code and whether to
    {!flush} the connection after every body chunk (useful for server-side
    events and other long-lived connection protocols). The body is handled by
    the separate {!S} module type, as it is dependent on the IO 
    implementation. *)
type t = {
  mutable encoding: Transfer.encoding;
  mutable headers: Header.t;
  mutable version: Code.version;
  mutable status: Code.status_code;
  mutable flush: bool;
} with fields, sexp

(** Retrieve response HTTP headers *)
val headers : t -> Header.t

(** Retrieve the transfer encoding of this HTTP response *)
val encoding : t -> Transfer.encoding

(** Retrieve HTTP version, usually 1.1 *)
val version : t -> Code.version

(** Retrieve HTTP status code of the response *)
val status : t -> Code.status_code

val make :
  ?version:Code.version -> 
  ?status:Code.status_code ->
  ?flush:bool ->
  ?encoding:Transfer.encoding -> 
  ?headers:Header.t -> 
  unit -> t

module type S = sig
  module IO : IO.S

  val read : IO.ic -> t option IO.t
  val has_body : t -> bool
  val read_body_chunk : t -> IO.ic -> Transfer.chunk IO.t

  val is_form: t -> bool
  val read_form : t -> IO.ic -> (string * string list) list IO.t

  val write_header : t -> IO.oc -> unit IO.t
  val write_body : t -> IO.oc -> string -> unit IO.t
  val write_footer : t -> IO.oc -> unit IO.t
  val write : (t -> IO.oc -> unit IO.t) -> t -> IO.oc -> unit IO.t
end

module Make(IO : IO.S) : S with module IO = IO
