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

type r = { 
  headers: Header.t;
  meth: Code.meth;
  uri: Uri.t;
  version: Code.version;
  encoding: Transfer.encoding;
}

val make : ?meth:Code.meth -> ?version:Code.version -> 
  ?encoding:Transfer.encoding -> ?headers:Header.t ->
  Uri.t -> r

val make_for_client:
  ?headers:Header.t ->
  ?chunked:bool ->
  ?body_length:int ->
  Code.meth -> Uri.t -> r

module type S = sig
  module IO : IO.S
  type t = r
  val meth : t -> Code.meth
  val uri : t -> Uri.t
  val version : t -> Code.version

  val path : t -> string
  val header : t -> string -> string option
  val headers : t -> Header.t

  val params : t -> (string * string list) list
  val get_param : t -> string -> string option

  val transfer_encoding : t -> string

  val read : IO.ic -> t option IO.t
  val has_body : t -> bool
  val read_body_chunk :
    t -> IO.ic -> Transfer.chunk IO.t

  val write_header : t -> IO.oc -> unit IO.t
  val write_body : t -> IO.oc -> string -> unit IO.t
  val write_footer : t -> IO.oc -> unit IO.t
  val write' : t -> (unit -> string option) -> IO.oc -> unit IO.t
  val write : (t -> IO.oc -> unit IO.t) -> t -> IO.oc -> unit IO.t

  val is_form: t -> bool
  val read_form : t -> IO.ic -> (string * string list) list IO.t

  val make : ?meth:Code.meth -> ?version:Code.version -> 
    ?encoding:Transfer.encoding -> ?headers:Header.t -> Uri.t -> r
end

module Make(IO : IO.S) : S with module IO = IO
