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

module M(IO:IO.M) : sig
  type response

  val version: response -> Code.version
  val status: response -> Code.status_code

  val make : ?version:Code.version -> ?status:Code.status_code -> 
    ?encoding:Transfer.encoding -> Header.t -> response

  val read: IO.ic -> response option IO.t
  val read_body: response -> IO.ic -> Transfer.chunk IO.t

  val write_header : response -> IO.oc -> unit IO.t
  val write_body : string -> response -> IO.oc -> unit IO.t
  val write_footer : response -> IO.oc -> unit IO.t
  val write : (response -> IO.oc -> unit IO.t) -> response -> IO.oc -> unit IO.t
end
