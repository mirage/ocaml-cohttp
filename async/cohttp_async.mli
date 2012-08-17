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

module Request : sig
  type request
  val meth : request -> Code.meth
  val uri : request -> Uri.t
  val version : request -> Code.version
  val path : request -> string
  val header : request -> string -> string option
  val headers : request -> Header.t
  val params_get : request -> Header.t
  val params_post : request -> Header.t
  val param : request -> string -> string list
  val transfer_encoding : request -> string
  val make : ?meth:Code.meth -> ?version:Code.version ->
    ?encoding:Transfer.encoding -> ?headers:Header.t -> Uri.t -> request
end

module Response : sig
  type response
  val version : response -> Code.version
  val status : response -> Code.status_code
  val headers : response -> Header.t
  val make : ?version:Code.version -> ?status:Code.status_code ->
    ?encoding:Transfer.encoding -> ?headers:Header.t -> unit -> response
end

module Client : sig
  type response = Response.response * string Async_core.Pipe.Reader.t option
  val call : ?headers:Header.t -> ?body:string Async_core.Pipe.Reader.t ->
    Code.meth -> Uri.t -> response option Async_core.Deferred.t
end
