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

module type IO = sig
  type +'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t

  type ic
  type oc

  val iter : ('a -> unit t) -> 'a list -> unit t
  val read_line : ic -> string option t
  val read : ic -> int -> string t
  val read_exactly : ic -> string -> int -> int -> bool t

  val write : oc -> string -> unit t
end

module type BODY = sig
  module IO : IO
  val read : Transfer.encoding -> (string option -> unit IO.t) -> IO.ic -> unit IO.t
  val write : Transfer.encoding -> (unit -> string option) -> IO.oc -> unit IO.t
end

module type REQUEST = sig
  module IO : IO
  type t
  val meth : t -> Code.meth
  val uri : t -> Uri.t
  val version : t -> Code.version

  val path : t -> string
  val header : t -> string -> string option
  val headers : t -> Header.t

  val params : t -> (string * string list) list
  val get_param : t -> string -> string option

  val transfer_encoding : t -> string

  val make : ?meth:Code.meth -> ?version:Code.version -> 
    ?encoding:Transfer.encoding -> ?headers:Header.t ->
    ?body:'a -> Uri.t -> t

  val read : IO.ic -> t option IO.t
  val has_body : t -> bool
  val read_body : t -> (string option -> unit IO.t) -> IO.ic -> unit IO.t

  val write : t -> (unit -> string option) -> IO.oc -> unit IO.t
end

module type RESPONSE = sig
  module IO : IO
  type t

  val version : t -> Code.version
  val status : t -> Code.status_code
  val headers: t -> Header.t

  val make : ?version:Code.version -> ?status:Code.status_code ->
    ?encoding:Transfer.encoding -> ?headers:Header.t -> unit -> t

  val read : IO.ic -> t option IO.t
  val has_body : t -> bool
  val read_body : t -> (string option -> unit IO.t) -> IO.ic -> unit IO.t

  val write : t -> (unit -> string option) -> IO.oc -> unit IO.t
end
