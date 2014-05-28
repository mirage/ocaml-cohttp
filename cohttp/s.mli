(*
 * Copyright (C) 2012-2014 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2014 Rudi Grinberg
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
  type conn

  val iter : ('a -> unit t) -> 'a list -> unit t
  val read_line : ic -> string option t
  val read : ic -> int -> string t
  val read_exactly : ic -> int -> string option t

  val write : oc -> string -> unit t
  val flush : oc -> unit t
end

module type Http_io = sig
  type t
  module IO : IO

  val read : IO.ic -> [ `Eof | `Invalid of string | `Ok of t ] IO.t
  val has_body : t -> [ `No | `Unknown | `Yes ]
  val read_body_chunk : t -> IO.ic -> Transfer.chunk IO.t

  val is_form: t -> bool
  val read_form : t -> IO.ic -> (string * string list) list IO.t

  val write_header : t -> IO.oc -> unit IO.t
  val write_body : t -> IO.oc -> string -> unit IO.t
  val write_footer : t -> IO.oc -> unit IO.t
  val write : (t -> IO.oc -> unit IO.t) -> t -> IO.oc -> unit IO.t
end

module type Request = sig
  type t = {
    mutable headers: Header.t;    (** HTTP request headers *)
    mutable meth: Code.meth;      (** HTTP request method *)
    mutable uri: Uri.t;           (** Full HTTP request uri *)
    mutable version: Code.version; (** HTTP version, usually 1.1 *)
    mutable encoding: Transfer.encoding; (** transfer encoding of this HTTP request *)
  } with fields, sexp

  val make : ?meth:Code.meth -> ?version:Code.version -> 
    ?encoding:Transfer.encoding -> ?headers:Header.t ->
    Uri.t -> t
  (** Return true whether the connection should be reused *)
  val is_keep_alive : t -> bool

  val make_for_client:
    ?headers:Header.t ->
    ?chunked:bool ->
    ?body_length:int ->
    Code.meth -> Uri.t -> t
end

module type Response = sig
  type t = {
    mutable encoding: Transfer.encoding; (** Transfer encoding of this HTTP response *)
    mutable headers: Header.t;    (** response HTTP headers *)
    mutable version: Code.version; (** (** HTTP version, usually 1.1 *) *)
    mutable status: Code.status_code; (** HTTP status code of the response *)
    mutable flush: bool;
  } with fields, sexp

  val make :
    ?version:Code.version -> 
    ?status:Code.status_code ->
    ?flush:bool ->
    ?encoding:Transfer.encoding -> 
    ?headers:Header.t -> 
    unit -> t
end

module type Body = sig
  type t
  val to_string : t -> string
  val empty : t
  val of_string : string -> t
  val of_string_list : string list -> t
  val map : t -> f:(string -> string) -> t
  val transfer_encoding : t -> Transfer.encoding
end
