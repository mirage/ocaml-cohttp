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

module IO : sig
  type 'a t = 'a Lwt.t
  val (>>=) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
  val return : 'a -> 'a Lwt.t
  type ic = Lwt_io.input_channel
  type oc = Lwt_io.output_channel
  type buf = Lwt_bytes.t
  val iter : ('a -> unit Lwt.t) -> 'a list -> unit Lwt.t
  val read_line : Lwt_io.input_channel -> string option Lwt.t
  val read : Lwt_io.input_channel -> int -> string Lwt.t
  val read_exactly : Lwt_io.input_channel -> string -> int -> int -> bool Lwt.t
  val write : Lwt_io.output_channel -> string -> unit Lwt.t
  val write_line : Lwt_io.output_channel -> string -> unit Lwt.t
  val ic_of_buffer : Lwt_bytes.t -> Lwt_io.input Lwt_io.channel
  val oc_of_buffer : Lwt_bytes.t -> Lwt_io.output Lwt_io.channel
end

module Parser : sig
  val parse_request_fst_line : IO.ic -> (Code.meth * Uri.t * Code.version) option IO.t
  val parse_response_fst_line : IO.ic -> (Code.version * Code.status_code) option IO.t
  val parse_headers : IO.ic -> Header.t IO.t
  val parse_content_range : Header.t -> int option
  val parse_media_type : string -> string option
end

module Body : sig
  val read : Transfer.encoding -> IO.ic -> string option IO.t
  val write : Transfer.encoding -> IO.oc -> string -> unit IO.t
end

module Request : sig
  type request
  val body : request -> IO.ic -> string option IO.t
  val meth : request -> Code.meth
  val uri : request -> Uri.t
  val version : request -> Code.version
  val path : request -> string
  val header : request -> string -> string list
  val params_get : request -> Header.t
  val params_post : request -> Header.t
  val param : request -> string -> string list
  val transfer_encoding : request -> string

  val make : ?meth:Code.meth -> ?version:Code.version -> 
    ?encoding:Transfer.encoding -> Header.t -> Uri.t -> request

  val read : IO.ic -> request option IO.t

  val write_header : request -> IO.oc -> unit IO.t
  val write_body : string -> request -> IO.oc -> unit IO.t
  val write_footer : request -> IO.oc -> unit IO.t
  val write : (request -> IO.oc -> unit IO.t) -> request -> IO.oc -> unit IO.t
end

module Response : sig
  type response
  val version : response -> Code.version
  val status : response -> Code.status_code
  val body : response -> string option IO.t

  val read : IO.ic -> response option IO.t
end
