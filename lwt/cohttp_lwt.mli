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

val ic_of_buffer : Lwt_bytes.t -> Lwt_io.input_channel
val oc_of_buffer : Lwt_bytes.t -> Lwt_io.output_channel

module Parser : sig
  val parse_request_fst_line : Lwt_io.input_channel -> (Code.meth * Uri.t * Code.version) option Lwt.t
  val parse_response_fst_line : Lwt_io.input_channel -> (Code.version * Code.status_code) option Lwt.t
  val parse_headers : Lwt_io.input_channel -> Header.t Lwt.t
  val parse_content_range : Header.t -> int option
  val parse_media_type : string -> string option
end

module Body : sig
  val read : Transfer.encoding -> Lwt_io.input_channel -> string option Lwt.t
  val write : Transfer.encoding -> Lwt_io.output_channel -> string -> unit Lwt.t
end

module Request : sig
  type request
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

  val read : Lwt_io.input_channel -> request option Lwt.t
  val read_body : request -> Lwt_io.input_channel -> string option Lwt.t

  val write_header : request -> Lwt_io.output_channel -> unit Lwt.t
  val write_body : string -> request -> Lwt_io.output_channel -> unit Lwt.t
  val write_footer : request -> Lwt_io.output_channel -> unit Lwt.t
  val write : (request -> Lwt_io.output_channel -> unit Lwt.t) -> request -> Lwt_io.output_channel -> unit Lwt.t
end

module Response : sig
  type response
  val version : response -> Code.version
  val status : response -> Code.status_code

  val make : ?version:Code.version -> ?status:Code.status_code -> 
    ?encoding:Transfer.encoding -> Header.t -> response

  val read : Lwt_io.input_channel -> response option Lwt.t
  val read_body : response -> Lwt_io.input_channel -> string option Lwt.t

  val write_header : response -> Lwt_io.output_channel -> unit Lwt.t
  val write_body : string -> response -> Lwt_io.output_channel -> unit Lwt.t
  val write_footer : response -> Lwt_io.output_channel -> unit Lwt.t
  val write : (response -> Lwt_io.output_channel -> unit Lwt.t) -> response -> Lwt_io.output_channel -> unit Lwt.t
end

val call :
  ?headers:Header.t ->
  ?body:(Request.request -> Lwt_io.output_channel -> unit Lwt.t) ->
  Code.meth -> Uri.t -> unit Lwt.t

