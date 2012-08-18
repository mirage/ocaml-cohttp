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
  val params : request -> (string * string) list
  val transfer_encoding : request -> string

  val make : ?meth:Code.meth -> ?version:Code.version -> 
    ?encoding:Transfer.encoding -> ?headers:Header.t -> Uri.t -> request

  val read : Lwt_io.input_channel -> request option Lwt.t
  val has_body : request -> bool
  val read_body : request -> Lwt_io.input_channel -> Transfer.chunk Lwt.t

  val write_header : request -> Lwt_io.output_channel -> unit Lwt.t
  val write_body : string -> request -> Lwt_io.output_channel -> unit Lwt.t
  val write_footer : request -> Lwt_io.output_channel -> unit Lwt.t
  val write : (request -> Lwt_io.output_channel -> unit Lwt.t) -> request -> 
    Lwt_io.output_channel -> unit Lwt.t

  val is_form: request -> bool
  val read_form : request -> Lwt_io.input_channel -> (string * string) list Lwt.t
end

module Response : sig
  type response
  val version : response -> Code.version
  val status : response -> Code.status_code
  val headers: response -> Header.t

  val make : ?version:Code.version -> ?status:Code.status_code -> 
    ?encoding:Transfer.encoding -> ?headers:Header.t -> unit -> response

  val read : Lwt_io.input_channel -> response option Lwt.t
  val has_body : response -> bool
  val read_body : response -> Lwt_io.input_channel -> Transfer.chunk Lwt.t

  val write_header : response -> Lwt_io.output_channel -> unit Lwt.t
  val write_body : string -> response -> Lwt_io.output_channel -> unit Lwt.t
  val write_footer : response -> Lwt_io.output_channel -> unit Lwt.t
  val write : (response -> Lwt_io.output_channel -> unit Lwt.t) -> 
    response -> Lwt_io.output_channel -> unit Lwt.t

  val is_form: response -> bool
  val read_form : response -> Lwt_io.input_channel -> (string * string) list Lwt.t
end
