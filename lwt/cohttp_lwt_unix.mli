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

open Cohttp

module Request : sig
  type t
  val meth : t -> Code.meth
  val uri : t -> Uri.t
  val version : t -> Code.version
  val path : t -> string
  val header : t -> string -> string option
  val headers : t -> Header.t
  val params : t -> (string * string) list
  val transfer_encoding : t -> string

  val make : ?meth:Code.meth -> ?version:Code.version -> 
    ?encoding:Transfer.encoding -> ?headers:Header.t ->
    ?body:'a -> Uri.t -> t

  val read : Lwt_io.input_channel -> t option Lwt.t
  val has_body : t -> bool
  val read_body : t -> Lwt_io.input_channel -> Transfer.chunk Lwt.t

  val write_header : t -> Lwt_io.output_channel -> unit Lwt.t
  val write_body : t -> Lwt_io.output_channel -> string -> unit Lwt.t
  val write_footer : t -> Lwt_io.output_channel -> unit Lwt.t
  val write : (t -> Lwt_io.output_channel -> unit Lwt.t) -> t -> 
    Lwt_io.output_channel -> unit Lwt.t

  val is_form: t -> bool
  val read_form : t -> Lwt_io.input_channel -> (string * string) list Lwt.t
end

module Response : sig
  type t
  val version : t -> Code.version
  val status : t -> Code.status_code
  val headers: t -> Header.t

  val make : ?version:Code.version -> ?status:Code.status_code -> 
    ?encoding:Transfer.encoding -> ?headers:Header.t -> unit -> t

  val read : Lwt_io.input_channel -> t option Lwt.t
  val has_body : t -> bool
  val read_body : t -> Lwt_io.input_channel -> Transfer.chunk Lwt.t

  val write_header : t -> Lwt_io.output_channel -> unit Lwt.t
  val write_body : t -> Lwt_io.output_channel -> string -> unit Lwt.t
  val write_footer : t -> Lwt_io.output_channel -> unit Lwt.t
  val write : (t -> Lwt_io.output_channel -> unit Lwt.t) -> 
    t -> Lwt_io.output_channel -> unit Lwt.t

  val is_form: t -> bool
  val read_form : t -> Lwt_io.input_channel -> (string * string) list Lwt.t
end
