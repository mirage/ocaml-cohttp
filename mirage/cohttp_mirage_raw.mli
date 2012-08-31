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
  type ic = Net.Channel.TCPv4.t
  type oc = Net.Channel.TCPv4.t
end

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
  val make :
    ?meth:Code.meth ->
    ?version:Code.version ->
    ?encoding:Transfer.encoding -> ?headers:Header.t -> Uri.t -> request
  val read : IO.ic -> request option Lwt.t
  val has_body : request -> bool
  val read_body : request -> IO.ic -> Transfer.chunk Lwt.t
  val write_header : request -> IO.oc -> unit Lwt.t
  val write_body : request -> IO.oc -> string -> unit Lwt.t
  val write_footer : request -> IO.oc -> unit Lwt.t
  val write :
    (request -> IO.oc -> unit Lwt.t) -> request -> IO.oc -> unit Lwt.t
  val is_form : request -> bool
  val read_form : request -> IO.ic -> (string * string) list Lwt.t
end

module Response : sig
  type response
  val version : response -> Code.version
  val status : response -> Code.status_code
  val headers : response -> Header.t
  val make :
    ?version:Code.version ->
    ?status:Code.status_code ->
    ?encoding:Transfer.encoding -> ?headers:Header.t -> unit -> response
  val read : IO.ic -> response option Lwt.t
  val has_body : response -> bool
  val read_body : response -> IO.ic -> Transfer.chunk Lwt.t
  val read_body_to_string : response -> IO.ic -> string Lwt.t
  val write_header : response -> IO.oc -> unit Lwt.t
  val write_body : response -> IO.oc -> string -> unit Lwt.t
  val write_footer : response -> IO.oc -> unit Lwt.t
  val write : (response -> IO.oc -> unit Lwt.t) -> response -> IO.oc -> unit Lwt.t
  val is_form : response -> bool
  val read_form : response -> IO.ic -> (string * string) list Lwt.t
end
