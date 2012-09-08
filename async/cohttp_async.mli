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

open Async.Std
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

  val is_form: t -> bool
  val read_form : t -> Reader.t -> (string * string) list Deferred.t
end

module Response : sig
  type t
  val version : t -> Code.version
  val status : t -> Code.status_code
  val headers : t -> Header.t
  val make : ?version:Code.version -> ?status:Code.status_code ->
    ?encoding:Transfer.encoding -> ?headers:Header.t -> unit -> t

  val is_form: t -> bool
  val read_form : t -> Reader.t -> (string * string) list Deferred.t
end

module Client : sig
  val call : ?headers:Header.t -> ?body:string Pipe.Reader.t ->
    Code.meth -> Uri.t -> (Response.t * string Pipe.Reader.t option) option Deferred.t
end

module Server : sig
  type conn_id = int
  
  type config = {
    callback: conn_id -> ?body:string Pipe.Reader.t -> Request.t -> (Response.t * string Pipe.Reader.t option) Deferred.t;
    port: int;
  }

  val respond_string : ?headers:Header.t -> status:Code.status_code -> body:string -> unit -> (Response.t * string Pipe.Reader.t option) Deferred.t

  val main : config -> (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t
end
