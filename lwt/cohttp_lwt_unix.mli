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
    ?body:Cohttp_lwt_body.contents -> Uri.t -> t

  val is_form: t -> bool

  val read : Lwt_io.input_channel -> t option Lwt.t
  val read_form : t -> Lwt_io.input_channel -> (string * string) list Lwt.t
  val read_body : t -> Lwt_io.input_channel -> Cohttp.Transfer.chunk Lwt.t
  val write : (t -> Lwt_io.output_channel -> unit Lwt.t) -> t -> Lwt_io.output_channel -> unit Lwt.t
  val write_body : t -> Lwt_io.output_channel -> string -> unit Lwt.t
end

module Response : sig
  type t
  val version : t -> Code.version
  val status : t -> Code.status_code
  val headers: t -> Header.t

  val make : ?version:Code.version -> ?status:Code.status_code -> 
    ?encoding:Transfer.encoding -> ?headers:Header.t -> unit -> t

  val is_form: t -> bool

  val read : Lwt_io.input_channel -> t option Lwt.t
  val read_body : t -> Lwt_io.input_channel -> Cohttp.Transfer.chunk Lwt.t
  val write : (t -> Lwt_io.output_channel -> unit Lwt.t) -> t -> Lwt_io.output_channel -> unit Lwt.t
  val write_body : t -> Lwt_io.output_channel -> string -> unit Lwt.t
end

module Client : sig
  val call :
    ?headers:Cohttp.Header.t ->
    ?body:Cohttp_lwt_body.contents ->
    ?chunked:bool ->
    Cohttp.Code.meth ->
    Uri.t -> (Response.t * Cohttp_lwt_body.t) option Lwt.t

  val head :
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Response.t * Cohttp_lwt_body.t) option Lwt.t

  val get :
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Response.t * Cohttp_lwt_body.t) option Lwt.t

  val delete :
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Response.t * Cohttp_lwt_body.t) option Lwt.t

  val post :
    ?body:Cohttp_lwt_body.contents ->
    ?chunked:bool ->
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Response.t * Cohttp_lwt_body.t) option Lwt.t

  val put :
    ?body:Cohttp_lwt_body.contents ->
    ?chunked:bool ->
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Response.t * Cohttp_lwt_body.t) option Lwt.t

  val patch :
    ?body:Cohttp_lwt_body.contents ->
    ?chunked:bool ->
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Response.t * Cohttp_lwt_body.t) option Lwt.t

  val post_form :
    ?headers:Cohttp.Header.t ->
    params:Cohttp.Header.t ->
    Uri.t -> (Response.t * Cohttp_lwt_body.t) option Lwt.t

  val callv :
    ?ssl:bool ->
    string ->
    int ->
    (Request.t * Cohttp_lwt_body.contents option) Lwt_stream.t ->
    (Response.t * Cohttp_lwt_body.t) Lwt_stream.t Lwt.t
end

module Server : sig

    type conn_id = int
    val string_of_conn_id : int -> string

    type config = {
      callback : conn_id -> ?body:Cohttp_lwt_body.contents -> Request.t -> (Response.t * Cohttp_lwt_body.t) Lwt.t;
      conn_closed : conn_id -> unit -> unit;
    }

    val respond_string :
      ?headers:Cohttp.Header.t ->
      status:Cohttp.Code.status_code ->
      body:string -> unit -> (Response.t * Cohttp_lwt_body.t) Lwt.t

    val respond_error :
      status:Cohttp.Code.status_code ->
      body:string -> unit -> (Response.t * Cohttp_lwt_body.t) Lwt.t
    val callback : config -> Lwt_io.input_channel -> Lwt_io.output_channel -> unit Lwt.t

end

val server : ?timeout:int -> address:string -> port:int -> Server.config -> unit Lwt.t
