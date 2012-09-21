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

module Body : sig
  type contents
  type t = contents option

  val string_of_body : t -> string Lwt.t
  val stream_of_body : t -> string Lwt_stream.t
  val create_stream : ('a -> Transfer.chunk Lwt.t) -> 'a -> string Lwt_stream.t

  val body_of_string : string -> t
  val body_of_string_list : string list -> t
  val body_of_stream : string Lwt_stream.t -> t

  val get_length : t -> (int * t) Lwt.t
  val write_body : (string -> unit Lwt.t) -> t -> unit Lwt.t
end

module Request : sig
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
    ?body:Body.contents -> Uri.t -> t

  val is_form: t -> bool

  val read : Net.Channel.t -> t option Lwt.t
  val read_form : t -> Net.Channel.t -> (string * string list) list Lwt.t
  val read_body : t -> Net.Channel.t -> Cohttp.Transfer.chunk Lwt.t
  val write : (t -> Net.Channel.t -> unit Lwt.t) -> t -> Net.Channel.t -> unit Lwt.t
  val write_body : t -> Net.Channel.t -> string -> unit Lwt.t
end

module Response : sig
  type t
  val version : t -> Code.version
  val status : t -> Code.status_code
  val headers: t -> Header.t

  val make : ?version:Code.version -> ?status:Code.status_code -> 
    ?encoding:Transfer.encoding -> ?headers:Header.t -> unit -> t

  val is_form: t -> bool

  val read : Net.Channel.t -> t option Lwt.t
  val read_body : t -> Net.Channel.t -> Cohttp.Transfer.chunk Lwt.t
  val write : (t -> Net.Channel.t -> unit Lwt.t) -> t -> Net.Channel.t -> unit Lwt.t
  val write_body : t -> Net.Channel.t -> string -> unit Lwt.t
end

module Client : sig
  val call :
    ?headers:Cohttp.Header.t ->
    ?body:Body.contents ->
    ?chunked:bool ->
    Cohttp.Code.meth ->
    Uri.t -> (Response.t * Body.t) option Lwt.t

  val head :
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Response.t * Body.t) option Lwt.t

  val get :
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Response.t * Body.t) option Lwt.t

  val delete :
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Response.t * Body.t) option Lwt.t

  val post :
    ?body:Body.contents ->
    ?chunked:bool ->
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Response.t * Body.t) option Lwt.t

  val put :
    ?body:Body.contents ->
    ?chunked:bool ->
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Response.t * Body.t) option Lwt.t

  val patch :
    ?body:Body.contents ->
    ?chunked:bool ->
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Response.t * Body.t) option Lwt.t

  val post_form :
    ?headers:Cohttp.Header.t ->
    params:Cohttp.Header.t ->
    Uri.t -> (Response.t * Body.t) option Lwt.t

  val callv :
    ?ssl:bool ->
    string ->
    int ->
    (Request.t * Body.contents option) Lwt_stream.t ->
    (Response.t * Body.t) Lwt_stream.t Lwt.t
end

module Server : sig

    type conn_id = int
    val string_of_conn_id : int -> string

    type config = {
      callback : conn_id -> ?body:Body.contents -> Request.t -> (Response.t * Body.t) Lwt.t;
      conn_closed : conn_id -> unit -> unit;
    }

    val callback : config -> Net.Channel.t -> Net.Channel.t -> unit Lwt.t

    val respond_string :
      ?headers:Cohttp.Header.t ->
      status:Cohttp.Code.status_code ->
      body:string -> unit -> (Response.t * Body.t) Lwt.t

    val respond_error :
      status:Cohttp.Code.status_code ->
      body:string -> unit -> (Response.t * Body.t) Lwt.t

   val respond_not_found :
      ?uri:Uri.t -> unit -> (Response.t * Body.t) Lwt.t
end

val listen :
  ?timeout:float ->
  Net.Manager.t -> Net.Nettypes.ipv4_src -> Server.config -> unit Lwt.t
