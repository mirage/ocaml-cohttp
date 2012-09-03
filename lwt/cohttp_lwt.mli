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
  val body_of_string : string -> t
  val body_of_string_list : string list -> t
  val body_of_stream : string Lwt_stream.t -> t
  val get_length : t -> (int * t) Lwt.t
end

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
    ?body:string Lwt_stream.t -> Uri.t -> t

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

  val is_form: t -> bool
  val read_form : t -> Lwt_io.input_channel -> (string * string) list Lwt.t
end

module Client : sig

  val call : 
    ?headers:Header.t -> 
    ?body:Body.contents ->
    ?chunked:bool -> 
    Code.meth -> Uri.t -> (Response.t * Body.t) option Lwt.t

  val head : ?headers:Header.t -> Uri.t -> (Response.t * Body.t) option Lwt.t

  val get : ?headers:Header.t -> Uri.t -> (Response.t * Body.t) option Lwt.t

  val post : 
    ?body:Body.contents -> 
    ?chunked:bool -> 
    ?headers:Header.t -> 
    Uri.t -> (Response.t * Body.t) option Lwt.t

  val post_form : 
    ?headers:Header.t -> 
    params:Header.t -> 
    Uri.t -> (Response.t * Body.t) option Lwt.t

  val put : 
    ?body:Body.contents -> 
    ?chunked:bool -> 
    ?headers:Header.t -> 
    Uri.t -> (Response.t * Body.t) option Lwt.t

  val patch : 
    ?body:Body.contents -> 
    ?chunked:bool -> 
    ?headers:Header.t -> 
    Uri.t -> (Response.t * Body.t) option Lwt.t

  val delete : 
    ?headers:Header.t -> 
    Uri.t -> (Response.t * Body.t) option Lwt.t

  val callv : ?ssl:bool -> string -> int ->
      (Request.t * Body.t) Lwt_stream.t ->
      (Response.t * Body.t) Lwt_stream.t Lwt.t
end

module Server : sig
  type conn_id

  val string_of_conn_id : conn_id -> string

  type config = {
    address : string;
    callback : conn_id -> ?body:Body.contents -> Request.t -> (Response.t * Body.t) Lwt.t;
    conn_closed : conn_id -> unit -> unit; 
    port : int;
    timeout : int option;
  }  

  val respond_string : ?headers:Header.t -> status:Code.status_code ->
    body:string -> unit -> (Response.t * Body.t) Lwt.t

  val respond_error : status:Code.status_code -> body:string -> unit -> (Response.t * Body.t) Lwt.t

  val main : config -> unit Lwt.t
end
