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

module Body : module type of Cohttp_lwt_body

module Request : Cohttp.Request.S 
  with module IO = Cohttp_lwt_mirage_io

module Response : Cohttp.Response.S 
  with module IO = Cohttp_lwt_mirage_io

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
