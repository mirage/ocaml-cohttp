(*
 * Copyright (c) 2013 Anil Madhavapeddy <anil@recoil.org>
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

module type Client = sig
  module IO : IO.S

  val call :
    ?headers:Cohttp.Header.t ->
    ?body:Cohttp_lwt_body.contents ->
    ?chunked:bool ->
    Cohttp.Code.meth ->
    Uri.t -> (Cohttp.Response.t * Cohttp_lwt_body.t) option Lwt.t

  val head :
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Cohttp.Response.t * Cohttp_lwt_body.t) option Lwt.t

  val get :
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Cohttp.Response.t * Cohttp_lwt_body.t) option Lwt.t

  val delete :
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Cohttp.Response.t * Cohttp_lwt_body.t) option Lwt.t

  val post :
    ?body:Cohttp_lwt_body.contents ->
    ?chunked:bool ->
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Cohttp.Response.t * Cohttp_lwt_body.t) option Lwt.t

  val put :
    ?body:Cohttp_lwt_body.contents ->
    ?chunked:bool ->
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Cohttp.Response.t * Cohttp_lwt_body.t) option Lwt.t

  val patch :
    ?body:Cohttp_lwt_body.contents ->
    ?chunked:bool ->
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Cohttp.Response.t * Cohttp_lwt_body.t) option Lwt.t

  val post_form :
    ?headers:Cohttp.Header.t ->
    params:Cohttp.Header.t ->
    Uri.t -> (Cohttp.Response.t * Cohttp_lwt_body.t) option Lwt.t

  val callv :
    ?ssl:bool ->
    string ->
    int ->
    (Cohttp.Request.t * Cohttp_lwt_body.contents option) Lwt_stream.t ->
    (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt_stream.t Lwt.t
end

module Make_client
    (IO:Cohttp.IO.S with type 'a t = 'a Lwt.t)
    (ReqIO:Cohttp.Request.S with module IO = IO)
    (ResIO:Cohttp.Response.S with module IO = IO)
    (Net:Cohttp_lwt_net.S with type oc = ResIO.IO.oc and type ic = ResIO.IO.ic) : Client with module IO=IO


module type Server = sig
  module IO : IO.S

  type conn_id = int
  val string_of_conn_id : int -> string

  type config = {
    callback : conn_id -> ?body:Cohttp_lwt_body.contents -> 
      Cohttp.Request.t -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t;
    conn_closed : conn_id -> unit -> unit;
  }

  val respond :
    ?headers:Cohttp.Header.t ->
    status:Cohttp.Code.status_code ->
    body:Cohttp_lwt_body.t -> unit -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t

  val respond_string :
    ?headers:Cohttp.Header.t ->
    status:Cohttp.Code.status_code ->
    body:string -> unit -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t

  val respond_error :
    status:Cohttp.Code.status_code ->
    body:string -> unit -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t

  val respond_redirect :
    ?headers:Cohttp.Header.t ->
    uri:Uri.t -> unit -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t

  val respond_need_auth :
    ?headers:Cohttp.Header.t ->
    auth:Cohttp.Auth.req -> unit -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t

  val respond_not_found :
    ?uri:Uri.t -> unit -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t

  val callback : config -> IO.ic -> IO.oc -> unit Lwt.t
end

module Make_server
    (IO:Cohttp.IO.S with type 'a t = 'a Lwt.t)
    (ReqIO:Cohttp.Request.S with module IO=IO)
    (ResIO:Cohttp.Response.S with module IO=IO)
    (Net:Cohttp_lwt_net.S with type oc = ResIO.IO.oc and type ic = ResIO.IO.ic) : 
    Server with module IO=IO
