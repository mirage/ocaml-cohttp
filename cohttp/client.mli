(*
 * Copyright (c) 2012-2013 Anil Madhavapeddy <anil@recoil.org>
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

module Make
  (IO:Make.IO)
  (Request:Make.REQUEST with type oc=IO.oc and type ic=IO.ic and type 'a io='a IO.t)
  (Response:Make.RESPONSE with type oc=Request.oc and type ic=Request.ic and type 'a io='a Request.io) 
  : sig

  type input_signal = [
    | `Failure
    | `Response of Response.t
    | `Body of string
    | `Body_end
  ]

  type signal_handler = (input_signal -> unit IO.t)

  val call :
    ?headers:Header.t ->
    ?chunked:bool -> ?body:(unit -> string option) ->
    Code.meth -> Uri.t -> signal_handler ->
    Response.ic -> Request.oc -> unit IO.t

  val head :
    ?headers:Header.t -> Uri.t -> signal_handler -> 
    Response.ic -> Request.oc -> unit IO.t

  val get :
    ?headers:Header.t -> Uri.t -> signal_handler ->
    Response.ic -> Request.oc -> unit IO.t

  val delete :
    ?headers:Header.t -> Uri.t -> signal_handler ->
    Response.ic -> Request.oc -> unit IO.t

  val post :
    ?body:(unit -> string option) -> ?chunked:bool ->
    ?headers:Header.t -> Uri.t -> signal_handler ->
    Response.ic -> Request.oc -> unit IO.t

  val put :
    ?body:(unit -> string option) -> ?chunked:bool ->
    ?headers:Header.t -> Uri.t -> signal_handler ->
    Response.ic -> Request.oc -> unit IO.t

  val patch :
    ?body:(unit -> string option) -> ?chunked:bool ->
    ?headers:Header.t -> Uri.t -> signal_handler ->
    Response.ic -> Request.oc -> unit IO.t

end
