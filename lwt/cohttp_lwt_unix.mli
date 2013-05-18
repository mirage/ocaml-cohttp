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

module Request : sig
  include module type of Cohttp.Request with type t = Cohttp.Request.t
  include Cohttp.Request.S with module IO=Cohttp_lwt_unix_io
end

module Response : sig
  include module type of Cohttp.Response with type t = Cohttp.Response.t
  include Cohttp.Response.S with module IO=Cohttp_lwt_unix_io
end

module Client : Cohttp_lwt.Client with module IO=Cohttp_lwt_unix_io
module Server : Cohttp_lwt.Server with module IO=Cohttp_lwt_unix_io

(*
    val resolve_file : docroot:string -> uri:Uri.t -> string

    val respond_file :
      ?headers:Cohttp.Header.t ->
      fname:string -> unit -> (Cohttp.Response.t * Body.t) Lwt.t

    val callback : 
      config -> Lwt_io.input_channel -> Lwt_io.output_channel -> unit Lwt.t
*)
val server : ?timeout:int -> address:string -> port:int -> Server.config -> unit Lwt.t
