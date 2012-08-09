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
  type 'a t = 'a Lwt.t
  val (>>=) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
  val return : 'a -> 'a Lwt.t
  type ic = Lwt_io.input_channel
  type oc = Lwt_io.output_channel
  type buf = Lwt_bytes.t
  val iter : ('a -> unit Lwt.t) -> 'a list -> unit Lwt.t
  val read_line : Lwt_io.input_channel -> string option Lwt.t
  val read : Lwt_io.input_channel -> int -> string Lwt.t
  val read_exactly : Lwt_io.input_channel -> string -> int -> int -> bool Lwt.t
  val write : Lwt_io.output_channel -> string -> unit Lwt.t
  val write_line : Lwt_io.output_channel -> string -> unit Lwt.t
  val ic_of_buffer : Lwt_bytes.t -> Lwt_io.input Lwt_io.channel
  val oc_of_buffer : Lwt_bytes.t -> Lwt_io.output Lwt_io.channel
end

module Parser : sig
  val parse_request_fst_line : IO.ic -> (Code.meth * Uri.t * Code.version) option IO.t
  val parse_response_fst_line : IO.ic -> (Code.version * Code.status_code) option IO.t
  val parse_headers : IO.ic -> (string * string) list IO.t
  val parse_request : IO.ic -> (string * (string * string) list) option IO.t
  val parse_content_range : (string * string) list -> int option
  val parse_media_type : string -> string option
end

module Request : sig
  type request
  val parse : IO.ic -> request option IO.t
  val meth : request -> Code.meth
  val uri : request -> Uri.t
  val version : request -> Code.version
  val path : request -> string
  val header : name:string -> request -> string list
  val params_get : request -> (string * string) list
  val params_post : request -> (string * string) list
  val param : string -> request -> string option
  val body : request -> string option IO.t
  val transfer_encoding : request -> string
end
