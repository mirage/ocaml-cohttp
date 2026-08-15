(*{{{ Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
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
  }}}*)

type t =
  [ Cohttp.Body.t
  | `Stream of string Lwt_stream.t
  | `Bigstring of Cohttp.Bigstring.write ]
[@@deriving sexp]

include Cohttp.S.Body with type t := t

val is_empty : t -> bool Lwt.t
val to_string : t -> string Lwt.t

val to_bigstring : t -> Cohttp.Bigstring.t Lwt.t
(** The whole body as bytes outside the OCaml heap. A body made by
    {!of_bigstring} hands back the buffer it was given rather than a copy. *)

val to_string_list : t -> string list Lwt.t
val to_stream : t -> string Lwt_stream.t
val of_stream : string Lwt_stream.t -> t

val of_bigstring : Cohttp.Bigstring.write -> t
(** A body whose bytes are already off the heap, so sending it never builds a
    body-sized string.

    The tag says what the connection may do with the buffer, and there is no
    default because only the caller knows:

    - [`Copy] for a buffer that is about to be reused or refilled -- a scratch
      block a read loop writes into, say. The bytes are taken before the send
      completes.
    - [`Passthrough] for a buffer that will outlive the exchange untouched, such
      as a memory-mapped file. It spares the last copy on the backends that can
      write from foreign memory, and costs nothing on those that cannot.

    [`Passthrough] leaves the bytes borrowed until the message carrying them has
    been written, so touching them before the client call returns, or before the
    server has finished serving the response, corrupts what goes out. When in
    doubt [`Copy] is always safe. *)

val to_form : t -> (string * string list) list Lwt.t

val create_stream :
  ('a -> Cohttp.Transfer.chunk Lwt.t) -> 'a -> string Lwt_stream.t

val length : t -> (int64 * t) Lwt.t

val write_body :
  ?write_bigstring:(Cohttp.Bigstring.write -> int -> int -> unit Lwt.t) ->
  (string -> unit Lwt.t) ->
  t ->
  unit Lwt.t
(** Without [write_bigstring] a [`Bigstring] body is copied onto the heap to be
    written as a string. *)

val drain_body : t -> unit Lwt.t
