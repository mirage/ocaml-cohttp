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

(** HTTP Authentication header parsing and generation *)

(** HTTP authentication request types *)
type req = [
 | `Basic of string (* Basic authorization with a realm *)
] with sexp

(** HTTP authentication response types *)
type resp = [
  | `Basic of string * string (** Basic authorization with a username and password *)
  | `Other of string (* An unknown response header that will be passed straight through to the HTTP layer *)
] with sexp

(** [string_of_resp] converts the {!resp} to a string compatible
    with the HTTP/1.1 wire format for responses *)
val string_of_resp : resp -> string

(** [resp_of_string resp] converts a HTTP response to an authorization
    request into a {!resp}.  If the response is not recognized, [None]
    is returned. *)
val resp_of_string : string -> resp

(** [string_of_req req] converts the {!req} to a string compatible with
    the HTTP/1.1 wire format for authorization requests.

    For example, a {!Basic} request with realm ["foo"] will be 
    marshalled to ["Basic realm=foo"], which can then be combined
    with a [www-authenticate] HTTP header and sent back to the
    client.  There is a helper function {!Header.add_authorization_req}
    that does just this. *)
val string_of_req : req -> string
