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

open Sexplib0.Sexp_conv

type encoding = Http.Transfer.encoding = Chunked | Fixed of int64 | Unknown
[@@deriving sexp]

let pp_encoding fmt = function
  | Chunked -> Format.pp_print_string fmt "chunked"
  | Fixed size -> Format.fprintf fmt "fixed %Ld" size
  | Unknown -> Format.pp_print_string fmt "unknown"

type chunk = Chunk of string | Final_chunk of string | Done [@@deriving sexp]

let string_of_encoding = function
  | Chunked -> "chunked"
  | Fixed i -> Printf.sprintf "fixed[%Ld]" i
  | Unknown -> "unknown"

let has_body = Http.Transfer.Private.has_body
