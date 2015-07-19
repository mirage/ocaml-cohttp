(*{{{ Copyright (C) 2012, David Sheets <sheets@alum.mit.edu>

  Permission to use, copy, modify, and/or distribute this software for
  any purpose with or without fee is hereby granted, provided that the
  above copyright notice and this permission notice appear in all
  copies.

  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
  WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
  AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
  DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
  OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
  TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
  PERFORMANCE OF THIS SOFTWARE.
  }}}*)

(** Type definitions for the {!Accept} module *)

open Sexplib.Std

type pv = T of string | S of string with sexp
type p = string * pv with sexp
type media_range =
  | MediaType of string * string
  | AnyMediaSubtype of string
  | AnyMedia with sexp
type charset =
  | Charset of string
  | AnyCharset with sexp
type encoding =
  | Encoding of string
  | Gzip
  | Compress
  | Deflate
  | Identity
  | AnyEncoding with sexp
type language =
  | Language of string list
  | AnyLanguage with sexp
type q = int with sexp
type 'a qlist = (q * 'a) list with sexp
